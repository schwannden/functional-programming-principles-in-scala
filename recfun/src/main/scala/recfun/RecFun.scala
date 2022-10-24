package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def _balance(chars: List[Char], acc: Int = 0): Boolean =
      if (acc < 0)
        false
      else
        chars match
          case Nil => acc == 0
          case head :: tail =>
            if (head == '(')
              _balance(tail, acc + 1)
            else if (head == ')')
              _balance(tail, acc - 1)
            else
              _balance(tail, acc)
    _balance(chars)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    val sorted_coins = coins.sorted
    def _countChange(money: Int, coins: List[Int]): Int =
      if (money > 0)
        coins match
          case Nil => 0
          case head :: tail =>
            _countChange(money - head, coins) + _countChange(money, tail)
      else
        if money == 0 then 1 else 0
    _countChange(money, sorted_coins)

