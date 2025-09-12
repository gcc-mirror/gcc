! { dg-do compile }
! { dg-options "-std=f2023" }
module m
  contains
    function f(n) result(str)
      integer, value :: n
      character(len=(n > 5 ? n : 5)) :: str
      str = ""
      str(1:5) = "abcde"
    end
end
