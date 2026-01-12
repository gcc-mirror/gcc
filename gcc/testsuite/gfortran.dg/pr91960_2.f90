! { dg-do compile }
module m
  implicit none
  integer :: i, j
  integer, parameter :: a(3) = [1, 2, 3]
  integer, parameter :: c    = a(j)
  integer            :: d    = c       ! { dg-error "initialization expression at" }
end
