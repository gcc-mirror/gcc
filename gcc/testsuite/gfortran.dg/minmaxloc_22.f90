! { dg-do compile }
! { dg-additional-options "-O" }
!
! Check that the inline code generated for MINLOC and MAXLOC supports
! a non-constant DIM argument if ARRAY has rank 1.

program p
  implicit none
  integer, parameter :: n = 5
  integer :: a(n), i
  a = (/ (i**2, i=1,n) /)
  print *, f(a, 1), g(a, 1)
contains
  function f(a, d)
    integer :: a(n)
    integer :: d
    integer :: f
    f = minloc(a, dim=d) 
  end function
  function g(a, d)
    integer :: a(n)
    integer :: d
    integer :: g
    g = maxloc(a, dim=d) 
  end function
end program p
