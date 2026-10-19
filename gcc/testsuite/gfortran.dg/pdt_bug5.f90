! { dg-do run }
! Heap corruption: parameterized array component with a non-default
! lower bound was freed incorrectly at scope exit.
! See attachment to PR82173 comment 10 - reproducers/bug5-lower-bound-double-free.f90
program p
  type :: t(n)
    integer, len :: n = 1
    integer :: a(0:n)
    integer :: b(-n:0)
  end type
  type(t(3)) :: p1
  p1%a = 1
  p1%b = 2
  if (sum (p1%a) /= 4) stop 1   ! correct value, then "double free or corruption (out)"
  if (sum (p1%b) /= 8) stop 2   ! ditto
end program
