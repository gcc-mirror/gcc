! { dg-do run }
! { dg-options "-std=f2023" }
program conditional_constant
  implicit none
  integer :: i = 42

  print *, (.true. ? 1 : -1)
  print *, (.false. ? "hello" : "world")
  i = (.true. ? 1 : -1)
  if (i /= 1) stop 1

  i = 0
  i = (i > 0 ? 1 : .false. ? -1 : 0)
  if (i /= 0) stop 2
end program conditional_constant
