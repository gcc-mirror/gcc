! { dg-do run }
! PR13415
! Test pointer variables in common blocks.

subroutine test
  implicit none
  real, pointer :: p(:), q
  common /block/ p, q

  if (any (p .ne. (/1.0, 2.0/)) .or. (q .ne. 42.0)) call abort ()
end subroutine

program common_pointer_1
  implicit none
  real, target :: a(2), b
  real, pointer :: x(:), y
  common /block/ x, y
  
  a = (/1.0, 2.0/)
  b = 42.0
  x=>a
  y=>b
  call test
end program
