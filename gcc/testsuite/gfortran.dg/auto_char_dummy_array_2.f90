! { dg-do compile }
! Test fix for pr24789 - would segfault on the assignment
! because the array descriptor size was not set.
!
! This is the example submitted by Martin Reineke  <martin@mpa-garching.mpg.de>

subroutine foo(vals)
  character(len = *), pointer :: vals(:)
  vals = ''
end subroutine

