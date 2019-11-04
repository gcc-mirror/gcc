! { dg-do compile } 

program test
  implicit none

  integer :: i
  real :: x

  !$acc parallel &
  !$acc loop & ! continuation
  !$acc & reduction(+:x)

  ! this line must be ignored
  !$acc ! kernels
  do i = 1,10
    x = x + 0.3
  enddo
  ! continuation must begin with sentinel
  !$acc end parallel & ! { dg-error "Unexpected junk" }
  ! loop

  print *, x
end
