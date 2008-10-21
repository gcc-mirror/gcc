! { dg-do run }
! Check the fix for PR36795, where the parentheses in the call to foo were
! simplified out ie. foo((xx), xx) simplified to foo (xx, xx)
!
! Conributed by Vivek Rao <vivekrao4@yahoo.com>
!
program main
  implicit none
  character(len=10), allocatable :: xx(:)
  character(len=10)              :: yy
  allocate (xx(2))
  xx(1)      = ""
  xx(2)      = "dog"
  call foo ((xx),xx)
  if (trim (xx(1)) .ne. "dog") call abort
  if (size (xx, 1) .ne. 1) call abort
contains
  subroutine foo (xx,yy)
  character(len=*), intent(in)               :: xx(:)
  character(len=*), intent(out), allocatable :: yy(:)
  if (allocated (yy)) deallocate (yy)
  allocate (yy(1))
  yy = xx(2)
  end subroutine foo
end program main

