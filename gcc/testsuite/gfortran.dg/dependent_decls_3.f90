! { dg-do run }
!
! Fix a regression caused by the fix for PR59104.
!
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
program p
  implicit none
  integer, parameter :: nx = 64, ny = 32
  real               :: x(nx,ny), s(nx/2,ny), d(nx/2,ny)

  s = 0.0
  d = 0.0
  call sub (x,s,d)
  if (sum(s) .ne. 256) stop 1
  if (sum(d) .ne. 256) stop 2  ! Stopped with sum(d) == 0.
contains
  subroutine sub  (v, w, d)
    real, intent(in)  :: v(:,:)
    real, intent(out), dimension (size (v,dim=1)/4,size (v,dim=2)/2) :: w, d
    w = 1.0
    d = 1.0
    if (any (shape (w) .ne. [nx/4, ny/2])) stop 3
    if (any (shape (d) .ne. [nx/4, ny/2])) print *, shape (d)  ! Printed "0 0" here
  end subroutine sub
end
