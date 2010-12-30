! { dg-do run }
!
! PR fortran/46978
! The coor assignment was using the wrong loop bounds if the argument to
! transpose was an intrinsic function call
!
! Original testcase by Martien Huelsen <m.a.hulsen@tue.nl>
! Reduced by Tobias Burnus <burnus@net-b.de>

program elastic2
  implicit none
  real, allocatable, dimension(:,:) :: coor
  real, allocatable, dimension(:) :: a
  integer :: nno
  nno = 3
  allocate(a(2*nno))
  call two()
  coor = transpose ( reshape ( a, (/2,nno/) ) )
  if (any(coor /= 12)) call abort
contains
  subroutine two()
    allocate(coor(3,2))
    coor = 99
    a = 12
  end subroutine
end program elastic2
