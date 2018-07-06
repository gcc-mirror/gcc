! { dg-do run }
! Check size with initialization expression value for dim=
! PR fortran/30882
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
program main
  integer :: a(10)
  call S1(a)
contains
  subroutine S1(a)
    integer :: a(*)
    if(size(a(1:10),1) /= 10) STOP 1
  end subroutine S1
end program main
