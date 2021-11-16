! { dg-do compile }
!
! Checking for removal of SHORT and LONG intrinsics.
!
  real,parameter :: a=3.1415927
  integer :: i

  i=SHORT(a) ! { dg-error "has been removed" }
  i=LONG(a)  ! { dg-error "has been removed" }

  end
