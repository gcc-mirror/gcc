! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/108131
!
! Incorrect array bounds when bound intrinsic used in declaration

program test
  implicit none
  integer, parameter :: mg(7:10)                 = 0
  integer, parameter :: u =   ubound(mg, dim=1)
  integer, parameter :: cx(-1:ubound(mg, dim=1)) = 1
  integer, parameter :: dx(lbound(mg, dim=1):ubound(cx, dim=1)) = 2

  write(*,*) ubound(mg, dim=1)
  write(*,*) ubound(cx, dim=1)
  if (u /= 10) stop 1
  if (ubound(mg, dim=1) /= 10) stop 2
  if (ubound(cx, dim=1) /= 10) stop 3
  if (ubound(dx, dim=1) /= 10) stop 4
  if (lbound(mg, dim=1) /=  7) stop 5
  if (lbound(cx, dim=1) /= -1) stop 6
  if (lbound(dx, dim=1) /=  7) stop 7
end program test

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
