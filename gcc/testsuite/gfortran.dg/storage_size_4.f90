! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/57553
!
! Ensure that there is no ICE and that compile-time simplication works.
!
  use iso_fortran_env
  implicit none
  integer, parameter :: ESize = storage_size('a')
  integer, parameter :: ESize2 = storage_size('aa')
  if ( ESize/CHARACTER_STORAGE_SIZE /= 1) STOP 1
  if ( ESize2/CHARACTER_STORAGE_SIZE /= 2) STOP 2
end

subroutine S ( A )
  character(len=*), intent(in) :: A
  integer :: ESize = 4
  esize = ( storage_size(a) + 7 ) / 8
end

! { dg-final { scan-tree-dump-not "_gfortran_stop" "original" } }
