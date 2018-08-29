! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/54208
! The I and J definitions used to raise an error because ARR's array spec
! was resolved to late for the LBOUND and UBOUND calls to be simplified to
! a constant.
!
! Contributed by Carlos A. Cruz <carlos.a.cruz@nasa.gov>

program testit
  integer, parameter :: n=2
  integer, dimension(1-min(n,2)/2:n) :: arr
  integer, parameter :: i=lbound(arr,1)
  integer, parameter :: j=ubound(arr,1)
  ! write(6,*) i, j
  if (i /= 0) STOP 1
  if (j /= 2) STOP 2
end program testit

! { dg-final { scan-tree-dump-times "bound" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stop" 0 "original" } }
