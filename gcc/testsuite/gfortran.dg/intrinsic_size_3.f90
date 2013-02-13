! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/55852
!
! Contributed by A. Kasahara
!
program bug
  implicit none

  Real, allocatable:: a(:)
  integer(2) :: iszs

  allocate(a(1:3))

  iszs = ubound((a), 1)! Was ICEing
!  print*, ubound((a), 1) ! Was ICEing
! print*, ubound(a, 1)   ! OK
! print*, lbound((a), 1) ! OK
! print*, lbound(a, 1)   ! OK

  stop
end program bug

! { dg-final { scan-tree-dump-times "iszs = \\(integer\\(kind=2\\)\\) MAX_EXPR <\\(D.\[0-9\]+->dim.0..ubound - D.\[0-9\]+->dim.0..lbound\\) \\+ 1, 0>;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
