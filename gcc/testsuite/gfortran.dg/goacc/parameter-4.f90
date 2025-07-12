! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine x
  integer :: var
  integer, parameter :: ilog = 0
  integer, parameter :: array(*) = [11,22,33]
  !$ACC DECLARE COPYIN(ilog, array, var)
end subroutine x

integer :: a
integer, parameter :: b = 4
integer, parameter :: c(*) = [1,2,3]

!$acc parallel copy(a,c,b)
  a = c(2) + b
!$acc end parallel

!$acc parallel firstprivate(a,c,b)
  a = c(2) + b
!$acc end parallel
end

! { dg-final { scan-tree-dump-times "#pragma acc data map\\(to:var\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc parallel map\\(tofrom:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc parallel firstprivate\\(a\\)" 1 "original" } }
