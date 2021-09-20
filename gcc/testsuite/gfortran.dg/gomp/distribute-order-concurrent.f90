! { dg-additional-options "-fdump-tree-original" }
!
! In OpenMP 5.0, 'order(concurrent)' does not apply to distribute
! Ensure that it is rejected in GCC 11.
! 
! Note: OpenMP 5.1 allows it; the GCC 12 testcase for it is gfortran.dg/gomp/order-5.f90

subroutine f(a)
implicit none
integer :: i, thr
!save :: thr
integer :: a(:)

!$omp distribute parallel do order(concurrent) private(thr)
  do i = 1, 10
    thr = 5
    a(i) = thr
  end do
!$omp end distribute parallel do
end

! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*order" "original" } }
! { dg-final { scan-tree-dump "#pragma omp distribute\[\n\r\]" "original" } }
! { dg-final { scan-tree-dump "#pragma omp parallel private\\(thr\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp for nowait order\\(concurrent\\)" "original" } }
