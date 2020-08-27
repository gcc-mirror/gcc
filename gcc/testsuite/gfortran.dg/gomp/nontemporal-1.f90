! { dg-do compile }
! { dg-additional-options "-O2 -fdump-tree-original" }

module m
  integer :: a(:), b(1024), c(1024), d(1024)
  allocatable :: a
end module m

subroutine foo
  use m
  implicit none
  integer :: i
  !$omp simd nontemporal (a, b)
  do i = 1, 1024
    a(i) = b(i) + c(i)
  end do

  !$omp simd nontemporal (d)
  do i = 1, 1024
    d(i) = 2 * c(i)
  end do
end subroutine foo

! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) nontemporal\\(a\\) nontemporal\\(b\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) nontemporal\\(d\\)" 1 "original" } }
