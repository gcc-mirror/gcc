! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

module m
  integer a, b
end module m

subroutine f1 (c, d)
  use m
  implicit none
  integer c(*), d(*), i
  !$omp do simd reduction (inscan, +: a)
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a)
    a = a + c(i)
  end do
end

! { dg-final { scan-tree-dump-times "#pragma omp for reduction\\(inscan,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) reduction\\(inscan,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp scan exclusive\\(a\\)" 1 "original" } }
