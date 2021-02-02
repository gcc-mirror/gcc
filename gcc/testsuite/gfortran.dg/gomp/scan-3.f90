! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

module m
  integer :: a, b
end module m

subroutine f1 (c, d)
  use m
  implicit none
  integer i, c(*), d(*)
  !$omp do reduction (inscan, +: a)
  do i = 1, 64
    d(i) = a
    !$omp scan inclusive (a)
    a = a + c(i)
  end do
end

! { dg-final { scan-tree-dump-times "#pragma omp for reduction\\(inscan,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp scan inclusive\\(a\\)" 1 "original" } }
