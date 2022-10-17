! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }
!
! PR fortran/106566
!
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare simd \\(linear\\(0:ref,step\\(4\\)\\) simdlen\\(8\\)\\)\\)\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare simd \\(linear\\(0:ref,step\\(8\\)\\) simdlen\\(8\\)\\)\\)\\)" 2 "gimple" } }

subroutine add_one2(p)
  implicit none
  !$omp declare simd(add_one2) linear(p: ref) simdlen(8)
  integer(kind=4) :: p

  p = p + 1
end subroutine

subroutine linear_add_one2(p)
  implicit none
  !$omp declare simd(linear_add_one2) linear(p: ref, step(2)) simdlen(8)
  integer(kind=4) :: p

  p = p + 1
end subroutine

module m
   integer, parameter :: NN = 1023
   integer(kind=4) :: a(NN)
contains
  subroutine module_add_one2(q)
    implicit none
    !$omp declare simd(module_add_one2) linear(q: ref) simdlen(8)
    integer(kind=4) :: q
    q = q + 1
  end subroutine

  subroutine linear_add_one2(q)
    implicit none
    !$omp declare simd(linear_add_one2) linear(q: ref, step(2)) simdlen(8)
    integer(kind=4) :: q
    q = q + 1
  end subroutine
end module
