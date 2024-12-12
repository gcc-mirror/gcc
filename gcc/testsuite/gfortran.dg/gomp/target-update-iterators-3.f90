! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

program test
  implicit none

  integer, parameter :: DIM1 = 17
  integer, parameter :: DIM2 = 39

  type :: array_ptr
    integer, pointer :: ptr(:)
  end type

  type (array_ptr) :: x(DIM1, DIM2), y(DIM1, DIM2), z(DIM1)

  !$omp target update to (iterator(i=1:DIM1, j=1:DIM2): x(i, j)%ptr(:), y(i, j)%ptr(:))
  !$omp target update from (iterator(i=1:DIM1): z(i)%ptr(:))
end program

! { dg-final { scan-tree-dump-times "if \\(i <= 17\\) goto <D\\\.\[0-9\]+>; else goto <D\\\.\[0-9\]+>;" 2 "gimple" } }
! { dg-final { scan-tree-dump "if \\(j <= 39\\) goto <D\\\.\[0-9\]+>; else goto <D\\\.\[0-9\]+>;" "gimple" } }
! { dg-final { scan-tree-dump-times "to\\(iterator\\(integer\\(kind=4\\) j=1:39:1, integer\\(kind=4\\) i=1:17:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=663\\):MEM <\[^>\]+> \\\[\\\(\[^ \]+ \\\*\\\)D\\\.\[0-9\]+\\\]" 2 "gimple" } }
! { dg-final { scan-tree-dump "from\\(iterator\\(integer\\(kind=4\\) i=1:17:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=17\\):MEM <\[^>\]+> \\\[\\\(\[^ \]+ \\\*\\\)D\\\.\[0-9\]+\\\]" "gimple" } }
