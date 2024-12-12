! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

program main
  implicit none

  integer, parameter :: DIM1 = 17
  integer, parameter :: DIM2 = 27
  type :: ptr_t
    integer, pointer :: ptr(:)
  end type
  
  type (ptr_t) :: x(DIM1), y(DIM2)

  !$omp target map(iterator(i=1:DIM1), to: x(i)%ptr(:)) map(iterator(i=1:DIM2), from: y(i)%ptr(:))
  !$omp end target
end program

! { dg-final { scan-tree-dump-times "if \\(i <= 17\\) goto <D\\\.\[0-9\]+>; else goto <D\\\.\[0-9\]+>;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "if \\(i <= 27\\) goto <D\\\.\[0-9\]+>; else goto <D\\\.\[0-9\]+>;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(iterator\\(integer\\(kind=4\\) i=1:17:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=17\\):to:MEM <\[^>\]+> \\\[\\\(\[^ \]+ \\\*\\\)D\\\.\[0-9\]+\\\]" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(iterator\\(integer\\(kind=4\\) i=1:27:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=27\\):from:MEM <\[^>\]+> \\\[\\\(\[^ \]+ \\\*\\\)D\\\.\[0-9\]+\\\]" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(iterator\\(integer\\(kind=4\\) i=1:17:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=17\\):attach:x\\\[D\\\.\[0-9\]+\\\]\.ptr\.data" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(iterator\\(integer\\(kind=4\\) i=1:27:1, loop_label=<D\\\.\[0-9\]+>, index=D\\\.\[0-9\]+, elems=omp_iter_data\\\.\[0-9\]+, elems_count=27\\):attach:y\\\[D\\\.\[0-9\]+\\\]\.ptr\.data" 1 "gimple" } }
