! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-omplower" }

module m
  integer, parameter :: DIM1 = 31
  integer, parameter :: DIM2 = 17
  type :: array_ptr
    integer, pointer :: ptr(:)
 end type
contains
  subroutine f (x, stride)
    type (array_ptr) :: x(:)
    integer :: stride

    !$omp target map(to: x) map(iterator(i=lbound(x, 1):ubound(x, 1):stride), to: x(i)%ptr(:))
    !$omp end target
  end subroutine
end module

! { dg-final { scan-tree-dump-times "D\\\.\[0-9\]+ = __builtin_malloc \\(D\\\.\[0-9\]+\\);" 3 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(omp_iter_data\\\.\[0-9\]+\\);" 3 "omplower" } }
