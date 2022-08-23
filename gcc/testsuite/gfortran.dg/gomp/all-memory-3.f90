module m
  use iso_c_binding
  implicit none
  integer, parameter :: omp_depend_kind = 2*c_size_t

  integer(omp_depend_kind) :: z
contains

subroutine foo
  integer :: x, y
  x = 0; y = 0
  !$omp task depend(out: omp_all_memory)
    block; end block
  !$omp task depend(inout: omp_all_memory)
    block; end block
  !$omp task depend(out: x, omp_all_memory, y)
    block; end block
  !$omp task depend(inout: omp_all_memory, y)
    block; end block
  !$omp task depend(out: x, omp_all_memory)
    block; end block
  !$omp depobj (z) depend (inout: omp_all_memory)
end
end
