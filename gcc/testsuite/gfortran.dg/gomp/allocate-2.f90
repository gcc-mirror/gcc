! { dg-do compile }

module omp_lib_kinds
  use iso_c_binding, only: c_int, c_intptr_t
  implicit none
  private :: c_int, c_intptr_t
  integer, parameter :: omp_allocator_handle_kind = c_intptr_t

end module

subroutine foo(x)
  use omp_lib_kinds
  implicit none
  integer  :: x

  !$omp task allocate (x) ! { dg-error "'x' specified in 'allocate' clause at .1. but not in an explicit privatization clause" }
  x=1
  !$omp end task

  !$omp parallel allocate (x) ! { dg-error "'x' specified in 'allocate' clause at .1. but not in an explicit privatization clause" }
  x=2
  !$omp end parallel

  !$omp parallel allocate (x) shared (x) ! { dg-error "'x' specified in 'allocate' clause at .1. but not in an explicit privatization clause" }
  x=3
  !$omp end parallel

  !$omp parallel private (x) allocate (x) allocate (x) ! { dg-warning "'x' appears more than once in 'allocate' clauses at .1." }
  x=4
  !$omp end parallel

  !$omp parallel private (x) allocate (x, x) ! { dg-warning "'x' appears more than once in 'allocate' clauses at .1." } 
  x=5
  !$omp end parallel

  !$omp parallel allocate (0_1: x) private(x) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind at .1." }
  x=6
  !$omp end parallel
  
  !$omp parallel private (x) allocate (0.1 : x) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind at .1." }
  x=7
  !$omp end parallel

end subroutine

