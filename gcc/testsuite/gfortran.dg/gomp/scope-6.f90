! { dg-additional-options "-fdump-tree-original" }

module m
  use iso_c_binding
  !use omp_lib, only: omp_allocator_handle_kind
  implicit none
  integer, parameter :: omp_allocator_handle_kind = c_intptr_t
  integer :: a = 0, b = 42, c = 0

contains
  subroutine foo (h)
  integer(omp_allocator_handle_kind), value :: h 
  !$omp scope private (a) firstprivate (b) reduction (+: c) allocate ( h : a , b , c)
    if (b /= 42) &
      error stop
    a = 36
    b = 15
    c = c + 1
  !$omp end scope
  end
end

! { dg-final { scan-tree-dump "omp scope private\\(a\\) firstprivate\\(b\\) reduction\\(\\+:c\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):a\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):b\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):c\\)" "original" } }
