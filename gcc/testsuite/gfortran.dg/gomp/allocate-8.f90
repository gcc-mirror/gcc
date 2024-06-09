! { dg-additional-options "-fdump-tree-original" }

module m
  use iso_c_binding
  !use omp_lib, only: omp_allocator_handle_kind
  implicit none
  integer, parameter :: omp_allocator_handle_kind = c_intptr_t
  integer :: a = 0, b = 42, c = 0

contains
  integer(omp_allocator_handle_kind) function get_alloc()
    allocatable :: get_alloc
    get_alloc = 2_omp_allocator_handle_kind
  end
  subroutine foo ()
  !$omp scope private (a) firstprivate (b) reduction (+: c) allocate ( get_alloc() : a , b , c)
    if (b /= 42) &
      error stop
    a = 36
    b = 15
    c = c + 1
  !$omp end scope
  end
end

! { dg-final { scan-tree-dump "omp scope private\\(a\\) firstprivate\\(b\\) reduction\\(\\+:c\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):a\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):b\\) allocate\\(allocator\\(D\\.\[0-9\]+\\):c\\)" "original" } }

! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = get_alloc \\(\\);\[\n\r\]+ *D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;\[\n\r\]+ *__builtin_free \\(\\(void \\*\\) D\\.\[0-9\]+\\);" 1 "original" } }

