! Test that the ompx_gnu_pinned_mem_alloc is accepted by the parser

module m
use iso_c_binding
integer, parameter :: omp_allocator_handle_kind = c_intptr_t
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: ompx_gnu_pinned_mem_alloc = 200
end

subroutine f ()
  use m
  implicit none
  ! The "Sorry" is here temporarily only to avoid excess error failures.
  integer, save :: i
  !$omp allocate(i) allocator(ompx_gnu_pinned_mem_alloc)
end
