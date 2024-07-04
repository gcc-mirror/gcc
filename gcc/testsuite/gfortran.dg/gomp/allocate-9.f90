module m
use iso_c_binding
integer, parameter :: omp_allocator_handle_kind = c_intptr_t
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_null_allocator = 0
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_default_mem_alloc = 1
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_large_cap_mem_alloc = 2
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_const_mem_alloc = 3
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_high_bw_mem_alloc = 4
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_low_lat_mem_alloc = 5
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_cgroup_mem_alloc = 6
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_pteam_mem_alloc = 7
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_thread_mem_alloc = 8
end


module m2
  use m
  implicit none
  integer :: A(5) = [1,2,3,4,5], A2, A3, A4, A5
  integer :: B, C, D

! If the following fails because of added predefined allocators, please update
! - c/c-parser.c's c_parser_omp_allocate
! - fortran/openmp.cc's is_predefined_allocator
! - libgomp/env.c's parse_allocator
! - libgomp/libgomp.texi (document the new values - multiple locations)
! + ensure that the memory-spaces are also up to date.

!$omp allocate(A) align(32) allocator(9_omp_allocator_handle_kind)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'a' at .2. has the SAVE attribute" }

! typo in allocator name:
!$omp allocate(A2) allocator(omp_low_latency_mem_alloc)  ! { dg-error "Symbol 'omp_low_latency_mem_alloc' at .1. has no IMPLICIT type; did you mean 'omp_low_lat_mem_alloc'\\?" }
! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'a2' at .2. has the SAVE attribute" "" { target *-*-* } .-1 }

! align be const multiple of 2
!$omp allocate(A3) align(31) allocator(omp_default_mem_alloc) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }

! allocator missing (required as A is static)
!$omp allocate(A4) align(32) ! { dg-error "An ALLOCATOR clause is required as the list item 'a4' at .1. has the SAVE attribute" }

! "expression in the clause must be a constant expression that evaluates to one of the
! predefined memory allocator values -> omp_low_lat_mem_alloc"
!$omp allocate(B) allocator(omp_high_bw_mem_alloc+1_omp_allocator_handle_kind) align(32) ! OK: omp_low_lat_mem_alloc

!$omp allocate(C) allocator(2_omp_allocator_handle_kind) ! OK: omp_large_cap_mem_alloc

!$omp allocate(A5) align(32) allocator(omp_null_allocator) ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'a5' at .2. has the SAVE attribute" }

!$omp allocate(C) align(32) allocator(omp_large_cap_mem_alloc)  ! { dg-error "Duplicated variable 'c' in !.OMP ALLOCATE at .1." }

contains

integer function f()
  !$omp allocate(D) align(32) allocator(omp_large_cap_mem_alloc) ! { dg-error "Argument 'd' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
  f = A(1)
end

integer function g()
  integer :: a2, b2
  !$omp allocate(a2)
  !$omp allocate(a2)  ! { dg-error "Duplicated variable 'a2' in !.OMP ALLOCATE at .1." }
  a2=1; b2=2
  block
    integer :: c2
    !$omp allocate(c2, b2) ! { dg-error "Argument 'b2' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
    c2 = 3
    g = c2+a2+b2
  end block
end

integer function h(q)
  integer :: q
  !$omp allocate(q)  ! { dg-error "Unexpected dummy argument 'q' as argument at .1. to declarative !.OMP ALLOCATE" }
  h = q
end

integer function k ()
  integer, save :: var3 = 8
  !$omp allocate(var3) allocator(-1_omp_allocator_handle_kind)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'var3' at .2. has the SAVE attribute" }
  k = var3
end
end module


subroutine foo
  integer :: a, b
  integer :: c, d,h
  !$omp allocate(a,b)
  b = 1; d = 5
contains
subroutine internal
  integer :: e,f
  !$omp allocate(c,d)
  ! { dg-error "Argument 'c' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" "" { target *-*-* } .-1 }
  ! { dg-error "Argument 'd' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" "" { target *-*-* } .-2 }
  !$omp allocate(e)
  a = 1; c = 2; e = 4
  block
    !$omp allocate(f) ! { dg-error "Argument 'f' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
    !$omp allocate(h) ! { dg-error "Argument 'h' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
  end block
end
end
