! { dg-additional-options "-fcoarray=single -fcray-pointer" }

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

subroutine coarrays(x)
  use m
  implicit none

  integer :: x[*]
  integer, allocatable :: y[:], z(:)[:]

  !$omp allocate(x)  ! { dg-error "Unexpected dummy argument 'x' as argument at .1. to declarative !.OMP ALLOCATE" }

  !$omp allocators allocate(y) ! { dg-error "29:Unexpected coarray 'y' in 'allocate' at .1." }
    allocate(y[*])

  !$omp allocate(z) ! { dg-error "18:Unexpected coarray 'z' in 'allocate' at .1." }
    allocate(z(5)[*])
  x = 5
end 


integer function f() result(res)
  !$omp allocate(f)   ! { dg-error "Argument 'f' at .1. to declarative !.OMP ALLOCATE directive must be a variable" }
  !$omp allocate(res) ! { dg-error "Unexpected function-result variable 'res' at .1. in declarative !.OMP ALLOCATE" }
  res = 5
end

integer function g() result(res)
  allocatable :: res
  !$omp allocators allocate(g)   ! { dg-error "Expected variable list at .1." }

  !$omp allocators allocate (res)
  allocate(res, source=5)
  deallocate(res)

  !$omp allocate (res)
  allocate(res, source=5)
end


subroutine cray_ptr()
   real pointee(10)
   pointer (ipt, pointee)
   !$omp allocate(pointee)  ! { dg-error "Sorry, Cray pointers and pointees such as 'pointee' are not supported with !.OMP ALLOCATE at .1." }
   !$omp allocate(ipt)      ! { dg-error "Sorry, Cray pointers and pointees such as 'ipt' are not supported with !.OMP ALLOCATE at .1." }
end

subroutine equiv
  integer :: A
  real :: B(2)
  equivalence(A,B)
  !$omp allocate (A)  ! { dg-error "Sorry, EQUIVALENCE object 'a' not supported with !.OMP ALLOCATE at .1." }
  !$omp allocate (B)  ! { dg-error "Sorry, EQUIVALENCE object 'b' not supported with !.OMP ALLOCATE at .1." }
end

subroutine common
  use m
  integer :: a,b,c(5)
  common /my/ a,b,c
  !$omp allocate(b) allocator(omp_cgroup_mem_alloc)  ! { dg-error "'b' at .1. is part of the common block '/my/' and may only be specificed implicitly via the named common block" }
end

subroutine c_and_func_ptrs
  use iso_c_binding
  implicit none
  procedure(), pointer :: p
  type(c_ptr) :: cptr
  type(c_ptr) :: cfunptr

  !$omp allocate(cptr)  ! OK
  !$omp allocate(cfunptr) ! OK? A normal derived-type var?
  !$omp allocate(p)  ! { dg-error "Argument 'p' at .1. to declarative !.OMP ALLOCATE directive must be a variable" }
end


subroutine coarray_2
  use m
  implicit none
  integer :: x
  integer, allocatable :: a, b, c[:], d
  x = 5 ! executable stmt
  !$omp allocate(a,b) align(16)
  !$omp allocate        ! { dg-error "Unexpected coarray 'c' in 'allocate' at .1., implicitly listed in '!.OMP ALLOCATE' at .2." }
  !$omp allocate(d) align(32)
  allocate(a,b,c[*],d)  ! { dg-error "Unexpected coarray 'c' in 'allocate' at .1., implicitly listed in '!.OMP ALLOCATE' at .2." }
end


subroutine coarray_3
  use m
  implicit none
  integer :: x
  integer, allocatable :: a, b, c[:], d
  x = 5 ! executable stmt
  !$omp allocators allocate(align(16): a,b) allocate(align(32) : d) 
  allocate(a,b,c[*],d)  ! OK - Fortran allocator used for 'C'
end


subroutine unclear
  use m
  implicit none
  integer :: x
  integer, allocatable :: a, b, c[:], d

  ! OpenMP is unclear which allocator is used for 'C' - the fortran one or the OpenMP one.
  ! GCC therefore rejects it.

  x = 5 ! executable stmt

  !$omp allocate(a,b) align(16)
  !$omp allocate(d) align(32)
  allocate(a,b,c[*],d)  ! { dg-error "'c' listed in 'allocate' statement at .1. but it is neither explicitly in listed in the '!.OMP ALLOCATE' directive nor exists a directive without argument list" }
end
