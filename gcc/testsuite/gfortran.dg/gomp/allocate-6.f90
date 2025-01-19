module my_omp_lib
  use iso_c_binding, only: c_intptr_t
  !use omp_lib
  implicit none
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
   type t
     integer,allocatable :: a
     integer,pointer :: b(:,:)
   end type t
end module my_omp_lib

subroutine zero()
  !$omp assumes absent (allocate)  ! { dg-error "Invalid 'ALLOCATE' directive at .1. in ABSENT clause: declarative, informational, and meta directives not permitted" }

  !$omp assume absent (allocate)  ! { dg-error "Invalid 'ALLOCATE' directive at .1. in ABSENT clause: declarative, informational, and meta directives not permitted" }
  !!$omp end assume
end

subroutine alloc(c,x2,y2)
  use my_omp_lib
  implicit none
  integer, allocatable :: a, b(:), c(:,:)
  type(t) :: x1,x2
  class(t) :: y1,y2
  allocatable :: x1, y1

  !$omp flush  ! some executable statement

  !$omp allocate(x2%a,x2%b,y2%a,y2%b) allocator(omp_pteam_mem_alloc) align(64)  ! { dg-error "Sorry, structure-element list item at .1. in ALLOCATE directive is not yet supported" }
  allocate(x2%a,x2%b(3,4),y2%a,y2%b(3,4))

  !$omp allocate(b(3)) align ( 64 ) ! { dg-error "Unexpected expression as list item at .1. in ALLOCATE directive" }
  allocate(b(3))
end

subroutine one(n, my_alloc)
  use my_omp_lib
  implicit none
integer :: n
integer(kind=omp_allocator_handle_kind), intent(in) :: my_alloc

integer :: a,b,c(n),d(5),e(2)
integer, save :: k,l,m(5),r(2)
integer :: q,x,y(2),z(5)
common /com1/ q,x
common /com2/ y,z
integer, allocatable :: alloc
integer, pointer :: ptr

!$omp allocate(q) ! { dg-error "'q' at .1. is part of the common block '/com1/' and may only be specificed implicitly via the named common block" }

!$omp allocate(d(:)) ! { dg-error "Unexpected expression as list item at .1. in ALLOCATE directive" }
!$omp allocate(a) align(4), align(4)  ! { dg-error "Duplicated 'align' clause" }
!$omp allocate(   e ) allocator( omp_high_bw_mem_alloc ), align(32),allocator( omp_high_bw_mem_alloc )  ! { dg-error "Duplicated 'allocator' clause" }

!$omp allocate align(32) ! { dg-error "'!.OMP ALLOCATE' directive at .1. must either have a variable argument or, if associated with an ALLOCATE stmt, must be preceded by an executable statement or OpenMP construct" }

!$omp allocate(alloc) align(128)  ! { dg-error "'!.OMP ALLOCATE' directive at .1. associated with an ALLOCATE stmt must be preceded by an executable statement or OpenMP construct; note the variables in the list all have the allocatable or pointer attribute" }
!$omp allocate(ptr) align(128)  ! { dg-error "'!.OMP ALLOCATE' directive at .1. associated with an ALLOCATE stmt must be preceded by an executable statement or OpenMP construct; note the variables in the list all have the allocatable or pointer attribute" }

!$omp allocate(e) allocate(omp_thread_mem_alloc)  ! { dg-error "Expected ALIGN or ALLOCATOR clause" }
end

subroutine two()
  integer, allocatable :: a,b,c

  call foo()
  !$omp allocate(a)
  a = 5  ! { dg-error "Unexpected assignment at .1.; expected ALLOCATE or !.OMP ALLOCATE statement" }

  !$omp allocate  ! { dg-error "!.OMP ALLOCATE statements at .1. and .2. have both no list item but only one may" }
  !$omp allocate(b)
  !$omp allocate  ! { dg-error "!.OMP ALLOCATE statements at .1. and .2. have both no list item but only one may" }
   allocate(a,b,c)

  !$omp allocate
   allocate(a,b,c)  ! allocate is no block construct, hence:
  !$omp end allocate  ! { dg-error "Unclassifiable OpenMP directive" }

  !$omp allocators allocate(align(64) : a, b)
  !$omp allocators allocate(align(128) : c)  ! { dg-error "Unexpected !.OMP ALLOCATORS at .1.; expected ALLOCATE statement after !.OMP ALLOCATORS" }
   allocate(a,b,c)
end
