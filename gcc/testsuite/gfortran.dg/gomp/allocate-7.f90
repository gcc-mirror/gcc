! { dg-additional-options "-fmax-errors=1000" }
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
   integer :: used
end module my_omp_lib

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
integer, parameter :: prm=5

!$omp allocate(prm) align(64) ! { dg-error "Argument 'prm' at .1. to declarative !.OMP ALLOCATE directive must be a variable" }

!$omp allocate(used) allocator(omp_pteam_mem_alloc)  ! { dg-error "Argument 'used' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
!$omp allocate(n) allocator(omp_pteam_mem_alloc) ! { dg-error "Unexpected dummy argument 'n' as argument at .1. to declarative !.OMP ALLOCATE" }

!$omp allocate (x) align(128) ! { dg-error "'x' at .1. is part of the common block '/com1/' and may only be specificed implicitly via the named common block" }

!$omp allocate (a, b, a) allocator (omp_pteam_mem_alloc) ! { dg-error "Duplicated variable 'a' in !.OMP ALLOCATE" }
contains

  subroutine inner
    !$omp allocate(a) allocator(omp_pteam_mem_alloc)  ! { dg-error "Argument 'a' at .1. to declarative !.OMP ALLOCATE shall be in the same scope as the variable declaration" }
  end
end

subroutine three(n)
  use my_omp_lib
  implicit none
integer,value :: n
integer :: a,b,c(n),d(5),e(2)
integer, save :: k,l,m(5)
integer :: q,x,y(2),z(5),r
common /com4/ y,z
allocatable :: q
pointer :: b
!$omp allocate (c, d) allocator (omp_pteam_mem_alloc)
!$omp allocate (/com4/) allocator (omp_pteam_mem_alloc)
!$omp allocate (c) allocator (omp_pteam_mem_alloc) ! { dg-error "Duplicated variable 'c' in !.OMP ALLOCATE" }
!$omp allocate (/com4/) allocator (omp_pteam_mem_alloc) ! { dg-error "Duplicated common block '/com4/' in !.OMP ALLOCATE" }

!$omp allocate(q,x)  ! { dg-error "Unexpected allocatable variable 'q' at .1. in declarative !.OMP ALLOCATE directive" }
!$omp allocate(b,e)  ! { dg-error "Unexpected pointer variable 'b' at .1. in declarative !.OMP ALLOCATE directive" }
end

subroutine four(n)
  integer :: qq, rr, ss, tt, uu, vv,n
!$omp allocate (qq) align(3+n) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
!$omp allocate (rr) align([4]) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
!$omp allocate (ss) align([4]) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
!$omp allocate (tt) align(32.0) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
!$omp allocate (uu) align(31) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
end

subroutine five(n,my_alloc)
  use my_omp_lib
  implicit none
  integer :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
!$omp allocate (tt) allocator(my_alloc)  ! OK
end


subroutine five_SaveAll(n,my_alloc)
  use my_omp_lib
  implicit none
  save
  integer :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'qq' at .2. has the SAVE attribute" }
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'rr' at .2. has the SAVE attribute" }
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'ss' at .2. has the SAVE attribute" }
!$omp allocate (tt) allocator(my_alloc)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'tt' at .2. has the SAVE attribute" }
end


subroutine five_Save(n,my_alloc)
  use my_omp_lib
  implicit none
  integer :: n
  integer, save :: qq, rr, ss, tt, uu, vv
  integer(omp_allocator_handle_kind) :: my_alloc
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'qq' at .2. has the SAVE attribute" }
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'rr' at .2. has the SAVE attribute" }
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'ss' at .2. has the SAVE attribute" }
!$omp allocate (tt) allocator(my_alloc)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'tt' at .2. has the SAVE attribute" }
end

module five_Module
  use my_omp_lib
  implicit none
  integer, save :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'qq' at .2. has the SAVE attribute" }
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'rr' at .2. has the SAVE attribute" }
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'ss' at .2. has the SAVE attribute" }
!$omp allocate (tt) allocator(my_alloc)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'tt' at .2. has the SAVE attribute" }
end module

program five_program
  use my_omp_lib
  implicit none
  integer, save :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'qq' at .2. has the SAVE attribute" }
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'rr' at .2. has the SAVE attribute" }
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'ss' at .2. has the SAVE attribute" }
!$omp allocate (tt) allocator(my_alloc)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item 'tt' at .2. has the SAVE attribute" }
end program



subroutine six(n,my_alloc)
  use my_omp_lib
  implicit none
  integer :: qq, rr, ss, tt, uu, vv,n
  common /com6qq/ qq
  common /com6rr/ rr
  common /com6ss/ ss
  common /com6tt/ tt
  integer(omp_allocator_handle_kind) :: my_alloc

!$omp allocate (/com6qq/) allocator(3.0)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item '/com6qq/' at .2. has the SAVE attribute" }
!$omp allocate (/com6rr/) allocator(3_2)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item '/com6rr/' at .2. has the SAVE attribute" }
!$omp allocate (/com6ss/) allocator([omp_pteam_mem_alloc])  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item '/com6ss/' at .2. has the SAVE attribute" }
!$omp allocate (/com6tt/) allocator(my_alloc)  ! { dg-error "Predefined allocator required in ALLOCATOR clause at .1. as the list item '/com6tt/' at .2. has the SAVE attribute" }
end


subroutine two()
  use my_omp_lib
  implicit none
  integer,allocatable :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc

  call foo()
!$omp allocate (qq) allocator(3.0)  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
allocate(qq)
!$omp allocate (rr) allocator(3_2)  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
allocate(rr)
!$omp allocate (ss) allocator([omp_pteam_mem_alloc])  ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
allocate(ss)
!$omp allocate (tt) allocator(my_alloc)  ! OK
allocate(tt)
end

subroutine two_ptr()
  use my_omp_lib
  implicit none
  integer,pointer :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc

  call foo()
!$omp allocate (qq) align(3+n) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
allocate(qq)
!$omp allocate (rr) align([4]) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
allocate(rr)
!$omp allocate (ss) align([4]) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
allocate(ss)
!$omp allocate (tt) align(32.0) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
allocate(tt)
!$omp allocate (uu) align(31) ! { dg-error "ALIGN requires a scalar positive constant integer alignment expression at .1. that is a power of two" }
allocate(uu)
end

subroutine next()
  use my_omp_lib
  implicit none
  integer,allocatable :: qq, rr, ss, tt, uu, vv,n
  integer(omp_allocator_handle_kind) :: my_alloc

  !$omp allocate(qq)  ! { dg-error "'!.OMP ALLOCATE' directive at .1. associated with an ALLOCATE stmt must be preceded by an executable statement or OpenMP construct; note the variables in the list all have the allocatable or pointer attribute" }
   allocate(qq,rr)

  !$omp allocate(uu,tt)
  !$omp allocate(tt)  ! { dg-warning "'tt' appears more than once in 'allocate" }
   allocate(uu,tt)

  !$omp allocate(uu,vv) ! { dg-error "'uu' specified in 'allocate' at .1. but not in the associated ALLOCATE statement" }
   allocate(vv)
end
