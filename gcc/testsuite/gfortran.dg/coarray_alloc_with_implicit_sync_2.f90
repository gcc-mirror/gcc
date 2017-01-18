! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
! 
! Test that the compiler generates sync_all statements only at the required
! locations. This program is not supposed to run (allocating already alloced).

program test_alloc_sync

  type :: T
    integer, allocatable :: i
  end type T
  type :: T2
    type(T), allocatable :: o[:]
  end type T2

  integer, allocatable :: caf[:]
  type (T) :: obj[*]
  type (T2) :: cafcomp

  allocate(caf[*])             ! implicit sync_all
  allocate(obj%i)              ! asynchronous
  allocate(cafcomp%o[*])       ! sync
  allocate(cafcomp%o%i)        ! async

  allocate(obj%i, cafcomp%o%i) ! async
  allocate(caf[*], obj%i, cafcomp%o%i) ! sync

end program test_alloc_sync

! { dg-final { scan-tree-dump-times "caf_sync_all" 3 "original" } }
