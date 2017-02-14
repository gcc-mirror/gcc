! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Contributed by Andre Vehreschild
! Check that sub-components are caf_deregistered and not freed.

program coarray_alloc_comp_3
  implicit none

  type dt
    integer, allocatable :: i
  end type dt

  type linktype
    type(dt), allocatable :: link
  end type linktype

  type(linktype) :: obj[*]

  allocate(obj%link)

  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' not allocated."
  if (allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' already allocated."

  allocate(obj%link%i, source = 42)

  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' not allocated."
  if (.not. allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' not allocated."
  if (obj%link%i /= 42) error stop "Test failed. obj%link%i /= 42."

  deallocate(obj%link%i)

  if (allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' still allocated."
  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' no longer allocated."

  ! Freeing this object, lead to crash with older gfortran...
  deallocate(obj%link)

  if (allocated(obj%link)) error stop "Test failed. 'obj%link' still allocated."
end program
! Ensure, that three calls to deregister are present.
! { dg-final { scan-tree-dump-times "_caf_deregister" 3 "original" } }
! And ensure that no calls to builtin_free are made.
! { dg-final { scan-tree-dump-not "_builtin_free" "original" } }
