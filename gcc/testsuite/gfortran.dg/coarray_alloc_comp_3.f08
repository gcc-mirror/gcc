! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Contributed by Andre Vehreschild
! Check that manually freeing components does not lead to a runtime crash,
! when the auto-deallocation is taking care.

program coarray_alloc_comp_3
  implicit none

  type dt
    integer, allocatable :: i
  end type dt

  type linktype
    type(dt), allocatable :: link
  end type linktype

  type(linktype), allocatable :: obj[:]

  allocate(obj[*])
  allocate(obj%link)

  if (.not. allocated(obj)) error stop "Test failed. 'obj' not allocated."
  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' not allocated."
  if (allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' already allocated."

  allocate(obj%link%i, source = 42)

  if (.not. allocated(obj)) error stop "Test failed. 'obj' not allocated."
  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' not allocated."
  if (.not. allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' not allocated."
  if (obj%link%i /= 42) error stop "Test failed. obj%link%i /= 42."

  deallocate(obj%link%i)

  if (allocated(obj%link%i)) error stop "Test failed. 'obj%link%i' still allocated."
  if (.not. allocated(obj%link)) error stop "Test failed. 'obj%link' no longer allocated."
  if (.not. allocated(obj)) error stop "Test failed. 'obj' no longer allocated."

  ! Freeing this object, lead to crash with older gfortran...
  deallocate(obj%link)

  if (allocated(obj%link)) error stop "Test failed. 'obj%link' still allocated."
  if (.not. allocated(obj)) error stop "Test failed. 'obj' no longer allocated."

  ! ... when auto-deallocating the allocated components.
  deallocate(obj)

  if (allocated(obj)) error stop "Test failed. 'obj' still allocated."
end program
