! Test allocation and deallocation.
program test_allocate
  call t1 (.true.)
  call t1 (.false.)
  call t2
contains

! Implicit deallocation and saved aloocated variables.
subroutine t1(first)
  real, allocatable, save :: p(:)
  real, allocatable :: q(:)
  logical first

  if (first) then
    if (allocated (p)) call abort ()
  else
    if (.not. allocated (p)) call abort ()
  end if
  if (allocated (q)) call abort ()

  if (first) then
    allocate (p(5))
  else
    deallocate (p)
  end if
  allocate (q(5))
end subroutine

! Explicit deallocation.
subroutine t2()
  real, allocatable :: r(:)

  allocate (r(5))
  pr = 1.0
  deallocate (r)
  if (allocated(r)) call abort ()
end subroutine
end program
