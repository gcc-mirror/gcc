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
    if (allocated (p)) STOP 1
  else
    if (.not. allocated (p)) STOP 2
  end if
  if (allocated (q)) STOP 3

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
  if (allocated(r)) STOP 4
end subroutine
end program
