! { dg-do run }
!
! PR fortran/41872
!
!
program test
  implicit none
  integer, allocatable :: a
  integer, allocatable :: b
  allocate(a)
  call foo(a)
  if(.not. allocated(a)) STOP 1
  if (a /= 5) STOP 2

  call bar(a)
  if (a /= 7) STOP 3

  deallocate(a)
  if(allocated(a)) STOP 4
  call check3(a)
  if(.not. allocated(a)) STOP 5
  if(a /= 6874) STOP 6
  call check4(a)
  if(.not. allocated(a)) STOP 7
  if(a /= -478) STOP 8

  allocate(b)
  b = 7482
  call checkOptional(.false.,.true., 7482)
  if (b /= 7482) STOP 9
  call checkOptional(.true., .true., 7482, b)
  if (b /= 46) STOP 10
contains
  subroutine foo(a)
    integer, allocatable, intent(out)  :: a
    if(allocated(a)) STOP 11
    allocate(a)
    a = 5
  end subroutine foo

  subroutine bar(a)
    integer, allocatable, intent(inout)  :: a
    if(.not. allocated(a)) STOP 12
    if (a /= 5) STOP 13
    a = 7
  end subroutine bar

  subroutine check3(a)
    integer, allocatable, intent(inout)  :: a
    if(allocated(a)) STOP 14
    allocate(a)
    a = 6874
  end subroutine check3

  subroutine check4(a)
    integer, allocatable, intent(inout)  :: a
    if(.not.allocated(a)) STOP 15
    if (a /= 6874) STOP 1
    deallocate(a)
    if(allocated(a)) STOP 16
    allocate(a)
    if(.not.allocated(a)) STOP 17
    a = -478
  end subroutine check4

  subroutine checkOptional(prsnt, alloc, val, x)
    logical, intent(in) :: prsnt, alloc
    integer, allocatable, optional :: x
    integer, intent(in) :: val
    if (present(x) .neqv. prsnt) STOP 18
    if (present(x)) then
      if (allocated(x) .neqv. alloc) STOP 19
    end if
    if (present(x)) then
      if (allocated(x)) then
        if (x /= val) STOP 20
      end if
    end if
    call checkOptional2(x)
    if (present(x)) then
      if (.not. allocated(x)) STOP 21
      if (x /= -6784) STOP 22
      x = 46
    end if
    call checkOptional2()
  end subroutine checkOptional
  subroutine checkOptional2(x)
    integer, allocatable, optional, intent(out) :: x
    if (present(x)) then
      if (allocated(x)) STOP 23
      allocate(x)
      x = -6784
    end if
  end subroutine checkOptional2
end program test
