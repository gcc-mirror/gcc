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
  if(.not. allocated(a)) call abort()
  if (a /= 5) call abort()

  call bar(a)
  if (a /= 7) call abort()

  deallocate(a)
  if(allocated(a)) call abort()
  call check3(a)
  if(.not. allocated(a)) call abort()
  if(a /= 6874) call abort()
  call check4(a)
  if(.not. allocated(a)) call abort()
  if(a /= -478) call abort()

  allocate(b)
  b = 7482
  call checkOptional(.false.,.true., 7482)
  if (b /= 7482) call abort()
  call checkOptional(.true., .true., 7482, b)
  if (b /= 46) call abort()
contains
  subroutine foo(a)
    integer, allocatable, intent(out)  :: a
    if(allocated(a)) call abort()
    allocate(a)
    a = 5
  end subroutine foo

  subroutine bar(a)
    integer, allocatable, intent(inout)  :: a
    if(.not. allocated(a)) call abort()
    if (a /= 5) call abort()
    a = 7
  end subroutine bar

  subroutine check3(a)
    integer, allocatable, intent(inout)  :: a
    if(allocated(a)) call abort()
    allocate(a)
    a = 6874
  end subroutine check3

  subroutine check4(a)
    integer, allocatable, intent(inout)  :: a
    if(.not.allocated(a)) call abort()
    if (a /= 6874) call abort
    deallocate(a)
    if(allocated(a)) call abort()
    allocate(a)
    if(.not.allocated(a)) call abort()
    a = -478
  end subroutine check4

  subroutine checkOptional(prsnt, alloc, val, x)
    logical, intent(in) :: prsnt, alloc
    integer, allocatable, optional :: x
    integer, intent(in) :: val
    if (present(x) .neqv. prsnt) call abort()
    if (present(x)) then
      if (allocated(x) .neqv. alloc) call abort()
    end if
    if (present(x)) then
      if (allocated(x)) then
        if (x /= val) call abort()
      end if
    end if
    call checkOptional2(x)
    if (present(x)) then
      if (.not. allocated(x)) call abort()
      if (x /= -6784) call abort()
      x = 46
    end if
    call checkOptional2()
  end subroutine checkOptional
  subroutine checkOptional2(x)
    integer, allocatable, optional, intent(out) :: x
    if (present(x)) then
      if (allocated(x)) call abort()
      allocate(x)
      x = -6784
    end if
  end subroutine checkOptional2
end program test
