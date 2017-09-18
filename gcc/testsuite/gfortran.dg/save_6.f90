! { dg-do run }
! { dg-require-effective-target lto }
! { dg-options "-fno-automatic -flto -g" }
!
! PR fortran/55733
!
! Check that -fno-automatic makes the local variable SAVEd
! Check that -flto -g works
!

! Scalar allocatable
subroutine foo(i)
  integer :: i
  integer, allocatable :: j
  if (i == 1) j = 42
  if (.not. allocated (j)) call abort ()
  if (j /= 42) call abort ()
end

! Deferred-length string scalar
subroutine bar()
  logical, save :: first = .true.
  character(len=:), allocatable :: str
  if (first) then
    first = .false.
    if (allocated (str)) call abort ()
    str = "ABCDEF"
  end if
  if (.not. allocated (str)) call abort ()
  if (len (str) /= 6) call abort ()
  if (str(1:6) /= "ABCDEF") call abort ()
end subroutine bar

! Deferred-length string array
subroutine bar_array()
  logical, save :: first = .true.
  character(len=:), allocatable :: str
  if (first) then
    first = .false.
    if (allocated (str)) call abort ()
    str = "ABCDEF"
  end if
  if (.not. allocated (str)) call abort ()
  if (len (str) /= 6) call abort ()
  if (str(1:6) /= "ABCDEF") call abort ()
end subroutine bar_array

call foo(1)
call foo(2)
call bar()
call bar_array()
call bar()
call bar_array()
end
