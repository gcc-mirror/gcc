! { dg-do run }
! PR fortran/83874
! There was an ICE while initializing the character arrays
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
program charinit
  implicit none
  type t
     character(len=1) :: name
  end type t
  type(t), parameter :: z(2)= [ t ('a'), t ('b') ]
  character(len=1), parameter :: names1(*) = z% name
  character(len=*), parameter :: names2(2) = z% name
  character(len=*), parameter :: names3(*) = z% name
  if (.not. (names1(1) == "a" .and. names1(2) == "b")) STOP 1
  if (.not. (names2(1) == "a" .and. names2(2) == "b")) STOP 2
  if (.not. (names3(1) == "a" .and. names3(2) == "b")) STOP 3
end program charinit
