! { do-do run }
! PR 51858 - this used to generate wrong code.
! Original test case by Don Simons.

program main
  implicit none
  logical :: test1_ok
  logical :: test2_ok
  logical :: test3_ok
  character(len=1):: charq

  charq = 'c'
  
  test1_ok = .true.
  test2_ok = .false.
  if (charq .eq. ' ') then
     test1_ok = .false.
  else if ((my_ichar(charq).ge.97 .and. my_ichar(charq).le.103)) then
     test2_OK = .true.
  end if
  if ((.not. test1_ok) .or. (.not. test2_ok)) call abort

  test1_ok = .true.
  test2_ok = .true.
  test3_ok = .false.

  if (charq .eq. ' ') then
     test1_ok = .false.
  else if ((my_ichar(charq).lt.97 .or. my_ichar(charq).gt.103)) then
     test2_ok = .false.
  else if ((my_ichar(charq).ge.97 .and. my_ichar(charq).le.103)) then
     test3_ok = .true.
  end if
  if ((.not. test1_ok) .or. (.not. test2_ok) .or. (.not. test3_ok)) call abort

  test1_ok = .true.
  test2_ok = .true.
  test3_ok = .false.

  if (charq .eq. ' ') then
     test1_ok = .false.
  else if ((my_ichar(charq).lt.97 .or. my_ichar(charq).gt.103)) then
     test2_ok = .false.
  else
     test3_ok = .true.
  end if

  if ((.not. test1_ok) .or. (.not. test2_ok) .or. (.not. test3_ok)) call abort

contains
  pure function my_ichar(c)
    integer :: my_ichar
    character(len=1), intent(in) :: c
    my_ichar = ichar(c)
  end function my_ichar
end program main

