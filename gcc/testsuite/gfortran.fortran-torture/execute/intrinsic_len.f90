! Program to test the LEN intrinsic
program test
  character(len=10) a
  character(len=8) w
  type person
    character(len=10) name
    integer age
  end type person
  type(person) Tom
  integer n
  a = w (n)

  if ((a .ne. "01234567") .or. (n .ne. 8)) STOP 1
  if (len(Tom%name) .ne. 10) STOP 2
  call array_test()
end

function w(i)
  character(len=8) w
  integer i
  w = "01234567"
  i = len(w)
end

! This is the testcase from PR 15211 converted to a subroutine
subroutine array_test
   implicit none
   character(len=10) a(4)
   if (len(a) .NE. 10) STOP 1
end subroutine array_test

