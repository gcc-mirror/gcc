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

  if ((a .ne. "01234567") .or. (n .ne. 8)) call abort
  if (len(Tom%name) .ne. 10) call abort
end

function w(i)
  character(len=8) w
  integer i
  w = "01234567"
  i = len(w)
end
