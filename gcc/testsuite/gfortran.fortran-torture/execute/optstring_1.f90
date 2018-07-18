! Test optional character arguments.  We still need to pass a string
! length for the absent arguments
program optional_string_1
  implicit none

  call test(1, "test");
  call test(2, c=42, b="Hello World")
contains
subroutine test(i, a, b, c)
  integer ::  i
  character(len=4), optional :: a
  character(len=*), optional :: b
  integer, optional :: c
  if (i .eq. 1) then
    if (a .ne. "test") STOP 1
  else
    if (b .ne. "Hello World") STOP 2
    if (c .ne. 42) STOP 3
  end if
end subroutine
end program
