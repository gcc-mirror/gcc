! { dg-do run }
! { dg-additional-sources bind_c_usage_16_c.c }
!
! PR fortran/34079
!
! Ensure character-returning, bind(C) function work.
!
module mod
  use iso_c_binding
  implicit none
contains
  function bar(x)  bind(c, name="returnA")
    character(len=1,kind=c_char) :: bar, x
    bar = x
    bar = 'A'
  end function bar
  function foo()  bind(c, name="returnB")
    character(len=1,kind=c_char) :: foo
    foo = 'B'
  end function foo
end module mod

subroutine test() bind(c)
  use mod
  implicit none
  character(len=1,kind=c_char) :: a
  character(len=3,kind=c_char) :: b
  character(len=1,kind=c_char) :: c(3)
  character(len=3,kind=c_char) :: d(3)
  integer :: i

  a = 'z'
  b = 'fffff'
  c = 'h'
  d = 'uuuuu'

  a = bar('x')
  if (a /= 'A') STOP 1
  b = bar('y')
  if (b /= 'A' .or. iachar(b(2:2))/=32 .or. iachar(b(3:3))/=32) STOP 2
  c = bar('x')
  if (any(c /= 'A')) STOP 3
  d = bar('y')
  if (any(d /= 'A')) STOP 4

  a = foo()
  if (a /= 'B') STOP 5
  b = foo()
  if (b /= 'B') STOP 6
  c = foo()
  if (any(c /= 'B')) STOP 7
  d = foo()
  if (any(d /= 'B')) STOP 8
  do i = 1,3
    if(iachar(d(i)(2:2)) /=32 .or. iachar(d(i)(3:3)) /= 32) STOP 9
  end do
end subroutine
