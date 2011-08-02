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
  if (a /= 'A') call abort()
  b = bar('y')
  if (b /= 'A' .or. iachar(b(2:2))/=32 .or. iachar(b(3:3))/=32) call abort()
  c = bar('x')
  if (any(c /= 'A')) call abort()
  d = bar('y')
  if (any(d /= 'A')) call abort()

  a = foo()
  if (a /= 'B') call abort()
  b = foo()
  if (b /= 'B') call abort()
  c = foo()
  if (any(c /= 'B')) call abort()
  d = foo()
  if (any(d /= 'B')) call abort()
  do i = 1,3
    if(iachar(d(i)(2:2)) /=32 .or. iachar(d(i)(3:3)) /= 32) call abort()
  end do
end subroutine

! { dg-final { cleanup-modules "mod" } }
