! { dg-do run }
!
! PR fortran/51758
!
! Contributed by Mikael Morin
!
! Check whether passing NULL() to an elemental procedure works,
! where NULL() denotes an absent optional argument.
!
program p

  integer :: a(2)
  integer :: b

  a = 0
  a = foo((/ 1, 1 /), null())
!  print *, a
  if (any(a /= 2)) STOP 1

  a = 0
  a = bar((/ 1, 1 /), null())
!  print *, a
  if (any(a /= 2)) STOP 2

  b = 0
  b = bar(1, null())
!  print *, b
  if (b /= 2) STOP 3

contains

  function foo(a, b)
    integer           :: a(:)
    integer, optional :: b(:)
    integer           :: foo(size(a))

    if (present(b)) STOP 4

    foo = 2
  end function foo

  elemental function bar(a, b)
    integer, intent(in)           :: a
    integer, intent(in), optional :: b
    integer                       :: bar

    bar = 2

    if (present(b)) bar = 1

  end function bar

end program p
