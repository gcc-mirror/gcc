! { dg-do run }
!
! PR fortran/103828
! Check that we can pass many function args as C char, which are interoperable
! with both INTEGER(C_SIGNED_CHAR) and CHARACTER(C_CHAR).

program test
  use, intrinsic :: iso_c_binding, only : c_signed_char, c_char
  implicit none

  interface
    ! In order to perform this test, we cheat and pretend to give each function
    ! the other one's prototype. It should still work, because all arguments
    ! are interoperable with C char.

    subroutine test1 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) bind(c, name='test_int')
      import c_char
      character(kind=c_char, len=1), value :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o
    end subroutine test1

    subroutine test2 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) bind(c, name='test_char')
      import c_signed_char
      integer(kind=c_signed_char), value :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o
    end subroutine test2

  end interface

  call test1('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')
  call test2(ichar('a', kind=c_signed_char), &
             ichar('b', kind=c_signed_char), &
             ichar('c', kind=c_signed_char), &
             ichar('d', kind=c_signed_char), &
             ichar('e', kind=c_signed_char), &
             ichar('f', kind=c_signed_char), &
             ichar('g', kind=c_signed_char), &
             ichar('h', kind=c_signed_char), &
             ichar('i', kind=c_signed_char), &
             ichar('j', kind=c_signed_char), &
             ichar('k', kind=c_signed_char), &
             ichar('l', kind=c_signed_char), &
             ichar('m', kind=c_signed_char), &
             ichar('n', kind=c_signed_char), &
             ichar('o', kind=c_signed_char))

end program test

subroutine test_int (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) bind(c)
  use, intrinsic :: iso_c_binding, only : c_signed_char
  implicit none
  integer(c_signed_char), value :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o

  if (a /= iachar('a')) stop 1
  if (b /= iachar('b')) stop 2
  if (c /= iachar('c')) stop 3
  if (d /= iachar('d')) stop 4
  if (e /= iachar('e')) stop 5
  if (f /= iachar('f')) stop 6
  if (g /= iachar('g')) stop 7
  if (h /= iachar('h')) stop 8
  if (i /= iachar('i')) stop 9
  if (j /= iachar('j')) stop 10
  if (k /= iachar('k')) stop 11
  if (l /= iachar('l')) stop 12
  if (m /= iachar('m')) stop 13
  if (n /= iachar('n')) stop 14
  if (o /= iachar('o')) stop 15
end subroutine

subroutine test_char (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) bind(c)
  use, intrinsic :: iso_c_binding, only : c_char
  implicit none
  character(kind=c_char, len=1), value :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o

  if (a /= 'a') stop 101
  if (b /= 'b') stop 102
  if (c /= 'c') stop 103
  if (d /= 'd') stop 104
  if (e /= 'e') stop 105
  if (f /= 'f') stop 106
  if (g /= 'g') stop 107
  if (h /= 'h') stop 108
  if (i /= 'i') stop 109
  if (j /= 'j') stop 110
  if (k /= 'k') stop 111
  if (l /= 'l') stop 112
  if (m /= 'm') stop 113
  if (n /= 'n') stop 114
  if (o /= 'o') stop 115
end subroutine

