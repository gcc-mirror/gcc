! { dg-do run }
! { dg-options "-fbackslash" }
!
! PR fortran/103828
! Check that we can C char with non-ASCII values, which are interoperable
! with both INTEGER(C_SIGNED_CHAR) and CHARACTER(C_CHAR).

program test
  use, intrinsic :: iso_c_binding, only : c_signed_char, c_char
  implicit none

  interface
    ! In order to perform this test, we cheat and pretend to give each function
    ! the other one's prototype. It should still work, because all arguments
    ! are interoperable with C char.

    subroutine test1 (a) bind(c, name='test_int')
      import c_char
      character(kind=c_char, len=1), value :: a
    end subroutine test1

    subroutine test2 (a) bind(c, name='test_char')
      import c_signed_char
      integer(kind=c_signed_char), value :: a
    end subroutine test2

  end interface

  call test1('\xA3')
  call test2(-93_c_signed_char)

end program test

subroutine test_int (a) bind(c)
  use, intrinsic :: iso_c_binding, only : c_signed_char
  implicit none
  integer(c_signed_char), value :: a

  if (a /= iachar('\xA3', kind=c_signed_char)) stop 1
end subroutine

subroutine test_char (a) bind(c)
  use, intrinsic :: iso_c_binding, only : c_char
  implicit none
  character(kind=c_char, len=1), value :: a

  if (a /= '\xA3') stop 101
end subroutine

