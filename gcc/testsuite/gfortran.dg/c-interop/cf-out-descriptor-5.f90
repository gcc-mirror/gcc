! PR92482
! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-5-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks use of an assumed-length character dummy argument
! as an intent(out) parameter in subroutines with C binding.

subroutine ftest (a, n) bind (c, name="ftest")
  use iso_c_binding
  character(kind=C_CHAR, len=*), intent(out) :: a
  integer(C_INT), value :: n

  if (len (a) .ne. n) stop 101
  a = 'abcdefghijklmnopqrstuvwxyz'
end subroutine

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a, n) bind (c)
      use iso_c_binding
      character(kind=C_CHAR, len=*), intent(out) :: a
      integer(C_INT), value :: n
    end subroutine

    subroutine ftest (a, n) bind (c)
      use iso_c_binding
      character(kind=C_CHAR, len=*), intent(out) :: a
      integer(C_INT), value :: n
    end subroutine
  end interface

  character(kind=C_CHAR, len=42) :: aa

  ! call ftest directly
  aa = '12345678910'
  call ftest (aa, 42)
  print *, aa

  ! ctest calls ftest indirectly
  aa = '12345678910'
  call ctest (aa, 42)
  print *, aa

end program
