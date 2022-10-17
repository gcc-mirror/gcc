! PR92482
! { dg-do run }
! { dg-additional-sources "cf-descriptor-5-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that building a descriptor for a character object 
! in C works and that you can use it to call back into a Fortran function 
! with an assumed-length dummy that is declared with C binding.

subroutine ftest (a, n) bind (c, name="ftest")
  use iso_c_binding
  character(kind=C_CHAR, len=*) :: a
  integer(C_INT), value :: n

  if (len (a) .ne. n) stop 101
end subroutine

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (n) bind (c)
      use iso_c_binding
      integer(C_INT), value :: n
    end subroutine
  end interface

  call ctest (42)

end program
