! Reported as pr94070.
! { dg-do run }
! { dg-additional-sources "fc-descriptor-6-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that an assumed-size array argument can be passed
! to a C function via a descriptor, and that the argument descriptor
! received by C correctly identifies it as assumed-size.

program testit
  use iso_c_binding
  implicit none

  ! Assumed-size arrays are not passed by descriptor.  What we'll do
  ! for this test function is bind an assumed-rank dummy
  ! to the assumed-size array.  This is supposed to fill in the descriptor
  ! with information about the array present at the call site.
  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(..)
    end subroutine
  end interface

  integer(C_INT), target :: aa(10,5:8)

  ! To get an assumed-size array descriptor, we have to first pass the
  ! fixed-size array to a Fortran function with an assumed-size dummy,
  call ftest1 (aa)
  call ftest2 (aa)
  call ftest3 (aa)

contains
  subroutine ftest1 (a)
    use iso_c_binding
    integer(C_INT) :: a(10,*)
    call ctest (a)
  end subroutine
  subroutine ftest2 (a)
    use iso_c_binding
    integer(C_INT) :: a(10,5:*)
    call ctest (a)
  end subroutine
  subroutine ftest3 (a)
    use iso_c_binding
    integer(C_INT) :: a(10,1:*)
    call ctest (a)
  end subroutine

end program
