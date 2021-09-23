! PR 101309
! { dg-do run { xfail *-*-* } }
! { dg-additional-sources "fc-descriptor-7-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests passing arrays that may not be contiguous through
! descriptors to C functions as assumed-shape arguments.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      integer(C_INT), intent (in) :: a(:,:)
    end subroutine
  end interface

  integer(C_INT), target :: aa(10,5)
  integer(C_INT), target :: bb(10,10)

  ! Test both calling the C function directly, and via another function
  ! that takes an assumed-shape argument.
  call ctest (transpose (aa))
  call ftest (transpose (aa))
  call ctest (bb(2:10:2, :))
  call ftest (bb(2:10:2, :))

contains
  subroutine ftest (a)
    use iso_c_binding
    integer(C_INT), intent(in) :: a(:,:)
    call ctest (a)
  end subroutine

end program
