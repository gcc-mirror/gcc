! { dg-do run }
! { dg-additional-sources "fc-descriptor-2-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that a C function declared to take an assumed-rank
! array argument can be called from Fortran, and receives a correct
! descriptor.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a, n) bind (c)
      use iso_c_binding
      real(C_FLOAT) :: a(..)
      integer(C_INT), value :: n
    end subroutine
  end interface

  real(C_FLOAT) :: aa(100)
  real(C_FLOAT) :: bb(3,4,5)

  ! Test both passing the fixed-size array directly to the function
  ! with a C interface, and indirectly via a Fortran function with an
  ! assumed-rank dummy argument.
  call ctest (aa, 1)
  call ctest (bb, 3)
  call ftest (aa, 1)
  call ftest (bb, 3)

contains
  subroutine ftest (a, n)
    use iso_c_binding
    real(C_FLOAT) :: a(..)
    integer, value :: n
    call ctest (a, n)
  end subroutine

end program
