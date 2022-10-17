! { dg-do run }
! { dg-additional-sources "fc-descriptor-1-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This checks that a C function declared to have an assumed-shape array
! argument can be called from Fortran and receives a correct descriptor.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(:,:)
    end subroutine
  end interface

  integer(C_INT) :: aa(10,-1:3)

  ! Test both passing the fixed-size array directly to the function
  ! with a C interface, and indirectly via a Fortran function with an
  ! assumed-shape dummy argument.
  call ctest (aa)
  call ftest (aa)

contains
  subroutine ftest (a)
    use iso_c_binding
    integer(C_INT) :: a(:,:)
    call ctest (a)
  end subroutine

end program
