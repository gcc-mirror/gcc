! { dg-do run }
! { dg-additional-sources "fc-descriptor-9-c.c dump-descriptors.c" }
!
! Check that C descriptors follow the layout restrictions described in
! section 8.3.3 of TS29113.
! This program is just a stub to create a descriptor and pass it to the
! C function, which does the actual test.

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
  call ctest (aa)

end program
