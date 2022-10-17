! PR 101308
! { dg-do run }
! { dg-additional-sources "fc-descriptor-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that pointer and allocatable scalar arguments are 
! correctly passed by descriptor from Fortran code into C.

program testit
  use iso_c_binding
  implicit none

  type, bind (c) :: m
    real(C_DOUBLE) :: a(3, 3)
  end type

  interface
    subroutine ctest (a, b, initp) bind (c)
      use iso_c_binding
      import m
      type(m), allocatable :: a
      type(m), pointer :: b
      integer(C_INT), value :: initp
    end subroutine
  end interface

  type (m), allocatable, target :: aa
  type (m), pointer :: bb

  ! Test both before and after allocation/pointer initialization.
  bb => null()
  call ctest (aa, bb, 0)
  allocate (aa)
  bb => aa
  call ctest (aa, bb, 1)

end program
