! { dg-do compile }
!
! TS 29113
! 5.3 ALLOCATABLE, OPTIONAL, and POINTER attributes
! The ALLOCATABLE, OPTIONAL, and POINTER attributes may be specified
! for a dummy argument in a procedure interface that has the BIND
! attribute.

subroutine test (a, b, c)
  integer, allocatable :: a
  integer, optional :: b
  integer, pointer :: c

  interface
    subroutine ctest (aa, bb, cc) bind (c)
      integer, allocatable :: aa
      integer, optional :: bb
      integer, pointer :: cc
    end subroutine
  end interface

  call ctest (a, b, c)
end subroutine
