! { dg-do run }
! { dg-additional-sources "contiguous-1-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! TS 29113
! 8.7 In an invocation of an interoperable procedure whose Fortran
! interface has an assumed-shape or assumed-rank dummy argument with the
! CONTIGUOUS attribute, the associated effective argument may be an
! array that is not contiguous or the address of a C descriptor for such
! an array. If the procedure is invoked from Fortran or the procedure is
! a Fortran procedure, the Fortran processor will handle the difference
! in contiguity. If the procedure is invoked from C and the procedure is
! a C procedure, the C code within the procedure shall be prepared to
! handle the situation of receiving a discontiguous argument.
!
! This program tests the cases where Fortran code passes a non-contiguous
! array section to a C function whose interface has the contiguous
! attribute.

program testit
  use iso_c_binding
  implicit none

  interface
    ! ctest1 and ctest2 both negate the elements of their input array.
    subroutine ctest1 (a) bind (c)
      use iso_c_binding
      integer(C_INT), contiguous :: a(:)
    end subroutine
    subroutine ctest2 (a) bind (c)
      use iso_c_binding
      integer(C_INT), contiguous :: a(..)
    end subroutine
  end interface

  integer(C_INT) :: aa(32)
  integer :: i

  ! assumed-shape
  do i = 1, 32
    aa(i) = i
  end do
  call ctest1 (aa(4:12:2))
  do i = 1, 32
    if (i .ge. 4 .and. i .le. 12 .and. mod (i-4,2) .eq. 0) then
      if (aa (i) .ne. -i) stop 101
    else
      if (aa (i) .ne. i) stop 102
    end if
  end do

  ! assumed-rank
  do i = 1, 32
    aa(i) = i
  end do
  call ctest2 (aa(7:19:3))
  do i = 1, 32
    if (i .ge. 7 .and. i .le. 19 .and. mod (i-7,3) .eq. 0) then
      if (aa (i) .ne. -i) stop 201
    else
      if (aa (i) .ne. i) stop 202
    end if
  end do
    
end program


