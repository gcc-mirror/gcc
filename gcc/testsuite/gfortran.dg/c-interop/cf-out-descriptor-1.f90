! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-1-c.c dump-descriptors.c" }
!
! This program checks that calling a Fortran function with C binding and
! an intent(out) argument works from both C and Fortran.  For this
! test case the argument is an assumed-shape array.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

! frob has regular Fortran binding.  It transposes input array argument
! a into the intent(out) argument b.

subroutine frob (a, b)
  use iso_c_binding
  use mm
  type(m) :: a(:,:)
  type(m), intent(out) ::  b(:,:)
  integer :: i, j

  if (lbound (a, 1) .ne. lbound (b, 2)) stop 101
  if (lbound (a, 2) .ne. lbound (b, 1)) stop 102
  if (ubound (a, 1) .ne. ubound (b, 2)) stop 103
  if (ubound (a, 2) .ne. ubound (b, 1)) stop 104

  do j = lbound (a, 2), ubound (a, 2)
    do i = lbound (a, 1), ubound (a, 1)
      b(j,i) = a(i,j)
    end do
  end do
end subroutine

! check also has regular Fortran binding, and two input arguments.

subroutine check (a, b)
  use iso_c_binding
  use mm
  type(m) :: a(:,:), b(:,:)
  integer :: i, j

  if (lbound (a, 1) .ne. 1 .or. lbound (b, 2) .ne. 1) stop 101
  if (lbound (a, 2) .ne. 1 .or. lbound (b, 1) .ne. 1) stop 102
  if (ubound (a, 1) .ne. ubound (b, 2)) stop 103
  if (ubound (a, 2) .ne. ubound (b, 1)) stop 104

  do j = 1, ubound (a, 2)
    do i = 1, ubound (a, 1)
      if (b(j,i)%i .ne. a(i,j)%i)  stop 105
      if (b(j,i)%j .ne. a(i,j)%j)  stop 106
    end do
  end do
end subroutine

! ftest1 has C binding and calls frob.  This allows us to test intent(out)
! arguments passed back from Fortran binding to a Fortran function with C
! binding.

subroutine ftest1 (a, b) bind (c, name="ftest1")
  use iso_c_binding
  use mm
  type(m) :: a(:,:)
  type(m), intent(out) ::  b(:,:)

  interface
    subroutine frob (a, b)
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
      type(m), intent(out) ::  b(:,:)
    end subroutine
    subroutine check (a, b)
      use iso_c_binding
      use mm
      type(m) :: a(:,:), b(:,:)
    end subroutine
  end interface

  call frob (a, b)
  call check (a, b)
end subroutine

! ftest2 has C binding and calls ftest1.  This allows us to test intent(out)
! arguments passed between two Fortran functions with C binding.

subroutine ftest2 (a, b) bind (c, name="ftest2")
  use iso_c_binding
  use mm
  type(m) :: a(:,:)
  type(m), intent(out) ::  b(:,:)

  interface
    subroutine ftest1 (a, b) bind (c, name="ftest1")
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
      type(m), intent(out) ::  b(:,:)
    end subroutine
    subroutine check (a, b)
      use iso_c_binding
      use mm
      type(m) :: a(:,:), b(:,:)
    end subroutine
  end interface

  call ftest1 (a, b)
  call check (a, b)
end subroutine

! main calls ftest2 directly and also indirectly from a C function ctest.
! The former allows us to test intent(out) arguments passed back from a
! Fortran routine with C binding to a regular Fortran routine, and the
! latter tests passing them back from Fortran to C and C to Fortran.

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ftest2 (a, b) bind (c, name="ftest2")
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
      type(m), intent(out) ::  b(:,:)
    end subroutine
    subroutine ctest (a, b) bind (c)
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
      type(m), intent(out) :: b(:,:)
    end subroutine
    subroutine check (a, b)
      use iso_c_binding
      use mm
      type(m) :: a(:,:), b(:,:)
    end subroutine
  end interface

  type(m) :: aa(imax,jmax), bb(jmax,imax)
  integer :: i, j

  ! initialize
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
      bb(j,i)%i = -1
      bb(j,i)%j = -2
    end do
  end do

  call ftest2 (aa, bb)
  call check (aa, bb)

  ! initialize again
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
      bb(j,i)%i = -1
      bb(j,i)%j = -2
    end do
  end do

  call ctest (aa, bb)
  call check (aa, bb)

end program
