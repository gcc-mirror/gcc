! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-2-c.c dump-descriptors.c" }
!
! This program checks that calling a Fortran function with C binding and
! an intent(out) argument works from both C and Fortran.  For this
! test case the argument is an assumed-rank array.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

! The call chains we'll be testing will be
!   main -> ctest -> ftest1
!   main -> ftest2 -> ftest1
!   main -> ftest1
! where everything has "c" binding except main.

! ftest1 has C binding and transposes a into b.

subroutine ftest1 (a, b) bind (c, name="ftest1")
  use iso_c_binding
  use mm
  type(m) :: a(..)
  type(m), intent(out) ::  b(..)

  select rank (a)
    rank (2)
      select rank (b)
        rank (2)
          b = transpose (a)
        rank default
          stop 101
      end select
    rank default
      stop 102
  end select
end subroutine

! ftest2 has C binding and calls ftest1.

subroutine ftest2 (a, b) bind (c, name="ftest2")
  use iso_c_binding
  use mm
  type(m) :: a(..)
  type(m), intent(out) ::  b(..)

  interface
    subroutine ftest1 (a, b) bind (c, name="ftest1")
      use iso_c_binding
      use mm
      type(m) :: a(..)
      type(m), intent(out) ::  b(..)
    end subroutine
  end interface

  call ftest1 (a, b)
  if (rank (a) .ne. 2)  stop 201
  if (rank (b) .ne. 2)  stop 202
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
    subroutine ftest1 (a, b) bind (c, name="ftest2")
      use iso_c_binding
      use mm
      type(m) :: a(..)
      type(m), intent(out) ::  b(..)
    end subroutine
    subroutine ftest2 (a, b) bind (c, name="ftest2")
      use iso_c_binding
      use mm
      type(m) :: a(..)
      type(m), intent(out) ::  b(..)
    end subroutine
    subroutine ctest (a, b) bind (c, name="ctest")
      use iso_c_binding
      use mm
      type(m) :: a(..)
      type(m), intent(out) :: b(..)
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

  ! frob and check
  call ftest1 (aa, bb)
  do j = 1, jmax
    do i = 1, imax
      if (aa(i,j)%i .ne. bb(j,i)%i) stop 301
      if (aa(i,j)%j .ne. bb(j,i)%j) stop 302
    end do
  end do

  ! initialize again
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
      bb(j,i)%i = -1
      bb(j,i)%j = -2
    end do
  end do

  ! frob and check
  call ftest2 (aa, bb)
  do j = 1, jmax
    do i = 1, imax
      if (aa(i,j)%i .ne. bb(j,i)%i) stop 401
      if (aa(i,j)%j .ne. bb(j,i)%j) stop 402
    end do
  end do

  ! initialize again
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
      bb(j,i)%i = -1
      bb(j,i)%j = -2
    end do
  end do

  ! frob and check
  call ctest (aa, bb)
  do j = 1, jmax
    do i = 1, imax
      if (aa(i,j)%i .ne. bb(j,i)%i) stop 501
      if (aa(i,j)%j .ne. bb(j,i)%j) stop 502
    end do
  end do

end program
