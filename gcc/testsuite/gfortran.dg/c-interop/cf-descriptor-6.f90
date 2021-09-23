! { dg-do run }
! { dg-additional-sources "cf-descriptor-6-c.c dump-descriptors.c" }
!
! This program tests passing the result of the CFI_section C library
! routine back to Fortran.  Most of the work happens on the C side.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
  integer, parameter :: ilb=2, jlb=1
  integer, parameter :: iub=8, jub=5
  integer, parameter :: istep=3, jstep=2
  integer, parameter :: isize=3, jsize=3
end module

subroutine ftest (b) bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m), pointer :: b(:,:)
  integer :: i, j, ii, jj

  if (size (b, 1) .ne. isize) stop 103
  if (size (b, 2) .ne. jsize) stop 104

  ! ii and jj iterate over the elements of b
  ! i and j iterate over the original array
  jj = lbound (b, 2)
  do j = jlb, jub, jstep
    ii = lbound (b, 1)
    do i = ilb, iub, istep
      if (b (ii, jj)%i .ne. i) stop 203
      if (b (ii, jj)%j .ne. j) stop 204
      ii = ii + 1
    end do
    jj = jj + 1
  end do
end subroutine


program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (a, lb1, lb2, ub1, ub2, step1, step2) bind (c)
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
      integer(C_INT), value :: lb1, lb2, ub1, ub2, step1, step2
    end subroutine
  end interface

  type(m), target :: aa(imax,jmax)
  integer :: i, j
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
    end do
  end do

  ! Pass the initialized array to a C function ctest, which will take
  ! a section of it and pass it to ftest.

  call ctest (aa, ilb, jlb, iub, jub, istep, jstep)

end program
