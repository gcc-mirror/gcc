! { dg-do run }
! { dg-additional-sources "cf-descriptor-7-c.c dump-descriptors.c" }
!
! This program tests passing the result of the CFI_select_part C library 
! routine back to Fortran.  Most of the work happens on the C side.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

subroutine ftest (iarray, jarray) bind (c, name="ftest")
  use iso_c_binding
  use mm
  integer(C_INT), pointer :: iarray(:,:), jarray(:,:)
  
  integer :: i, j, i1, i2, j1, j2

  ! iarray and jarray must have the same shape as the original array,
  ! but might be zero-indexed instead of one-indexed.
  if (size (iarray, 1) .ne. imax) stop 101
  if (size (iarray, 2) .ne. jmax) stop 102
  if (size (jarray, 1) .ne. imax) stop 103
  if (size (jarray, 2) .ne. jmax) stop 104

  j1 = lbound(iarray, 2)
  j2 = lbound(jarray, 2)
  do j = 1, jmax
    i1 = lbound(iarray, 1)
    i2 = lbound(jarray, 1)
    do i = 1, imax
      if (iarray (i1, j1) .ne. i) stop 201
      if (jarray (i2, j2) .ne. j) stop 202
      i1 = i1 + 1
      i2 = i2 + 1
    end do
  j1 = j1 + 1
  j2 = j2 + 1
  end do
end subroutine


program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      use mm
      type(m) :: a(:,:)
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

  ! Pass the initialized array to a C function ctest, which will split it
  ! into i and j component arrays and pass them to ftest.

  call ctest (aa)

end program
