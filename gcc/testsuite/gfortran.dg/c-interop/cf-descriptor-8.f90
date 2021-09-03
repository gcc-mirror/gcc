! { dg-do run }
! { dg-additional-sources "cf-descriptor-8-c.c dump-descriptors.c" }
!
! This program tests passing the result of the CFI_setpointer C library 
! function back to Fortran.  Most of the  work happens on the C side.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

subroutine ftest1 (a, lb1, lb2) bind (c, name="ftest1")
  use iso_c_binding
  use mm
  type(m), pointer :: a(:,:)
  integer(C_INT), value :: lb1, lb2
  integer :: i, j, ii, jj

  if (size (a,1) .ne. imax) stop 101
  if (size (a,2) .ne. jmax) stop 102
  if (lbound (a, 1) .ne. lb1) stop 103
  if (lbound (a, 2) .ne. lb2) stop 104

  if (.not. associated (a)) stop 105

  jj = lb2
  do j = 1, jmax
    ii = lb1
    do i = 1, imax
      if (a(ii,jj)%i .ne. i) stop 201
      if (a(ii,jj)%j .ne. j) stop 202
      ii = ii + 1
    end do
  jj = jj + 1
  end do
end subroutine

subroutine ftest2 (a) bind (c, name="ftest2")
  use iso_c_binding
  use mm
  type(m), pointer :: a(:,:)

  if (associated (a)) stop 301
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

  ! Pass the initialized array to a C function ctest, which will use it
  ! as the target of a pointer array with various bounds, calling
  ! ftest1 and ftest2 to check that CFI_setpointer did the right thing.

  call ctest (aa)

end program
