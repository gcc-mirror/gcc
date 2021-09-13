! { dg-do run }
! { dg-additional-sources "cf-descriptor-1-c.c dump-descriptors.c" }
!
! This program checks that building a descriptor for a fixed-size array
! in C works and that you can use it to call back into a Fortran function
! declared to have c binding, as an assumed-shape argument.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

subroutine ftest (a, b) bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m) :: a(:,:), b(:,:)
  integer :: i, j

  if (size (a,1) .ne. imax) stop 101
  if (size (a,2) .ne. jmax) stop 102
  if (size (b,1) .ne. jmax) stop 103
  if (size (b,2) .ne. imax) stop 104

  do j = 1, jmax
    do i = 1, imax
      if (a(i,j)%i .ne. i) stop 201
      if (a(i,j)%j .ne. j) stop 202
      if (b(j,i)%i .ne. i) stop 203
      if (b(j,i)%j .ne. j) stop 204
    end do
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

  type(m) :: aa(imax,jmax)
  integer :: i, j
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
    end do
  end do

  ! Pass the initialized array to a C function ctest, which will generate its
  ! transpose and call ftest with it.

  call ctest (aa)

end program
