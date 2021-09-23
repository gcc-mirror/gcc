! { dg-do run }
! { dg-additional-sources "fc-out-descriptor-2-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that passing a fixed-size array as an intent(out)
! assumed-rank argument to a C function called from Fortran works.  

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (ii, jj, a) bind (c)
      use iso_c_binding
      use mm
      integer(C_INT), value :: ii, jj
      type(m), intent(out) :: a(..)
    end subroutine
  end interface

  type(m) :: aa(imax,jmax)
  integer :: i, j

  ! initialize the array to all zeros; ctest will overwrite it.
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = 0
      aa(i,j)%j = 0
    end do
  end do    

  call ctest (imax, jmax, aa)
  call verify (aa)

contains
subroutine verify (a)
  use iso_c_binding
  use mm
  type(m) :: a(:,:)
  integer :: i, j

  if (rank (a) .ne. 2) stop 100
  if (lbound (a, 1) .ne. 1) stop 101
  if (lbound (a, 2) .ne. 1) stop 102
  if (ubound (a, 1) .ne. imax) stop 103
  if (ubound (a, 2) .ne. jmax) stop 104

  do j = 1, jmax
    do i = 1, imax
      if (a(i,j)%i .ne. i) stop 201
      if (a(i,j)%j .ne. j) stop 202
    end do
  end do
end subroutine

end program
