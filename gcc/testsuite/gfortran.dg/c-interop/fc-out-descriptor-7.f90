! PR 101310
! { dg-do run }
! { dg-additional-sources "fc-out-descriptor-7-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that returning a noncontiguous array as an intent(out)
! argument to a C function called from Fortran works.  

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer(C_INT), parameter :: imax = 10, jmax=5

end module

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    ! ctest points b at a section of array a defined by the
    ! indicated bounds and steps.  The returned array is 1-based.
    subroutine ctest (a, lb1, ub1, s1, lb2, ub2, s2, b) bind (c)
      use iso_c_binding
      use mm
      type(m), target :: a(:,:)
      integer(C_INT), value :: lb1, ub1, s1, lb2, ub2, s2
      type(m), intent(out), pointer :: b(:,:)
    end subroutine
  end interface

  type(m), target :: a(imax, jmax)
  type(m), pointer :: b(:,:)
  integer :: i, j, ii, jj

  do j = 1, jmax
    do i = 1, imax
      a(i,j)%i = i
      a(i,j)%j = j
    end do
  end do

  b => NULL ()
  ! resulting array is 1-based and has shape (3,3)
  call ctest (a, 2, 8, 3, 1, 5, 2, b)
  if (.not. associated (b)) stop 101
  if (rank(b) .ne. 2) stop 102
  if (lbound (b, 1) .ne. 1) stop 103
  if (ubound (b, 1) .ne. 3) stop 104
  if (lbound (b, 2) .ne. 1) stop 105
  if (ubound (b, 2) .ne. 3) stop 106

  ! check that the returned array b contains the expected elements
  ! from array a.
  jj = lbound (b, 2)
  do j = 1, 5, 2
    ii = lbound (b, 1)
    do i = 2, 8, 3
      if (b(ii,jj)%i .ne. i) stop 107
      if (b(ii,jj)%j .ne. j) stop 108
      ii = ii + 1
    end do
    jj = jj + 1
  end do

end program

