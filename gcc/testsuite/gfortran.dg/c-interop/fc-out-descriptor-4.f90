! PR 92621 (?)
! { dg-do run }
! { dg-additional-sources "fc-out-descriptor-4-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that passing an allocatable or pointer array
! as an intent(out) argument to a C function called from Fortran works.  

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer(C_INT), parameter :: imin = 5, imax = 10, jmin = -10, jmax = -1

end module

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest1 (i0, ii, j0, jj, p) bind (c)
      use iso_c_binding
      use mm
      integer(C_INT), value :: i0, ii, j0, jj
      type(m), intent(out), pointer :: p(:,:)
    end subroutine
    subroutine ctest2 (i0, ii, j0, jj, a) bind (c)
      use iso_c_binding
      use mm
      integer(C_INT), value :: i0, ii, j0, jj
      type(m), intent(out), allocatable :: a(:,:)
    end subroutine
  end interface

  type(m), pointer :: p(:,:)
  type(m), allocatable :: a(:,:)
  integer :: i, j

  p => NULL ()
  call ctest1 (imin, imax, jmin, jmax, p)
  if (.not. associated (p)) stop 101
  if (rank(p) .ne. 2) stop 102
  if (lbound (p, 1) .ne. imin) stop 103
  if (ubound (p, 1) .ne. imax) stop 104
  if (lbound (p, 2) .ne. jmin) stop 105
  if (ubound (p, 2) .ne. jmax) stop 106
  do j = jmin, jmax
    do i = imin, imax
      if (p(i,j)%i .ne. i) stop 107
      if (p(i,j)%j .ne. j) stop 108
    end do
  end do

  ! The intent(out) argument is supposed to be deallocated automatically
  ! on entry to the called function.
  allocate (a (jmin:jmax,imin:imax))
  if (.not. allocated (a)) stop 201
  call ctest2 (imin, imax, jmin, jmax, a)
  if (.not. allocated (a)) stop 201
  if (rank(a) .ne. 2) stop 202
  if (lbound (a, 1) .ne. imin) stop 203
  if (ubound (a, 1) .ne. imax) stop 204
  if (lbound (a, 2) .ne. jmin) stop 205
  if (ubound (a, 2) .ne. jmax) stop 206
  do j = jmin, jmax
    do i = imin, imax
      if (a(i,j)%i .ne. i) stop 207
      if (a(i,j)%j .ne. j) stop 208
    end do
  end do
end program
