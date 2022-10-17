! PR 92621 (?)
! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-4-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that calling a Fortran function with C binding and
! an intent(out) argument works from both C and Fortran.  For this
! test case the argument is an allocatable or pointer array.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=5, jmax=10

end module

! The call chains being tested here are
!   main -> frob
!   main -> ftest -> frob
!   main -> ctest -> frob
! where everything other than main has C binding.

! frob allocates and initializes its arguments.
! There are two allocatable dummies so that we can pass both
! unallocated (a) and allocated (aa).

subroutine frob (a, aa, p) bind (c, name="frob")
  use iso_c_binding
  use mm
  type(m), intent(out), allocatable :: a(:,:), aa(:,:)
  type(m), intent(out), pointer :: p(:,:)
  integer :: i, j

  if (allocated (a))  stop 101
  allocate (a (imax, jmax))
  do j = 1, jmax
    do i = 1, imax
      a(i,j)%i = i
      a(i,j)%j = j
    end do
  end do

  if (allocated (aa))  stop 102
  allocate (aa (imax, jmax))
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
    end do
  end do

  allocate (p (jmax, imax))
  do j = 1, jmax
    do i = 1, imax
      p(j,i)%i = i
      p(j,i)%j = j
    end do
  end do
end subroutine

subroutine ftest () bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m), allocatable :: a(:,:), aa(:,:)
  type(m), pointer :: p(:,:)

  integer :: i, j

  interface
    subroutine frob (a, aa, p) bind (c, name="frob")
      use iso_c_binding
      use mm
      type(m), intent(out), allocatable :: a(:,:), aa(:,:)
      type(m), intent(out), pointer :: p(:,:)
    end subroutine
  end interface

  p => NULL ()
  if (allocated (a) .or. allocated (aa)) stop 200
  allocate (aa (jmax, imax))
  do j = 1, jmax
    do i = 1, imax
      aa(j,i)%i = 0
      aa(j,i)%j = 0
    end do
  end do
  call frob (a, aa, p)

  if (.not. allocated (a)) stop 201
  if (lbound (a, 1) .ne. 1) stop 202
  if (lbound (a, 2) .ne. 1) stop 203
  if (ubound (a, 1) .ne. imax) stop 204
  if (ubound (a, 2) .ne. jmax) stop 205
  do j = 1, jmax
    do i = 1, imax
      if (a(i,j)%i .ne. i) stop 206
      if (a(i,j)%j .ne. j) stop 207
    end do
  end do

  if (.not. allocated (aa)) stop 211
  if (lbound (aa, 1) .ne. 1) stop 212
  if (lbound (aa, 2) .ne. 1) stop 213
  if (ubound (aa, 1) .ne. imax) stop 214
  if (ubound (aa, 2) .ne. jmax) stop 215
  do j = 1, jmax
    do i = 1, imax
      if (aa(i,j)%i .ne. i) stop 216
      if (aa(i,j)%j .ne. j) stop 217
    end do
  end do

  if (.not. associated (p)) stop 221
  if (lbound (p, 1) .ne. 1) stop 222
  if (lbound (p, 2) .ne. 1) stop 223
  if (ubound (p, 1) .ne. jmax) stop 224
  if (ubound (p, 2) .ne. imax) stop 225
  do j = 1, jmax
    do i = 1, imax
      if (p(j,i)%i .ne. i) stop 226
      if (p(j,i)%j .ne. j) stop 227
    end do
  end do

end subroutine

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine frob (a, aa, p) bind (c, name="frob")
      use iso_c_binding
      use mm
      type(m), intent(out), allocatable :: a(:,:), aa(:,:)
      type(m), intent(out), pointer :: p(:,:)
    end subroutine
    subroutine ftest () bind (c, name="ftest")
      use iso_c_binding
      use mm
    end subroutine
    subroutine ctest (ii, jj) bind (c, name="ctest")
      use iso_c_binding
      use mm
      integer(C_INT), value :: ii, jj
    end subroutine
  end interface

  type(m), allocatable :: a(:,:), aa(:,:)
  type(m), pointer :: p(:,:)
  integer :: i, j

  p => NULL ()
  if (allocated (a) .or. allocated (aa)) stop 300
  allocate (aa (jmax, imax))
  do j = 1, jmax
    do i = 1, imax
      aa(j,i)%i = 0
      aa(j,i)%j = 0
    end do
  end do
  call frob (a, aa, p)

  if (.not. allocated (a)) stop 301
  if (lbound (a, 1) .ne. 1) stop 302
  if (lbound (a, 2) .ne. 1) stop 303
  if (ubound (a, 1) .ne. imax) stop 304
  if (ubound (a, 2) .ne. jmax) stop 305
  do j = 1, jmax
    do i = 1, imax
      if (a(i,j)%i .ne. i) stop 306
      if (a(i,j)%j .ne. j) stop 307
    end do
  end do

  if (.not. allocated (aa)) stop 311
  if (lbound (aa, 1) .ne. 1) stop 312
  if (lbound (aa, 2) .ne. 1) stop 313
  if (ubound (aa, 1) .ne. imax) stop 314
  if (ubound (aa, 2) .ne. jmax) stop 315
  do j = 1, jmax
    do i = 1, imax
      if (aa(i,j)%i .ne. i) stop 316
      if (aa(i,j)%j .ne. j) stop 317
    end do
  end do

  if (.not. associated (p)) stop 321
  if (lbound (p, 1) .ne. 1) stop 322
  if (lbound (p, 2) .ne. 1) stop 323
  if (ubound (p, 1) .ne. jmax) stop 324
  if (ubound (p, 2) .ne. imax) stop 325
  do j = 1, jmax
    do i = 1, imax
      if (p(j,i)%i .ne. i) stop 326
      if (p(j,i)%j .ne. j) stop 327
    end do
  end do

  call ftest
  call ctest (imax, jmax)

end program
