! { dg-do run }
!
! This program checks that passing arrays as assumed-shape dummies to
! and from Fortran functions with C binding works.

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

  type(m) :: aa(imax,jmax)
  integer :: i, j
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
    end do
  end do

  call testc (aa)
  call testf (aa)

contains

  ! C binding version

  subroutine checkc (a, b) bind (c)
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
        if (a(i,j)%i .ne. i) stop 105
        if (a(i,j)%j .ne. j) stop 106
        if (b(j,i)%i .ne. i) stop 107
        if (b(j,i)%j .ne. j) stop 108
      end do
    end do
  end subroutine

  ! Fortran binding version
  subroutine checkf (a, b)
    use iso_c_binding
    use mm
    type(m) :: a(:,:), b(:,:)
    integer :: i, j

    if (size (a,1) .ne. imax) stop 201
    if (size (a,2) .ne. jmax) stop 202
    if (size (b,1) .ne. jmax) stop 203
    if (size (b,2) .ne. imax) stop 204

    do j = 1, jmax
      do i = 1, imax
        if (a(i,j)%i .ne. i) stop 205
        if (a(i,j)%j .ne. j) stop 206
        if (b(j,i)%i .ne. i) stop 207
        if (b(j,i)%j .ne. j) stop 208
      end do
    end do
  end subroutine

  ! C binding version
  subroutine testc (a) bind (c)
    use iso_c_binding
    use mm
    type(m) :: a(:,:)
    type(m) :: b(jmax, imax)
    integer :: i, j

    if (size (a,1) .ne. imax) stop 301
    if (size (a,2) .ne. jmax) stop 302
    do j = 1, jmax
      do i = 1, imax
        b(j,i)%i  = a(i,j)%i
        b(j,i)%j  = a(i,j)%j
      end do
    end do

    ! Call both the C and Fortran binding check functions
    call checkc (a, b)
    call checkf (a, b)
  end subroutine

  ! Fortran binding version
  subroutine testf (a)
    use iso_c_binding
    use mm
    type(m) :: a(:,:)
    type(m) :: b(jmax, imax)
    integer :: i, j

    if (size (a,1) .ne. imax) stop 401
    if (size (a,2) .ne. jmax) stop 402
    do j = 1, jmax
      do i = 1, imax
        b(j,i)%i  = a(i,j)%i
        b(j,i)%j  = a(i,j)%j
      end do
    end do

    ! Call both the C and Fortran binding check functions
    call checkc (a, b)
    call checkf (a, b)
  end subroutine

end program
