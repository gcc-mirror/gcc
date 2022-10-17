! { dg-do run }
!
! This program checks that passing arrays as assumed-rank dummies to
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
    type(m) :: a(..), b(..)

    if (rank (a) .ne. 2) stop 101
    if (rank (b) .ne. 2) stop 102
    if (size (a,1) .ne. imax) stop 103
    if (size (a,2) .ne. jmax) stop 104
    if (size (b,1) .ne. jmax) stop 105
    if (size (b,2) .ne. imax) stop 106

  end subroutine

  ! Fortran binding version
  subroutine checkf (a, b)
    use iso_c_binding
    use mm
    type(m) :: a(..), b(..)

    if (rank (a) .ne. 2) stop 201
    if (rank (b) .ne. 2) stop 202
    if (size (a,1) .ne. imax) stop 203
    if (size (a,2) .ne. jmax) stop 204
    if (size (b,1) .ne. jmax) stop 205
    if (size (b,2) .ne. imax) stop 206

  end subroutine

  ! C binding version
  subroutine testc (a) bind (c)
    use iso_c_binding
    use mm
    type(m) :: a(..)
    type(m) :: b(jmax, imax)

    if (rank (a) .ne. 2) stop 301
    if (size (a,1) .ne. imax) stop 302
    if (size (a,2) .ne. jmax) stop 303

    ! Call both the C and Fortran binding check functions
    call checkc (a, b)
    call checkf (a, b)
  end subroutine

  ! Fortran binding version
  subroutine testf (a)
    use iso_c_binding
    use mm
    type(m) :: a(..)
    type(m) :: b(jmax, imax)

    if (rank (a) .ne. 2) stop 401
    if (size (a,1) .ne. imax) stop 402
    if (size (a,2) .ne. jmax) stop 403

    ! Call both the C and Fortran binding check functions
    call checkc (a, b)
    call checkf (a, b)
  end subroutine

end program
