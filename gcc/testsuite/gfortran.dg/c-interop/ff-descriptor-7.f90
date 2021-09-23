! { dg-do run }
!
! Test that arrays that may not be contiguous can be passed both ways
! between Fortran subroutines with C and Fortran binding conventions.

program testit
  use iso_c_binding
  implicit none

  integer(C_INT), target :: aa(10,5)
  integer(C_INT), target :: bb(10,10)

  integer :: i, j, n

  ! Test both C and Fortran binding.
  n = 0
  do j = 1, 10
    do i = 1, 5
      aa(j,i) = n
      n = n + 1
    end do
  end do
  call testc (transpose (aa))
  call testf (transpose (aa))

  bb = -1
  n = 0
  do j = 1, 10
    do i = 2, 10, 2
      bb(i,j) = n
      n = n + 1
    end do
  end do
  call testc (bb(2:10:2, :))
  call testf (bb(2:10:2, :))

contains

  subroutine testc (a) bind (c)
    use iso_c_binding
    integer(C_INT), intent(in) :: a(:,:)
    call checkc (a)
    call checkf (a)
  end subroutine

  subroutine testf (a)
    use iso_c_binding
    integer(C_INT), intent(in) :: a(:,:)
    call checkc (a)
    call checkf (a)
  end subroutine

  subroutine checkc (a) bind (c)
    use iso_c_binding
    integer(C_INT), intent(in) :: a(:,:)
    integer :: i, j, n

    if (rank (a) .ne. 2) stop 101
    if (size (a, 1) .ne. 5) stop 102
    if (size (a, 2) .ne. 10) stop 103

    n = 0
    do j = 1, 10
      do i = 1, 5
        if (a(i,j) .ne. n) stop 104
        n = n + 1
      end do
    end do
  end subroutine

  subroutine checkf (a)
    use iso_c_binding
    integer(C_INT), intent(in) :: a(:,:)
    integer :: i, j, n

    if (rank (a) .ne. 2) stop 101
    if (size (a, 1) .ne. 5) stop 102
    if (size (a, 2) .ne. 10) stop 103

    n = 0
    do j = 1, 10
      do i = 1, 5
        if (a(i,j) .ne. n) stop 104
        n = n + 1
      end do
    end do
  end subroutine

end program
