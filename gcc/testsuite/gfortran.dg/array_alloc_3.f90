! Like array_alloc_1.f90, but check multi-dimensional arrays.
! { dg-do run }
program main
  implicit none
  call test ((/ 3, 4, 5 /), f ((/ 3, 4, 5 /)))
contains
  subroutine test (expected, x)
    integer, dimension (:,:,:) :: x
    integer, dimension (3) :: expected
    integer :: i, i1, i2, i3
    do i = 1, 3
      if (size (x, i) .ne. expected (i)) STOP 1
    end do
    do i1 = 1, expected (1)
      do i2 = 1, expected (2)
        do i3 = 1, expected (3)
          if (x (i1, i2, i3) .ne. i1 + i2 * 10 + i3 * 100) STOP 2
        end do
      end do
    end do
  end subroutine test

  function f (x)
    integer, dimension (3) :: x
    integer, dimension (x(1), x(2), x(3)) :: f
    integer :: i1, i2, i3
    do i1 = 1, x(1)
      do i2 = 1, x(2)
        do i3 = 1, x(3)
          f (i1, i2, i3) = i1 + i2 * 10 + i3 * 100
        end do
      end do
    end do
  end function f
end program main
