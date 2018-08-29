! Like array_constructor_6.f90, but check integer(8) iterators.
! { dg-do run }
program main
  integer (kind = 8) :: i, l8, u8, step8
  integer (kind = 4) :: l4, step4
  integer (kind = 8), parameter :: big = 10000000000_8

  l4 = huge (l4)
  u8 = l4 + 10_8
  step4 = 2
  call test ((/ (i, i = l4, u8, step4) /), l4 + 0_8, u8, step4 + 0_8)

  l8 = big
  u8 = big * 20
  step8 = big
  call test ((/ (i, i = l8, u8, step8) /), l8, u8, step8)

  u8 = big + 100
  l8 = big
  step4 = -20
  call test ((/ (i, i = u8, l8, step4) /), u8, l8, step4 + 0_8)

  u8 = big * 40
  l8 = big * 20
  step8 = -big * 2
  call test ((/ (i, i = u8, l8, step8) /), u8, l8, step8)

  u8 = big
  l4 = big / 100
  step4 = -big / 500
  call test ((/ (i, i = u8, l4, step4) /), u8, l4 + 0_8, step4 + 0_8)

  u8 = big * 40 + 200
  l4 = 200
  step8 = -big
  call test ((/ (i, i = u8, l4, step8) /), u8, l4 + 0_8, step8)
contains
  subroutine test (a, l, u, step)
    integer (kind = 8), dimension (:), intent (in) :: a
    integer (kind = 8), intent (in) :: l, u, step
    integer (kind = 8) :: i
    integer :: j

    j = 1
    do i = l, u, step
      if (a (j) .ne. i) STOP 1
      j = j + 1
    end do
    if (size (a, 1) .ne. j - 1) STOP 2
  end subroutine test
end program main
