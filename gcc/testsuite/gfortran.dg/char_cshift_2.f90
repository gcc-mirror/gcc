! Test cshift1 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 2, n2 = 3, n3 = 4, slen = 3
  character (len = slen), dimension (n1, n2, n3) :: a
  integer (kind = 1), dimension (2, 4) :: shift1
  integer (kind = 2), dimension (2, 4) :: shift2
  integer (kind = 4), dimension (2, 4) :: shift3
  integer (kind = 8), dimension (2, 4) :: shift4
  integer :: i1, i2, i3

  do i3 = 1, n3
    do i2 = 1, n2
      do i1 = 1, n1
        a (i1, i2, i3) = 'ab'(i1:i1) // 'cde'(i2:i2) // 'fghi'(i3:i3)
      end do
    end do
  end do

  shift1 (1, :) = (/ 4, 11, 19, 20 /)
  shift1 (2, :) = (/ 55, 5, 1, 2 /)
  shift2 = shift1
  shift3 = shift1
  shift4 = shift1

  call test (cshift (a, shift1, 2))
  call test (cshift (a, shift2, 2))
  call test (cshift (a, shift3, 2))
  call test (cshift (a, shift4, 2))
contains
  subroutine test (b)
    character (len = slen), dimension (n1, n2, n3) :: b
    integer :: i2p

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          i2p = mod (shift1 (i1, i3) + i2 - 1, n2) + 1
          if (b (i1, i2, i3) .ne. a (i1, i2p, i3)) STOP 1
        end do
      end do
    end do
  end subroutine test
end program main
