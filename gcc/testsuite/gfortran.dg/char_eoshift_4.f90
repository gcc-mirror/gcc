! Test eoshift3 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 2, n2 = 5, n3 = 4, slen = 3
  character (len = slen), dimension (n1, n2, n3) :: a
  character (len = slen), dimension (n1, n3) :: filler
  integer (kind = 1), dimension (n1, n3) :: shift1
  integer (kind = 2), dimension (n1, n3) :: shift2
  integer (kind = 4), dimension (n1, n3) :: shift3
  integer (kind = 8), dimension (n1, n3) :: shift4
  integer :: i1, i2, i3

  filler (1, :) = (/ 'tic', 'tac', 'toe', 'tip' /)
  filler (2, :) = (/ 'zzz', 'yyy', 'xxx', 'www' /)

  shift1 (1, :) = (/ 1, 3, 2, 2 /)
  shift1 (2, :) = (/ 2, 1, 1, 3 /)
  shift2 = shift1
  shift3 = shift1
  shift4 = shift1

  do i3 = 1, n3
    do i2 = 1, n2
      do i1 = 1, n1
        a (i1, i2, i3) = 'ab'(i1:i1) // 'cdefg'(i2:i2) // 'hijk'(i3:i3)
      end do
    end do
  end do

  call test (eoshift (a, shift1, filler, 2), .true.)
  call test (eoshift (a, shift2, filler, 2), .true.)
  call test (eoshift (a, shift3, filler, 2), .true.)
  call test (eoshift (a, shift4, filler, 2), .true.)

  call test (eoshift (a, shift1, dim = 2), .false.)
  call test (eoshift (a, shift2, dim = 2), .false.)
  call test (eoshift (a, shift3, dim = 2), .false.)
  call test (eoshift (a, shift4, dim = 2), .false.)
contains
  subroutine test (b, has_filler)
    character (len = slen), dimension (n1, n2, n3) :: b
    logical :: has_filler
    integer :: i2p

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          i2p = i2 + shift1 (i1, i3)
          if (i2p .le. n2) then
            if (b (i1, i2, i3) .ne. a (i1, i2p, i3)) STOP 1
          else if (has_filler) then
            if (b (i1, i2, i3) .ne. filler (i1, i3)) STOP 2
          else
            if (b (i1, i2, i3) .ne. '') STOP 3
          end if
        end do
      end do
    end do
  end subroutine test
end program main
