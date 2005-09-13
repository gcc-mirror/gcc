! Test eoshift2 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 2, n2 = 5, n3 = 4, slen = 3
  character (len = slen), dimension (n1, n2, n3) :: a
  character (len = slen), dimension (n1, n3) :: filler
  integer (kind = 1) :: shift1 = 4
  integer (kind = 2) :: shift2 = 2
  integer (kind = 4) :: shift3 = 3
  integer (kind = 8) :: shift4 = 1
  integer :: i1, i2, i3

  filler (1, :) = (/ 'tic', 'tac', 'toe', 'tip' /)
  filler (2, :) = (/ 'zzz', 'yyy', 'xxx', 'www' /)

  do i3 = 1, n3
    do i2 = 1, n2
      do i1 = 1, n1
        a (i1, i2, i3) = 'ab'(i1:i1) // 'cdefg'(i2:i2) // 'hijk'(i3:i3)
      end do
    end do
  end do

  call test (eoshift (a, shift1, filler, 2), int (shift1), .true.)
  call test (eoshift (a, shift2, filler, 2), int (shift2), .true.)
  call test (eoshift (a, shift3, filler, 2), int (shift3), .true.)
  call test (eoshift (a, shift4, filler, 2), int (shift4), .true.)

  call test (eoshift (a, shift1, dim = 2), int (shift1), .false.)
  call test (eoshift (a, shift2, dim = 2), int (shift2), .false.)
  call test (eoshift (a, shift3, dim = 2), int (shift3), .false.)
  call test (eoshift (a, shift4, dim = 2), int (shift4), .false.)
contains
  subroutine test (b, d2, has_filler)
    character (len = slen), dimension (n1, n2, n3) :: b
    logical :: has_filler
    integer :: d2

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          if (i2 + d2 .le. n2) then
            if (b (i1, i2, i3) .ne. a (i1, i2 + d2, i3)) call abort
          else if (has_filler) then
            if (b (i1, i2, i3) .ne. filler (i1, i3)) call abort
          else
            if (b (i1, i2, i3) .ne. '') call abort
          end if
        end do
      end do
    end do
  end subroutine test
end program main
