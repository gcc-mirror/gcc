! Test reshape for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n = 20, slen = 9
  character (len = slen), dimension (n) :: a, pad
  integer, dimension (3) :: shape, order
  integer :: i

  do i = 1, n
    a (i) = 'abcdefghijklmnopqrstuvwxyz'(i:i+6)
    pad (i) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'(i:i+6)
  end do

  shape = (/ 4, 6, 5 /)
  order = (/ 3, 1, 2 /)
  call test (reshape (a, shape, pad, order))
contains
  subroutine test (b)
    character (len = slen), dimension (:, :, :) :: b
    integer :: i1, i2, i3, ai, padi

    do i = 1, 3
      if (size (b, i) .ne. shape (i)) STOP 1
    end do
    ai = 0
    padi = 0
    do i2 = 1, shape (2)
      do i1 = 1, shape (1)
        do i3 = 1, shape (3)
          if (ai .lt. n) then
            ai = ai + 1
            if (b (i1, i2, i3) .ne. a (ai)) STOP 2
          else
            padi = padi + 1
            if (padi .gt. n) padi = 1
            if (b (i1, i2, i3) .ne. pad (padi)) STOP 3
          end if
        end do
      end do
    end do
  end subroutine test
end program main
