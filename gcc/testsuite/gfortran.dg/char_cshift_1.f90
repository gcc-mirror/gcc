! Test cshift0 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 2, n2 = 3, n3 = 4, slen = 3
  character (len = slen), dimension (n1, n2, n3) :: a
  integer (kind = 1) :: shift1 = 3
  integer (kind = 2) :: shift2 = 4
  integer (kind = 4) :: shift3 = 5
  integer (kind = 8) :: shift4 = 6
  integer :: i1, i2, i3

  do i3 = 1, n3
    do i2 = 1, n2
      do i1 = 1, n1
        a (i1, i2, i3) = 'ab'(i1:i1) // 'cde'(i2:i2) // 'fghi'(i3:i3)
      end do
    end do
  end do

  call test (cshift (a, shift1, 1), int (shift1), 0, 0)
  call test (cshift (a, shift2, 2), 0, int (shift2), 0)
  call test (cshift (a, shift3, 3), 0, 0, int (shift3))
  call test (cshift (a, shift4, 3), 0, 0, int (shift4))
contains
  subroutine test (b, d1, d2, d3)
    character (len = slen), dimension (n1, n2, n3) :: b
    integer :: d1, d2, d3

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          if (b (i1, i2, i3) .ne. a (mod (d1 + i1 - 1, n1) + 1, &
                                     mod (d2 + i2 - 1, n2) + 1, &
                                     mod (d3 + i3 - 1, n3) + 1)) call abort
        end do
      end do
    end do
  end subroutine test
end program main
