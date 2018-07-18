! Test eoshift0 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 6, n2 = 5, n3 = 4, slen = 3
  character (len = slen), dimension (n1, n2, n3) :: a
  character (len = slen) :: filler
  integer (kind = 1) :: shift1 = 4
  integer (kind = 2) :: shift2 = 2
  integer (kind = 4) :: shift3 = 3
  integer (kind = 8) :: shift4 = 1
  integer :: i1, i2, i3

  do i3 = 1, n3
    do i2 = 1, n2
      do i1 = 1, n1
        a (i1, i2, i3) = 'abcdef'(i1:i1) // 'ghijk'(i2:i2) // 'lmno'(i3:i3)
      end do
    end do
  end do

  call test (eoshift (a, shift1, 'foo', 1), int (shift1), 0, 0, 'foo')
  call test (eoshift (a, shift2, 'foo', 2), 0, int (shift2), 0, 'foo')
  call test (eoshift (a, shift3, 'foo', 2), 0, int (shift3), 0, 'foo')
  call test (eoshift (a, shift4, 'foo', 3), 0, 0, int (shift4), 'foo')

  filler = ''
  call test (eoshift (a, shift1, dim = 1), int (shift1), 0, 0, filler)
  call test (eoshift (a, shift2, dim = 2), 0, int (shift2), 0, filler)
  call test (eoshift (a, shift3, dim = 2), 0, int (shift3), 0, filler)
  call test (eoshift (a, shift4, dim = 3), 0, 0, int (shift4), filler)
contains
  subroutine test (b, d1, d2, d3, filler)
    character (len = slen), dimension (n1, n2, n3) :: b
    character (len = slen) :: filler
    integer :: d1, d2, d3

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          if (i1 + d1 .gt. n1 .or. i2 + d2 .gt. n2 .or. i3 + d3 .gt. n3) then
            if (b (i1, i2, i3) .ne. filler) STOP 1
          else
            if (b (i1, i2, i3) .ne. a (i1 + d1, i2 + d2, i3 + d3)) STOP 2
          end if
        end do
      end do
    end do
  end subroutine test
end program main
