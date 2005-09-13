! Test spread for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 3, n2 = 10, n3 = 4, slen = 9
  character (len = slen), dimension (n1, n3) :: a
  integer :: i1, i2, i3

  do i3 = 1, n3
    do i1 = 1, n1
      a (i1, i3) = 'ab'(i1:i1) // 'cde'(i3:i3) // 'cantrip'
    end do
  end do

  call test (spread (a, 2, n2))
contains
  subroutine test (b)
    character (len = slen), dimension (:, :, :) :: b

    if (size (b, 1) .ne. n1) call abort
    if (size (b, 2) .ne. n2) call abort
    if (size (b, 3) .ne. n3) call abort

    do i3 = 1, n3
      do i2 = 1, n2
        do i1 = 1, n1
          if (b (i1, i2, i3) .ne. a (i1, i3)) call abort
        end do
      end do
    end do
  end subroutine test
end program main
