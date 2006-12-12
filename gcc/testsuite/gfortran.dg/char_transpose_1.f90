! Test transpose for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 3, n2 = 4, slen = 9
  character (len = slen), dimension (n1, n2) :: a
  integer :: i1, i2

  do i2 = 1, n2
    do i1 = 1, n1
      a (i1, i2) = 'abc'(i1:i1) // 'defg'(i2:i2) // 'cantrip'
    end do
  end do

  call test (transpose (a))
contains
  subroutine test (b)
    character (len = slen), dimension (:, :) :: b

    if (size (b, 1) .ne. n2) call abort
    if (size (b, 2) .ne. n1) call abort

    do i2 = 1, n2
      do i1 = 1, n1
        if (b (i2, i1) .ne. a (i1, i2)) call abort
      end do
    end do
  end subroutine test
end program main
