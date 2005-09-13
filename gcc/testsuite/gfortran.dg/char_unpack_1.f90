! Test unpack0 for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 3, n2 = 4, nv = 10, slen = 9
  character (len = slen), dimension (n1, n2) :: field
  character (len = slen), dimension (nv) :: vector
  logical, dimension (n1, n2) :: mask
  integer :: i1, i2, i

  do i2 = 1, n2
    do i1 = 1, n1
      field (i1, i2) = 'abc'(i1:i1) // 'defg'(i2:i2) // 'cantrip'
    end do
  end do
  mask (1, :) = (/ .true., .false., .true., .true. /)
  mask (2, :) = (/ .true., .false., .false., .false. /)
  mask (3, :) = (/ .false., .true., .true., .true. /)

  do i = 1, nv
    vector (i) = 'crespo' // '0123456789'(i:i)
  end do

  call test (unpack (vector, mask, field))
contains
  subroutine test (a)
    character (len = slen), dimension (:, :) :: a

    if (size (a, 1) .ne. n1) call abort
    if (size (a, 2) .ne. n2) call abort

    i = 0
    do i2 = 1, n2
      do i1 = 1, n1
        if (mask (i1, i2)) then
          i = i + 1
          if (a (i1, i2) .ne. vector (i)) call abort
        else
          if (a (i1, i2) .ne. field (i1, i2)) call abort
        end if
      end do
    end do
  end subroutine test
end program main
