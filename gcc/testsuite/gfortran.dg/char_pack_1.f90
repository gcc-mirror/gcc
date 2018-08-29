! Test (non-scalar) pack for character arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n1 = 3, n2 = 4, nv = 10, slen = 9
  character (len = slen), dimension (n1, n2) :: a
  character (len = slen), dimension (nv) :: vector
  logical, dimension (n1, n2) :: mask
  integer :: i1, i2, i

  do i2 = 1, n2
    do i1 = 1, n1
      a (i1, i2) = 'abc'(i1:i1) // 'defg'(i2:i2) // 'cantrip'
    end do
  end do
  mask (1, :) = (/ .true., .false., .true., .true. /)
  mask (2, :) = (/ .true., .false., .false., .false. /)
  mask (3, :) = (/ .false., .true., .true., .true. /)

  do i = 1, nv
    vector (i) = 'crespo' // '0123456789'(i:i)
  end do

  call test1 (pack (a, mask))
  call test2 (pack (a, mask, vector))
contains
  subroutine test1 (b)
    character (len = slen), dimension (:) :: b

    i = 0
    do i2 = 1, n2
      do i1 = 1, n1
        if (mask (i1, i2)) then
          i = i + 1
          if (b (i) .ne. a (i1, i2)) STOP 1
        end if
      end do
    end do
    if (size (b, 1) .ne. i) STOP 2
  end subroutine test1

  subroutine test2 (b)
    character (len = slen), dimension (:) :: b

    if (size (b, 1) .ne. nv) STOP 3
    i = 0
    do i2 = 1, n2
      do i1 = 1, n1
        if (mask (i1, i2)) then
          i = i + 1
          if (b (i) .ne. a (i1, i2)) STOP 4
        end if
      end do
    end do
    do i = i + 1, nv
      if (b (i) .ne. vector (i)) STOP 5
    end do
  end subroutine test2
end program main
