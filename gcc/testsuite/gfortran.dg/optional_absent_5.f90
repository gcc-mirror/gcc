! { dg-do run }
! PR 82995 - segfault passing on an optional argument;
! this tests the library versions.
module z
  implicit none
contains
  subroutine sum_1 (input, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = sum (input, dim=1, mask=mask)
  end subroutine sum_1

  subroutine sum_2 (input, res, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = sum (input, dim=1, mask=mask)
  end subroutine sum_2

  subroutine maxloc_1 (input, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = maxloc (input, dim=1, mask=mask)
  end subroutine maxloc_1

  subroutine minloc_1 (input, res, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = minloc (input, dim=1, mask=mask)
  end subroutine minloc_1

  subroutine maxloc_2 (input, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer :: n
    n = 1
    res = maxloc (input, dim=n, mask=mask)
  end subroutine maxloc_2

  subroutine findloc_1 (input, val, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer, intent(in) :: val
    res = findloc(input, val)
  end subroutine findloc_1

  subroutine findloc_2 (input, val, res, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer, intent(in) :: val
    res = findloc(input, val)
  end subroutine findloc_2

  subroutine findloc_3 (input, val, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer, intent(in) :: val
    res = findloc(input, val, dim=1)
  end subroutine findloc_3

  subroutine findloc_4 (input, val, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer, intent(in) :: val
    integer :: n = 1
    res = findloc(input, val, dim=n)
  end subroutine findloc_4

  subroutine maxval_1 (input, res, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = maxval (input, dim=1, mask=mask)
  end subroutine maxval_1

  subroutine maxval_2 (input, res, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer :: n = 1
    res = maxval (input, dim=n, mask=mask)
  end subroutine maxval_2

  subroutine minval_1 (input, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    res = minval (input, dim=1, mask=mask)
  end subroutine minval_1

  subroutine minval_2 (input, res, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer, dimension(:), intent(out) :: res
    integer :: n = 1
    res = minval (input, dim=n, mask=mask)
  end subroutine minval_2

end module z

program main
  use z
  implicit none
  integer :: i2(2,3) = reshape([1,2,4,8,16,32], [2,3])
  integer, dimension(3) :: res3
  integer, dimension(2) :: res2
  call sum_1 (i2, res3)
  if (any (res3 /= [3, 12, 48])) stop 1
  res3 = -2
  call sum_2 (i2, res3)
  if (any (res3 /= [3, 12, 48])) stop 2
  call maxloc_1 (i2, res3)
  if (any (res3 /= 2)) stop 3
  call minloc_1 (i2, res3)
  if (any (res3 /= 1)) stop 4
  call maxloc_2 (i2, res3)
  if (any (res3 /= 2)) stop 5
  call findloc_1 (i2, 4, res2)
  if (any(res2 /= [1,2])) stop 6
  res2 = -1234
  call findloc_2 (i2, 4, res2)
  if (any(res2 /= [1,2])) stop 7
  call findloc_3 (i2, 4, res3)
  if (any(res3 /= [0,1,0])) stop 8
  call findloc_4 (i2, 4, res3)
  if (any(res3 /= [0,1,0])) stop 9
  call maxval_1 (i2, res3)
  if (any (res3 /= [2,8,32])) stop 10
  call minval_1 (i2, res3)
  if (any (res3 /= [1,4,16])) stop 11
  call maxval_2 (i2, res3)
  if (any (res3 /= [2,8,32])) stop 12
  call minval_2 (i2, res3)
  if (any (res3 /= [1,4,16])) stop 13

end program main
