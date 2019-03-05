! { dg-do run }
! PR 82995 - segfault passing on an optional argument;
! this tests the inline versions.
module y
  implicit none
contains 

  function sum_1 (input, mask)
    logical, intent(in), optional :: mask(:)
    integer, intent(in) :: input(:)
    integer :: sum_1
    sum_1 = sum (input, mask)
  end function sum_1

  function sum_2 (input, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:)
    integer :: sum_2
    sum_2 = sum(input, mask)
  end function sum_2

  function sum_3 (input, mask)
    logical, intent(in), optional :: mask(:,:)
    integer, intent(in) :: input(:,:)
    integer :: sum_3
    sum_3 = sum (input, mask)
  end function sum_3

  function minval_1 (input, mask)
    logical, intent(in), optional :: mask(:,:)
    real, intent(in) :: input(:,:)
    real :: minval_1
    minval_1 = minval (input, mask)
  end function minval_1

  function maxval_1 (input, mask)
    logical, intent(in), optional :: mask
    real, intent(in) :: input(:,:)
    real :: maxval_1
    maxval_1 = maxval (input, mask)
  end function maxval_1

  function maxloc_1 (input, mask)
    logical, intent(in), optional :: mask(:)
    real, intent(in) :: input(:)
    integer :: maxloc_1

    maxloc_1 = maxloc(input, dim=1, mask=mask)
  end function maxloc_1

  function findloc_1 (input, val, mask)
    logical, intent(in), optional :: mask (:)
    integer, intent(in) :: input(:)
    integer, intent(in) :: val
    integer :: findloc_1

    findloc_1 = findloc(input, val, dim=1, mask=mask)
  end function findloc_1

  function findloc_2 (input, val, mask)
    logical, intent(in), optional :: mask
    integer, intent(in) :: input(:)
    integer, intent(in) :: val
    integer :: findloc_2

    findloc_2 = findloc(input, val, dim=1, mask=mask)
  end function findloc_2

end module y

program test_sum_1 
  use y
  implicit none 
  integer :: input(5) = [1,2,4,8,16]
  integer :: i2(2,3) = reshape([1,2,4,8,16,32], [2,3])
  real :: r2(2,3) = reshape ([32.,16.,8.,4.,2.,1.], [2,3])
  real :: r1(6) = [2.,4.,8.,32.,1.,16.]
  integer :: res
  real :: rres
  res = sum_1(input) 
  if (res /= 31) stop 1
  res = sum_2 (input)
  if (res /= 31) stop 2
  res = sum_3 (i2)
  if (res /= 63) stop 3
  rres = minval_1 (r2)
  if (rres /= 1.0) stop 4
  rres = maxval_1 (r2)
  if (rres /= 32.) stop 5
  res = maxloc_1 (r1)
  if (res /= 4) stop 6
  res = findloc_1 (input, 8)
  if (res /= 4) stop 7
  res = findloc_2 (input, 2)
  if (res /= 2) stop 8
end program test_sum_1
