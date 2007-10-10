! { dg-do run }
! { dg-options "-std=legacy -ffree-line-length-none -fno-range-check -fwrapv" }
program test
  integer :: count
  integer :: i
  integer(kind=1) :: i1
  real :: r

#define TEST_LOOP(var,from,to,step,total,test,final) \
  count = 0 ; do var = from, to, step ; count = count + 1 ; end do ; \
  if (count /= total) call abort ; \
  if (test (from, to, step, final) /= total) call abort

  ! Integer loops
  TEST_LOOP(i, 0, 0, 1, 1, test_i, 1)
  TEST_LOOP(i, 0, 0, 2, 1, test_i, 2)
  TEST_LOOP(i, 0, 0, -1, 1, test_i, -1)
  TEST_LOOP(i, 0, 0, -2, 1, test_i, -2)

  TEST_LOOP(i, 0, 1, 1, 2, test_i, 2)
  TEST_LOOP(i, 0, 1, 2, 1, test_i, 2)
  TEST_LOOP(i, 0, 1, 3, 1, test_i, 3)
  TEST_LOOP(i, 0, 1, huge(0), 1, test_i, huge(0))
  TEST_LOOP(i, 0, 1, -1, 0, test_i, 0)
  TEST_LOOP(i, 0, 1, -2, 0, test_i, 0)
  TEST_LOOP(i, 0, 1, -3, 0, test_i, 0)
  TEST_LOOP(i, 0, 1, -huge(0), 0, test_i, 0)
  TEST_LOOP(i, 0, 1, -huge(0)-1, 0, test_i, 0)

  TEST_LOOP(i, 1, 0, 1, 0, test_i, 1)
  TEST_LOOP(i, 1, 0, 2, 0, test_i, 1)
  TEST_LOOP(i, 1, 0, 3, 0, test_i, 1)
  TEST_LOOP(i, 1, 0, huge(0), 0, test_i, 1)
  TEST_LOOP(i, 1, 0, -1, 2, test_i, -1)
  TEST_LOOP(i, 1, 0, -2, 1, test_i, -1)
  TEST_LOOP(i, 1, 0, -3, 1, test_i, -2)
  TEST_LOOP(i, 1, 0, -huge(0), 1, test_i, 1-huge(0))
  TEST_LOOP(i, 1, 0, -huge(0)-1, 1, test_i, -huge(0))

  TEST_LOOP(i, 0, 17, 1, 18, test_i, 18)
  TEST_LOOP(i, 0, 17, 2, 9, test_i, 18)
  TEST_LOOP(i, 0, 17, 3, 6, test_i, 18)
  TEST_LOOP(i, 0, 17, 4, 5, test_i, 20)
  TEST_LOOP(i, 0, 17, 5, 4, test_i, 20)
  TEST_LOOP(i, 17, 0, -1, 18, test_i, -1)
  TEST_LOOP(i, 17, 0, -2, 9, test_i, -1)
  TEST_LOOP(i, 17, 0, -3, 6, test_i, -1)
  TEST_LOOP(i, 17, 0, -4, 5, test_i, -3)
  TEST_LOOP(i, 17, 0, -5, 4, test_i, -3)

  TEST_LOOP(i1, -huge(i1)-1_1, huge(i1), 1_1, int(huge(i1))*2+2, test_i1, huge(i1)+1_1)
  TEST_LOOP(i1, -huge(i1)-1_1, huge(i1), 2_1, int(huge(i1))+1, test_i1, huge(i1)+1_1)
  TEST_LOOP(i1, -huge(i1)-1_1, huge(i1), huge(i1), 3, test_i1, 2_1*huge(i1)-1_1)

  TEST_LOOP(i1, huge(i1), -huge(i1)-1_1, -1_1, int(huge(i1))*2+2, test_i1, -huge(i1)-2_1)
  TEST_LOOP(i1, huge(i1), -huge(i1)-1_1, -2_1, int(huge(i1))+1, test_i1, -huge(i1)-2_1)
  TEST_LOOP(i1, huge(i1), -huge(i1)-1_1, -huge(i1), 3, test_i1, -2_1*huge(i1))
  TEST_LOOP(i1, huge(i1), -huge(i1)-1_1, -huge(i1)-1_1, 2, test_i1, -huge(i1)-2_1)

  TEST_LOOP(i1, -2_1, 3_1, huge(i1), 1, test_i1, huge(i1)-2_1)
  TEST_LOOP(i1, -2_1, 3_1, -huge(i1), 0, test_i1, -2_1)
  TEST_LOOP(i1, 2_1, -3_1, -huge(i1), 1, test_i1, 2_1-huge(i1))
  TEST_LOOP(i1, 2_1, -3_1, huge(i1), 0, test_i1, 2_1)

  ! Real loops
  TEST_LOOP(r, 0.0, 1.0, 0.11, 1 + int(1.0/0.11), test_r, 0.0)
  TEST_LOOP(r, 0.0, 1.0, -0.11, 0, test_r, 0.0)
  TEST_LOOP(r, 0.0, -1.0, 0.11, 0, test_r, 0.0)
  TEST_LOOP(r, 0.0, -1.0, -0.11, 1 + int(1.0/0.11), test_r, 0.0)
  TEST_LOOP(r, 0.0, 0.0, 0.11, 1, test_r, 0.0)
  TEST_LOOP(r, 0.0, 0.0, -0.11, 1, test_r, 0.0)

#undef TEST_LOOP

contains

  function test_i1 (from, to, step, final) result(res)
    integer(kind=1), intent(in) :: from, to, step, final
    integer(kind=1) :: i
    integer :: res

    res = 0
    do i = from, to, step
      res = res + 1
    end do
    if (i /= final) call abort
  end function test_i1

  function test_i (from, to, step, final) result(res)
    integer, intent(in) :: from, to, step, final
    integer :: i
    integer :: res

    res = 0
    do i = from, to, step
      res = res + 1
    end do
    if (i /= final) call abort
  end function test_i

  function test_r (from, to, step, final) result(res)
    real, intent(in) :: from, to, step, final
    real :: i
    integer :: res

    res = 0
    do i = from, to, step
      res = res + 1
    end do
    ! final is ignored
  end function test_r

end program test
