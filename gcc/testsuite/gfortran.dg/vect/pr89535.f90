! { dg-do compile }

subroutine foo(tmp1, tmp2, tmp3)
  integer, parameter :: n = 100
  real :: tmp1(n,2), tmp2(n), tmp3(n)
  integer :: i, c1, c2, c3
  logical :: cond
  common c1, c2, c3

  c2 = c3
  cond = c1 .eq. 1 .and. c3 .eq. 1
  do i = 1,100
     if (cond) tmp2(i) = tmp1(i,1) / tmp1(i,2)
  end do
  do i = 1,100
     if (cond) tmp3(i) = tmp2(i)
  end do
end subroutine foo
