! { dg-do run }
! { dg-additional-sources bind_c_coms_driver.c }
! { dg-options "-w" }
! the -w option is to prevent the warning about long long ints
module bind_c_coms
  use, intrinsic :: iso_c_binding
  implicit none

  common /COM/ R, S
  real(c_double) :: r
  real(c_double) :: t 
  real(c_double) :: s 
  bind(c) :: /COM/, /SINGLE/, /MYCOM/
  common /SINGLE/ T
  common /MYCOM/ LONG_INTS
  integer(c_long) :: LONG_INTS
  common /MYCOM2/ LONG_LONG_INTS
  integer(c_long_long) :: long_long_ints
  bind(c) :: /mycom2/

  common /com2/ i, j
  integer(c_int) :: i, j
  bind(c, name="f03_com2") /com2/

  common /com3/ m, n
  integer(c_int) :: m, n
  bind(c, name="") /com3/

contains
  subroutine test_coms() bind(c)
    r = r + .1d0;
    s = s + .1d0;
    t = t + .1d0;
    long_ints = long_ints + 1
    long_long_ints = long_long_ints + 1
    i = i + 1
    j = j + 1

    m = 1
    n = 1
  end subroutine test_coms
end module bind_c_coms

module bind_c_coms_2
  use, intrinsic :: iso_c_binding, only: c_int
  common /com3/ m, n
  integer(c_int) :: m, n
  bind(c, name="") /com3/
end module bind_c_coms_2
