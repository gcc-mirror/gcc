! { dg-do run }
! { dg-additional-options "-fno-range-check -w" }
!
! Check that we can print large constants
!
! "-fno-range-check -w" is used so the testcase compiles even with targets
! that don't support large integer kinds.

program test
  use iso_fortran_env, only : ikinds => integer_kinds
  implicit none

  ! Largest integer kind
  integer, parameter :: k = ikinds(size(ikinds))
  integer, parameter :: hk = k / 2

  if (k <= 8) stop

  call check(9000000000000000000_k, "9000000000000000000")
  call check(90000000000000000000_k, "90000000000000000000")
  call check(int(huge(1_hk), kind=k), "9223372036854775807")
  call check(2_k**63, "9223372036854775808")
  call check(10000000000000000000_k, "10000000000000000000")
  call check(18446744065119617024_k, "18446744065119617024")
  call check(2_k**64 - 1, "18446744073709551615")
  call check(2_k**64, "18446744073709551616")
  call check(20000000000000000000_k, "20000000000000000000")
  call check(huge(0_k), "170141183460469231731687303715884105727")
  call check(huge(0_k)-1, "170141183460469231731687303715884105726")

  call check(-9000000000000000000_k, "-9000000000000000000")
  call check(-90000000000000000000_k, "-90000000000000000000")
  call check(-int(huge(1_hk), kind=k), "-9223372036854775807")
  call check(-2_k**63, "-9223372036854775808")
  call check(-10000000000000000000_k, "-10000000000000000000")
  call check(-18446744065119617024_k, "-18446744065119617024")
  call check(-(2_k**64 - 1), "-18446744073709551615")
  call check(-2_k**64, "-18446744073709551616")
  call check(-20000000000000000000_k, "-20000000000000000000")
  call check(-huge(0_k), "-170141183460469231731687303715884105727")
  call check(-(huge(0_k)-1), "-170141183460469231731687303715884105726")
  call check(-huge(0_k)-1, "-170141183460469231731687303715884105728")

  call check(2_k * huge(1_hk), "18446744073709551614")
  call check((-2_k) * huge(1_hk), "-18446744073709551614")

contains

  subroutine check (i, str)
    implicit none
    integer(kind=k), intent(in), value :: i
    character(len=*), intent(in) :: str

    character(len=100) :: buffer
    write(buffer,*) i
    if (adjustl(buffer) /= adjustl(str)) STOP 1
  end subroutine

end

