! { dg-require-effective-target fortran_large_real }
! { dg-do run { xfail powerpc*-apple-darwin* } }
! Test XFAILed on powerpc-darwin because the system's printf() lacks
! proper support for long doubles.
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
module test_default_format
  interface test
    module procedure test_rl
  end interface test

  integer, parameter :: kl = selected_real_kind (precision (0.0_8) + 1)
  integer, parameter :: count = 200

contains

  function test_rl (start, towards) result (res)
    integer, parameter :: k = kl
    integer, intent(in) :: towards
    real(k), intent(in) :: start

    integer :: res, i
    real(k) :: x, y
    character(len=100) :: s

    res = 0

    if (towards >= 0) then
      x = start
      do i = 0, count
        write (s,*) x
        read (s,*) y
        if (y /= x) res = res + 1
        x = nearest(x,huge(x))
      end do
    end if

    if (towards <= 0) then
      x = start
      do i = 0, count
        write (s,*) x
        read (s,*) y
        if (y /= x) res = res + 1
        x = nearest(x,-huge(x))
      end do
    end if
  end function test_rl

end module test_default_format

program main
  use test_default_format

  if (test (1.0_kl, 0) /= 0) call abort
  if (test (0.0_kl, 0) /= 0) call abort
  if (test (tiny(0.0_kl), 0) /= 0) call abort
  if (test (-tiny(0.0_kl), 0) /= 0) call abort
  if (test (huge(0.0_kl), -1) /= 0) call abort
  if (test (-huge(0.0_kl), 1) /= 0) call abort

end program main
!
! { dg-final { cleanup-modules "test_default_format" } }
