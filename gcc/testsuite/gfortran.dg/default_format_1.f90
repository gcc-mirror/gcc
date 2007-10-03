! { dg-do run { xfail *-apple-darwin* } }
! Test XFAILed on Darwin because the system's printf() lacks
! proper support for denormals.
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
module test_default_format
  interface test
    module procedure test_r4
    module procedure test_r8
  end interface test

  integer, parameter :: count = 200

contains
  function test_r4 (start, towards) result (res)
    integer, parameter :: k = 4
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
  end function test_r4

  function test_r8 (start, towards) result (res)
    integer, parameter :: k = 8
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
  end function test_r8

end module test_default_format

program main
  use test_default_format

  if (test (1.0_4, 0) /= 0) call abort
  if (test (0.0_4, 0) /= 0) call abort
  if (test (tiny(0.0_4), 0) /= 0) call abort
  if (test (-tiny(0.0_4), 0) /= 0) call abort
  if (test (huge(0.0_4), -1) /= 0) call abort
  if (test (-huge(0.0_4), 1) /= 0) call abort

  if (test (1.0_8, 0) /= 0) call abort
  if (test (0.0_8, 0) /= 0) call abort
  if (test (tiny(0.0_8), 0) /= 0) call abort
  if (test (-tiny(0.0_8), 0) /= 0) call abort
  if (test (huge(0.0_8), -1) /= 0) call abort
  if (test (-huge(0.0_8), 1) /= 0) call abort

end program main
!
! { dg-final { cleanup-modules "test_default_format" } }
