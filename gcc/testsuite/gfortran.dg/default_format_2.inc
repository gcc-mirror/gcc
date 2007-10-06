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
