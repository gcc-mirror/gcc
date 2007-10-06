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
