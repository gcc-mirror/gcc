! { dg-do compile }
!
! Test the fix for PR115070
!
! Contributed by Sebastien Bardeau  <bardeau@iram.fr>
!
module my_mod
  type my_type
    integer :: a
  contains
    final :: myfinal
  end type my_type
contains
  subroutine my_sub(obs)
    use ieee_arithmetic
    class(my_type), intent(out) :: obs
  end subroutine my_sub
  subroutine myfinal (arg)
    type (my_type) :: arg
    print *, arg%a
  end
end module my_mod

  use my_mod
  type (my_type) :: z
  z%a = 42
  call my_sub (z)
end
