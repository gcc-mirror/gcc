! { dg-do compile }
! PR63797 - Bogus ambiguous reference to 'sqrt'

module mod1
  implicit none
  real, parameter :: z = sqrt (0.0)
  real            :: w = sqrt (1.0)
  interface
     pure real function sqrt_ifc (x)
       real, intent(in) :: x
     end function sqrt_ifc
  end interface
contains
  pure function myroot () result (f)
    procedure(sqrt_ifc), pointer :: f
    intrinsic :: sqrt
    f => sqrt
  end function myroot
end module mod1

module mod2
  implicit none
  type t
     real :: a = 0.
  end type
  interface sqrt
     module procedure sqrt
  end interface
contains
  elemental function sqrt (a)
    type(t), intent(in) :: a
    type(t)             :: sqrt
    sqrt% a = a% a
  end function sqrt
end module mod2

module mod3
  implicit none
  abstract interface
     function real_func (x)
       real              :: real_func
       real, intent (in) :: x
     end function real_func
  end interface
  intrinsic :: sqrt
  procedure(real_func), pointer :: real_root => sqrt
end module mod3

program test
  use mod1
  use mod2
  use mod3
  implicit none
  type(t) :: x, y
  procedure(sqrt_ifc), pointer :: root
  root => myroot ()
  y    = sqrt (x)
  y% a = sqrt (x% a) + z - w + root (x% a)
  y% a = real_root (x% a)
end program test
