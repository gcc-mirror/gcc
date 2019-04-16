! { dg-do compile }
! PR fortran/80752
module exchange_utils

  implicit none

  integer, parameter, public :: knd = 8

  type, private :: a
     logical :: add_vs98 = 0.0_knd ! { dg-error "Cannot convert" }
  end type a

  type, private :: x_param_t
     type(a) :: m05_m06
  end type x_param_t

  type(x_param_t), public, save :: x_param

end module exchange_utils

