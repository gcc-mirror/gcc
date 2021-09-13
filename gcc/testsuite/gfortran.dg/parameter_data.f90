! { dg-do compile }
! PR fortran/49278 - ICE when combining DATA with default initialization

program p
  implicit none
  type t
     real :: a
  end type t
  integer, parameter :: b = 42
  type(t), parameter :: z = t(4.0)
  data b   / 666 / ! { dg-error "shall not appear in a DATA statement" }
  data z%a / 3.0 / ! { dg-error "shall not appear in a DATA statement" }
end
