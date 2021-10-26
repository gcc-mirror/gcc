! { dg-do compile }
! PR fortran/102816

program p
  type t
     integer :: a([2])     ! { dg-error "must be scalar" }
  end type
  type(t) :: x = t([3, 4]) ! { dg-error "Bad array spec of component" }
end
