!{ dg-do compile }
!
program p
  type t
    real :: a = 1.0
  end type
  type(t), parameter :: x = z'1' ! { dg-error "incompatible with a BOZ" }
  x%a = x%a + 2 ! { dg-error "has no IMPLICIT type" }
end
