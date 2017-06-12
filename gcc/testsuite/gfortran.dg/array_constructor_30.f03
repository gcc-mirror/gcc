! { dg-do compile }

! PR fortran/36492
! Similar to the ICE-test, but now test for complaint about constant
! specification expression.

implicit none

integer :: a = 42
type t
  character (a) :: arr (1) = [ "a" ]
  ! { dg-error "in the expression" "" { target *-*-* } .-1 }
  ! { dg-error "specification expression" "" { target *-*-* } .-2 }
end type t

end
