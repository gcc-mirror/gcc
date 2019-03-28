! Test the presence of an ACC ROUTINE directive inside a function
! containg an error.

! { dg-do compile }

module m
contains
  recursive function f(x)
  end function f
  recursive function f(x)
    !$acc routine (f)
  end function f
end module m

! { dg-error "Procedure 'f' at .1. is already defined" "" { target *-*-* } 8 }
! { dg-error ".1." "" { target *-*-* } 10 }
! { dg-error "Invalid NAME 'f' in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } 11 }
! { dg-error "Expecting END MODULE statement" "" { target *-*-* } 12 }
