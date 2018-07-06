! { dg-do compile }
! { dg-options "-Werror -O3" }
!
! Tests the fix for PR78619, in which the recursive use of 'f' at line 13
! caused an ICE.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
  print *, g(1.0) ! 'g' is OK
contains
  function f(x) result(z)
    real :: x, z
    z = sign(1.0, f) ! { dg-error "calling itself recursively|must be the same type" }
  end
  real function g(x)
    real :: x
    g = -1
    g = -sign(1.0, g) ! This is OK.
  end
end
! { dg-message "all warnings being treated as errors" "" { target *-*-* } 0 }
