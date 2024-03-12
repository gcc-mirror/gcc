! { dg-do compile }
! PR fortran/32986 - Improve diagnostic message for COMMON with automatic object

function a(n)
  real :: x(n) ! { dg-error "Automatic object" }
  common /c/ x ! { dg-error "cannot appear in COMMON" }
end function
