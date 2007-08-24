! PR 29441 : No error was given for disallowed function in
! initialization expression, even if -std=f95 was used
! { dg-do compile }
! { dg-options "-std=f95" }
real, parameter :: pi = 4.0*Atan(1.0) ! { dg-error "Fortran 2003: Elemental function as initialization expression" }
real, parameter :: three = 27.0**(1.0/3.0) ! { dg-error "Noninteger exponent in an initialization expression" }
end
