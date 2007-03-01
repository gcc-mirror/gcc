! PR 29441 : No error was given for disallowed function in
! initialization expression, even if -std=f95 was used
! { dg-do compile }
! { dg-options "-std=f95" }
real, parameter :: pi = 4.0*Atan(1.0) ! { dg-error "Evaluation of nonstandard initialization expression" }
end
