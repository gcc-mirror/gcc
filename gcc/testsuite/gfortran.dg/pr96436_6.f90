! { dg-do run }
! { dg-options "-std=f2003 -pedantic" }
! { dg-shouldfail "Zero width in format descriptor" }

character(10) :: fmt = "(g0.2)"
print fmt, 0.3
end

! { dg-output "Fortran runtime error: Zero width in format descriptor" }

