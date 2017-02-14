! { dg-do compile }
! { dg-options -O }
program p
   logical x(2), y(2)
   x = .true.
   y = .nt. x   ! { dg-error "Unknown operator" }
end
