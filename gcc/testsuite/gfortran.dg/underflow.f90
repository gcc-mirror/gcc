! { dg-do compile}
program a
   real x
   x = 1e-20 / 1e+20  ! { dg-warning "Arithmetic underflow" "" }
end program a
