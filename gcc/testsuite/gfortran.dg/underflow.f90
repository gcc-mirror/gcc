! { dg-do compile}
program a
   real x
   x = tiny(x) / huge(x)  ! { dg-warning "Arithmetic underflow" "" }
end program a
