! { dg-do compile }
program foo
   print *, iand(z'1234', z'3456')  ! { dg-error "cannot both be" }
   print *,  and(z'1234', z'3456')  ! { dg-error "cannot both be" }
   print *, ieor(z'1234', z'3456')  ! { dg-error "cannot both be" }
   print *,  xor(z'1234', z'3456')  ! { dg-error "cannot both be" }
   print *,  ior(z'1234', z'3456')  ! { dg-error "cannot both be" }
   print *,   or(z'1234', z'3456')  ! { dg-error "cannot both be" }
end program foo

