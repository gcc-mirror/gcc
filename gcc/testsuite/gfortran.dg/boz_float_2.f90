! { dg-do compile }
! { dg-options "-fallow-invalid-boz" }
program foo
   print *, float(z'1234') ! { dg-warning "cannot appear in" }
end program foo
