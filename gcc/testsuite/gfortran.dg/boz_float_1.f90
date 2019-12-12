! { dg-do compile }
program foo
   print *, float(z'1234') ! { dg-error "cannot appear in" }
end program foo
