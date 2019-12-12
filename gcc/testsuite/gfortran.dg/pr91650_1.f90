! { dg-do compile }
! PR fortran/91650
! Code contributed by Gerhard Steinmetz.
program p
   print *, b'10110' ! { dg-error "cannot appear in an output IO list" }
   print *, o'10110' ! { dg-error "cannot appear in an output IO list" }
   print *, z'10110' ! { dg-error "cannot appear in an output IO list" }
end
