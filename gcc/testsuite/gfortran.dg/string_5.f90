! { dg-do compile }
program test

  ! PR fortran/48876 - this used to segfault.
  ! Test case contributed by mhp77 (a) gmx.at.
  character ::  string =  "string"( : -1 )

  ! PR fortran/50409
  character v(3)
  v = (/ ('123'(i:1), i = 3, 1, -1) /) 
  print *, v

end program test

