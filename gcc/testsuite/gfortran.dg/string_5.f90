! { dg-do compile }
! PR fortran/48876 - this used to segfault.
! Test case contributed by mhp77 (a) gmx.at.
program test
  character ::  string =  "string"( : -1 )
end program test

