! { dg-do compile }
! PR fortran/85521
program p
   character(3) :: c = 'abc'
   character(3) :: z(1)
   z = [ c(:-1) ]
   print *, z
end
