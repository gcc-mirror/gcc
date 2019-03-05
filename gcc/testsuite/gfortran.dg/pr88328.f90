! { dg-do compile }
! PR fortran/88328
program p
   character(3), parameter :: a(0) = [character(3)::]
   print a ! { dg-error "zero-sized array" }
end
