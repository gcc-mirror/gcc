! { dg-do compile }
! PR fortran/92018
subroutine sub (f)
   integer :: f
   print *, f(b'11') ! { dg-error "cannot appear as an actual" }
   print *, f(o'11') ! { dg-error "cannot appear as an actual" }
   print *, f(z'11') ! { dg-error "cannot appear as an actual" }
end
