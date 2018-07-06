! { dg-do compile }
! PR fortran/85543
program p
   procedure(), pointer :: z
contains
   real(z()) function f()  ! { dg-error "in initialization expression at" }
   end
end
