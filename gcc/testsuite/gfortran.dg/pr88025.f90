! { dg-do compile }
! PR fortran/88025
program p
   type t
      character(('')) :: c = 'c'  ! { dg-error "Scalar INTEGER expression expected" }
   end type
end
