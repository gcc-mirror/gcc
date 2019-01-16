! { dg-do compile }
! PR fortran/88025
program p
   type t
      character(('')) :: c = 'c'    ! { dg-error "must be of INTEGER type" }
   end type
end
