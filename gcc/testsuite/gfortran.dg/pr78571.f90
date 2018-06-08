! { dg-do compile }
! PR fortran/78571
program p
   type t
      character :: c
   end type
   character :: x = t('a') ! { dg-error "convert TYPE" }
   data x /'b'/
end
