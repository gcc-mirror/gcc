! PR fortran/99010
!
! Follow-up to PR fortran/98913
!
! Contributed by G. Steinmetz
!
program p
   integer :: x[*]
   x = this_image()
   if ( this_image() == 2 ) then
      x = x[1]
   end if
end
