! { dg-do compile }
! PR fortran/67802
! Original code contribute by gerhard.steinmetz.fortran at t-online.de
program p
   character(1.) :: c1 = ' '      ! { dg-error "must be of INTEGER" }
   character(1d1) :: c2 = ' '     ! { dg-error "must be of INTEGER" }
   character((0.,1.)) :: c3 = ' ' ! { dg-error "must be of INTEGER" }
   character(.true.) :: c4 = ' '  ! { dg-error "must be of INTEGER" }
end program p
