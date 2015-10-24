! { dg-do compile }
! PR fortran/67802
! Original code contribute by gerhard.steinmetz.fortran at t-online.de
program p
   character(1.) :: c1 = ' '      ! { dg-error "INTEGER expression expected" }
   character(1d1) :: c2 = ' '     ! { dg-error "INTEGER expression expected" }
   character((0.,1.)) :: c3 = ' ' ! { dg-error "INTEGER expression expected" }
   character(.true.) :: c4 = ' '  ! { dg-error "INTEGER expression expected" }
end program p
