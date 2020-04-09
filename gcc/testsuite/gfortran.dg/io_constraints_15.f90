! { dg-do compile }
!
! PR fortran/87923
!
program p
   open (1, blank=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   open (2, decimal=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   open (3, encoding=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   open (4, round=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   open (5, sign=char(1000,4)) ! { dg-error "must be a character string of default kind" }
end
