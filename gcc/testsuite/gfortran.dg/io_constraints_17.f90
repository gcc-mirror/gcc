! { dg-do compile }
!
! PR fortran/87923
!
program p
   write (1, blank=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   write (1, delim=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   write (1, pad=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   write (1, round=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   write (1, sign=char(1000,4)) ! { dg-error "must be a character string of default kind" }
end
