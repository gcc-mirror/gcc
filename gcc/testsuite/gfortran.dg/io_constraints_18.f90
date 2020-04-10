! { dg-options "-fdec" }
! { dg-do compile }
!
! PR fortran/87923
!
program p
   open (1, carriagecontrol=char(1000,4)) ! { dg-error "must be a character string of default kind" }
   open (2, share=char(1000,4)) ! { dg-error "must be a character string of default kind" }
end
