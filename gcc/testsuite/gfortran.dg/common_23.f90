! { dg-do compile }
!
! PR fortran/66707
! Check the compilation on wrong usage of common
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>
program p
   integer, pointer :: a
   common a, a ! { dg-error "is already in a COMMON block" }
   common a
end
