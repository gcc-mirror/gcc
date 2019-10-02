! { dg-do compile }
! PR fortran/91642
! Code contributed by Gerhard Steinmetz
program p
   integer i
   integer :: iol
   integer, external :: null
   i = 0
   inquire (iolength=iol) null()
   if (iol == 4) stop 1
end

subroutine q
   integer i
   integer :: iol
   i = 0
   inquire (iolength=iol) i, null() ! { dg-error "cannot appear in INQUIRE" }
   if (iol == 4) stop 2
end
