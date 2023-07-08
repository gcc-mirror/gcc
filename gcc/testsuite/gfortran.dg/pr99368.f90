! { dg-do compile }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type y ! { dg-error "Derived type" }
   end type
contains
   subroutine s1
      namelist /x/ y ! { dg-error "conflicts with namelist object" }
      character(3) y
   end
   subroutine s2
      namelist /z/ y ! { dg-error "conflicts with namelist object" }
      character(3) y
   end
end