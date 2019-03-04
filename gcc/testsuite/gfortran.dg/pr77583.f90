! { dg-do compile }
!
! PR fortran/77583 - ICE in pp_quoted_string, at pretty-print.c:966
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

pure subroutine sub(s)
contains
   pure subroutine s  ! { dg-error "conflicts with DUMMY argument" }
   end
end
