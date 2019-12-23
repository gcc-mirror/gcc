! { dg-do compile }
! PR fortran/70853
! Contributed by Gerhard Steinmetz
program p
   real, pointer :: z(:)
   z(1:2) => null() ! { dg-error "pointer target shall not be NULL" }
   z(2:1) => null() ! { dg-error "pointer target shall not be NULL" }
end
