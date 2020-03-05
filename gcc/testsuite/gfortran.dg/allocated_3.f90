! { dg-do compile }
! PR fortran/91551
! Contributed by Gerhard Steinmetz
program p
   if (allocated()) stop 1 ! { dg-error "requires an array or scalar allocatable" }
end
