! { dg-do compile }
! PR fortran/78814
! Code contributed by Gerhard Steinmetz
program p
   class(*) :: x  ! { dg-error " must be dummy, allocatable or pointer" }
   print *, f(x)
end

