! { dg-do compile }
! PR fortran/69499
! Contributed by Gerhard Steinmetz.
module m
   class(*) :: z        ! { dg-error "must be dummy, allocatable or pointer" }
   select type (x => z) ! { dg-error "cannot appear in this scope" }
end
