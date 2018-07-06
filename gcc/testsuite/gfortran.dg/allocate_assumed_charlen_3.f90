! { dg-do compile }
! PR Fortran/83741
! Contributed by Gerhard Steinmetz <gscfq at t-online dot de>
program p
   allocate (character(*) :: x) ! { dg-error "Incompatible allocate-object" }
end

