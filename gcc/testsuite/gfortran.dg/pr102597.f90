! { dg-do compile }
! Check that PR102597 does not resurface. Regression caused ICE at associate
! statement.
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
program p
   use iso_fortran_env
   associate (y => (compiler_version)) ! { dg-error "is a procedure name" }
   end associate
end
