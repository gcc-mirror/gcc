! { dg-do compile }
! PR fortran/104349 - reject module variable as character length in PARAMETER
! Contributed by G.Steinmetz

module m
  character(n), parameter :: a(1) = 'b' ! { dg-error "cannot appear" }
  character(n), parameter :: c    = 'b' ! { dg-error "cannot appear" }
end
