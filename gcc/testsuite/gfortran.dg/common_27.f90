! { dg-do compile }
! PR fortran/108453 - a use associated variable cannot occur in COMMON
! Contributed by G.Steinmetz

module m
  type t
  end type
  real :: r
end
program p
  use m, only: t, r
  common t      ! { dg-error "USE associated from module" }
  common /cm/ r ! { dg-error "USE associated from module" }
end
