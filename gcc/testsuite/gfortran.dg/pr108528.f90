! { dg-do compile }
! PR fortran/108528 -
! Contributed by G.Steinmetz

function f()         ! { dg-error "mismatched array specifications" }
  integer :: f((2.)) ! { dg-error "must be of INTEGER type" }
  integer :: g((2))
entry g()
end
