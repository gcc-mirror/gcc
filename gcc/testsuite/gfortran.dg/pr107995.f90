! { dg-do compile }
! PR fortran/107995
! Contributed by G.Steinmetz

program p
  implicit none
  integer :: n  ! { dg-error "Self-referential argument" }
  n(n) = 1      ! { dg-warning "Statement function" }
  print *, n(n) ! { dg-error "Statement function" }
end
