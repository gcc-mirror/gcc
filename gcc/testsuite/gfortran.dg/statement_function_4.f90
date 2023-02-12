! { dg-do compile }
! PR fortran/69604
! Contributed by G.Steinmetz

program p
  x(n) = 1 + n(2.0) ! { dg-error "Invalid use of statement function argument" }
  y(k) = k()        ! { dg-error "Invalid use of statement function argument" }
  z(m) = m          ! { dg-warning "Statement function" }
  print *, x(n)
end
