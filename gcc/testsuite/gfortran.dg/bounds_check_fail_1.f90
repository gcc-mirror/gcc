! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
  integer x(1)
  x(2) = x(1) ! { dg-warning "out of bounds" }
  end
! { dg-output "out of bounds for array 'x', upper bound of dimension 1 exceeded." }
