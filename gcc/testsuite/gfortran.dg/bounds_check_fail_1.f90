! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
  integer x(1)
  x(2) = x(1) ! { dg-warning "out of bounds" }
  end
! { dg-output "Index '2' of dimension 1 of array 'x' above upper bound of 1" }
