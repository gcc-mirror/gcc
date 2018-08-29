! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
  integer x(10), m, n
  x = (/ (i, i = 1, 10) /)
  m = -3
  n = -2
  x(7:1:m) = x(6:2:n) 
  if (any(x /= (/ 2, 2, 3, 4, 5, 6, 6, 8, 9, 10 /))) STOP 1
  x(8:1:m) = x(5:2:n) 
  end
! { dg-output "line 10 .* bound mismatch .* dimension 1 .* array \'x\' \\\(3/2\\\)" }
