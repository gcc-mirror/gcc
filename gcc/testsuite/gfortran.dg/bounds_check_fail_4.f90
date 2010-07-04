! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
  integer x(10), m, n
  x = (/ (i, i = 1, 10) /)
  m = -3
  n = -2
  x(7:1:m) = x(1:3) + x(6:2:n) 
  if (any(x /= (/ 5, 2, 3, 6, 5, 6, 7, 8, 9, 10 /))) call abort()
  x(8:1:m) = x(1:3) + x(5:2:n) 
  end
! { dg-output "line 10 .* bound mismatch .* dimension 1 .* array \'x\' \\\(2/3\\\)" }
