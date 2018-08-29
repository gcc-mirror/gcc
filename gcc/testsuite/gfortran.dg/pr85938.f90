! { dg-do run }
! PR fortran/85938
program foo
  real a(9), b(3)
  integer :: n = 3
  a = 1.0
  b = 1.0
  if (any(matmul(reshape(A, (/ n, n /)), b) /= 3.)) stop 1
end program
