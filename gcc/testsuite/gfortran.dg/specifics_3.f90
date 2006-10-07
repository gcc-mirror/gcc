! { dg-do compile }
! { dg-options "-std=f95" }
  intrinsic char
  call foo(char) ! { dg-error "Fortran 2003: CHAR intrinsic allowed as actual argument" }
  end
