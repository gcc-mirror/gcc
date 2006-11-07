! { dg-do compile }
! { dg-options "-std=f95" }
  intrinsic char
  call foo(char) ! { dg-error "Fortran 2003: CHAR intrinsic as actual argument" }
  end
