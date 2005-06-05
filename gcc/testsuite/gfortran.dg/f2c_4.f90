! { dg-do run }
! { dg-additional-sources f2c_4.c }
! { dg-options "-ff2c -w" }

! Check -ff2c calling conventions
!   Return value of REAL function is promoted to C type double
!   Addional underscore appended to function name  
call f2c_4a()
end

real function f2c_4b(x)
  double precision x
  f2c_4b = x
end
