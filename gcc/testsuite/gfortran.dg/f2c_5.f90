! { dg-do run }
! { dg-additional-sources f2c_5.c }
! { dg-options "-fno-f2c -w" }
! Check calling conventions without -ff2c
program f2c_5  
  call f2c_5a()
end

real function f2c_5b(x)
  double precision x
  f2c_5b = x
end
