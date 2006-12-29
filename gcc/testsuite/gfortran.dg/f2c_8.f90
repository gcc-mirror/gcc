! { dg-do compile }
! { dg-options "-ff2c" }
! PR 25392
! Verify that the type of the result variable matches the declared
! type of the function.  The actual type of the function may be
! different for f2c calling conventions.
real function goo () result (foo)
  real x
  foo = sign(foo, x)
end

real function foo ()
  real x
  foo = sign(foo, x)
end

