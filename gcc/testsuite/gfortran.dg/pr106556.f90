! { dg-do compile }
! { dg-options "-O1 -fnon-call-exceptions -ftree-loop-if-convert" }


program p
  real :: a(2)

  a(:) = 1.0
  if (minloc (a, dim = 1).ne.1) STOP 1
end
