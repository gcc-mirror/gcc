! PR middle-end/101624
! { dg-do compile }
! { dg-options "-O2 -fsanitize=undefined" }

complex function foo (x)
  complex, intent(in) :: x
  foo = aimag (x)
end
program pr101624
  complex, parameter :: a = (0.0, 1.0)
  complex :: b, foo
  b = foo (a)
end
