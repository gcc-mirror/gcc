! PR15620
! Check that evaluating a statement function doesn't affect the value of
! its dummy argument variables.
program st_function_2
  integer fn, a, b
  fn(a, b) = a + b
  if (foo(1) .ne. 43) STOP 1

  ! Check that values aren't modified when avaluating the arguments.
  a = 1
  b = 5
  if (fn (b + 2, a + 3) .ne. 11) STOP 2
contains
function foo (x) 
  integer z, y, foo, x 
  bar(z) = z*z 
  z = 42 
  t = bar(x) 
  foo = t + z 
end function 
end program 
