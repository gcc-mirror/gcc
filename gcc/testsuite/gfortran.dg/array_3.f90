! { dg-do compile }
! PR31610 ICE with transfer, merge in gfc_conv_expr_descriptor
  integer :: i(1) = 1
  integer :: foo(3)
  integer :: n(1)
  foo(1) = 17
  foo(2) = 55
  foo(3) = 314
  print *, i, foo
  write(*,*) foo([1]), foo([1]+i), [1]+1
  n = foo([1]+i)
  print *, n, shape(foo([1]+i)), shape(foo(i+[1]))
end
