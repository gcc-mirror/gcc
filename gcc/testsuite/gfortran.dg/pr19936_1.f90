! { dg-do compile }
program pr19936_1
  integer, parameter :: i=4
  print *,(/(i,i=1,4)/) ! { dg-error "Expected VARIABLE" }
end program pr19936_1
