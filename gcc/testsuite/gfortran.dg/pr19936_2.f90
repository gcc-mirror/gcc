! { dg-do compile }
program pr19936_2
  integer i
  print *,(/(i,i=1a,4)/) ! { dg-error "Syntax error in iterator" }
end program pr19936_2
