! { dg-do compile }
program pr19936_3
  integer, parameter :: i = 4
  print *,(/(i,i,4)/) ! { dg-error "Syntax error in COMPLEX" }
end program pr19936_3
