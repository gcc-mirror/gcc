! { dg-do compile }
! { dg-options "-funsigned -pedantic" }
program main
  unsigned, parameter :: u = 7u
  print *,mod(-(u+1u),u) ! { dg-error "Negation of unsigned expression" }
end program main
