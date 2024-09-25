! { dg-do compile }
! { dg-options "-funsigned -pedantic" }
! Some checks with -pedantic.
program main
  unsigned :: u
  print *,-129u_1 ! { dg-error "Negation of unsigned constant" }
  print *,256u_1 ! { dg-error "Unsigned constant truncated" }
  u = 1u
  u = -u ! { dg-error "Negation of unsigned expression" }
end program
