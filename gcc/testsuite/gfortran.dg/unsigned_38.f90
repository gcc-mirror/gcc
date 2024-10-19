! { dg-do compile }
! { dg-options "-funsigned" }
program main
  unsigned, parameter :: u = 7u
  print *,mod(-(u+1u),u) ! { dg-error "Operand of unary numeric operator" }
end program main
