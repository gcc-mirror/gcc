! { dg-do compile }
! { dg-options "-funsigned" }
! Test different prohibited conversions.
program main
  integer :: i
  unsigned :: u
  print *,1 + 2u   ! { dg-error "Operands of binary numeric operator" }
  print *,2u + 1   ! { dg-error "Operands of binary numeric operator" }
  print *,2u ** 1  ! { dg-error "Operands of binary numeric operator" }
  print *,2u ** 1u
  print *,1u < 2   ! { dg-error "Inconsistent types" }
  print *,int(1u) < 2
end program main
