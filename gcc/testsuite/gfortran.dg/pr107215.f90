! { dg-do compile }
! PR fortran/107215 - ICE in gfc_real2real and gfc_complex2complex
! Contributed by G.Steinmetz

program p
  double precision, parameter :: z = 1.0d0
  complex :: x(1)
  real    :: y(1)
  x = [real :: -'1'] * z      ! { dg-error "Operand of unary numeric operator" }
  y = z * [real :: -'1']      ! { dg-error "Operand of unary numeric operator" }
  x = [real :: -(.true.)] * z ! { dg-error "Operand of unary numeric operator" }
  y = z * [real :: -(.true.)] ! { dg-error "Operand of unary numeric operator" }
  x = [complex :: -'1'] * z   ! { dg-error "Operand of unary numeric operator" }
  y = z * [complex :: -'1']   ! { dg-error "Operand of unary numeric operator" }
  x = [complex :: -(.true.)] * z ! { dg-error "Operand of unary numeric operator" }
  y = z * [complex :: -(.true.)] ! { dg-error "Operand of unary numeric operator" }
end
