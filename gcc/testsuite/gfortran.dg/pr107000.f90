! { dg-do compile }
! PR fortran/107000 - ICE in gfc_real2complex, reduce_unary, reduce_binary_*
! Contributed by G.Steinmetz

program p
  real    :: y(1)
  complex :: x(1)
  x = (1.0, 2.0) * [real :: -'1']    ! { dg-error "Operand of unary numeric operator" }
  x = (1.0, 2.0) * [complex :: +'1'] ! { dg-error "Operand of unary numeric operator" }
  x = [complex :: -'1'] * (1.0, 2.0) ! { dg-error "Operand of unary numeric operator" }
  y = [complex :: -'1'] * 2          ! { dg-error "Operand of unary numeric operator" }
  y = 2 * [complex :: -'1']          ! { dg-error "Operand of unary numeric operator" }
  y = 2 * [complex :: -(.true.)]     ! { dg-error "Operand of unary numeric operator" }
  y = [complex :: -(.true.)] * 2     ! { dg-error "Operand of unary numeric operator" }
  print *, - [real ::  -'1' ]        ! { dg-error "Operand of unary numeric operator" }
  print *, - [real :: [-'1']]        ! { dg-error "Operand of unary numeric operator" }
  print *, - [real ::  +(.true.) ]   ! { dg-error "Operand of unary numeric operator" }
  print *, - [real :: [+(.true.)]]   ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real ::  -'1' ]      ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real :: (-'1')]      ! { dg-error "Operand of unary numeric operator" }
  print *, [real ::  -'1' ] * 2      ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: (-'1')] * 2      ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [integer :: -('1')]   ! { dg-error "Operand of unary numeric operator" }
  print *, [integer :: -('1')] * 2   ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real :: 0, (-'1')]   ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 0, (-'1')] * 2   ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real :: 0, -'1']     ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 0, -'1'] * 2     ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real :: 0, 1+'1']    ! { dg-error "Operands of binary numeric operator" }
  print *, [real :: 0, 1+'1'] * 2    ! { dg-error "Operands of binary numeric operator" }
  print *, [real :: 1, +(.true.)]    ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, -(.true.)]    ! { dg-error "Operand of unary numeric operator" }
  print *, 2 * [real :: 1, +(.true.)]      ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, +(.true.)] * 2      ! { dg-error "Operand of unary numeric operator" }
  print *, [1, 2] * [real :: 1, +(.true.)] ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, +(.true.)] * [1, 2] ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, 2] * [real :: 1, +(.true.)] ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, +(.true.)] * [real :: 1, 2] ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 0, -'1'] * [real :: 1, +(+(.true.))] ! { dg-error "Operand of unary numeric operator" }
  print *, [real :: 1, [(+(.true.))]] * [real :: 0, [(-'1')]] ! { dg-error "Operand of unary numeric operator" }

  ! Legal:
  print *, 2 * [real :: 1, [2], 3]
  print *, [real :: 1, [2], 3] * 2
  print *, [real :: 1, [2], 3] * [real :: 1, [2], 3]
  print *, [real :: 1, [2], 3] * [integer :: 1, [2], 3]
  print *, [real :: 1, [2], 3] * [1, [2], 3]
  print *, [real :: 1,  huge(2.0)] * [real :: 1,  real(1.0)]
  print *, [real :: 1, -(huge(2.0))] * [real :: 1, +(real(1))]
end
