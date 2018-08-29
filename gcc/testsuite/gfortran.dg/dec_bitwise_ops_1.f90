! { dg-do run }
! { dg-options "-fdec" }
!
! Runtime tests to verify logical-to-bitwise operations perform as expected
! with -fdec.
!

subroutine assert(expected, actual, str)
  implicit none
  character(*), intent(in) :: str
  integer, intent(in)      :: expected, actual
  if (actual .ne. expected) then
    write (*, '(A,I4,I4)') str, expected, actual
    STOP 1
  endif
end subroutine

implicit none

integer expected, expected_expr
integer output_vars, output_const, output_expr
integer op1, op2, mult

mult = 3
op1 = 3
op2 = 5

!!!! AND -> IAND

expected      = IAND(op1, op2)
expected_expr = mult*expected

output_const  = 3 .AND. 5
output_vars   = op1 .AND. op2
output_expr   = mult * (op1 .AND. op2)

call assert(expected, output_vars,      "( ) and")
call assert(expected, output_const,     "(c) and")
call assert(expected_expr, output_expr, "(x) and")

!!!! EQV -> NOT IEOR

expected   = NOT(IEOR(op1, op2))
expected_expr = mult*expected

output_const    = 3 .EQV. 5
output_vars     = op1 .EQV. op2
output_expr     = mult * (op1 .EQV. op2)

call assert(expected, output_vars,       "( ) EQV")
call assert(expected, output_const,      "(c) EQV")
call assert(expected_expr, output_expr,  "(x) EQV")

!!!! NEQV -> IEOR

expected   = IEOR(op1, op2)
expected_expr = mult*expected

output_const    = 3 .NEQV. 5
output_vars     = op1 .NEQV. op2
output_expr     = mult * (op1 .NEQV. op2)

call assert(expected, output_vars,       "( ) NEQV")
call assert(expected, output_const,      "(c) NEQV")
call assert(expected_expr, output_expr,  "(x) NEQV")

!!!! NOT -> NOT

expected   = NOT(op2)
expected_expr = mult*expected

output_const    = .NOT. 5
output_vars     = .NOT. op2
output_expr     = mult * (.NOT. op2)

call assert(expected, output_vars,       "( ) NOT")
call assert(expected, output_const,      "(c) NOT")
call assert(expected_expr, output_expr,  "(x) NOT")

!!!! OR -> IOR

expected   = IOR(op1, op2)
expected_expr = mult*expected

output_const    = 3 .OR. 5
output_vars     = op1 .OR. op2
output_expr     = mult * (op1 .OR. op2)

call assert(expected, output_vars,       "( ) OR")
call assert(expected, output_const,      "(c) OR")
call assert(expected_expr, output_expr,  "(x) OR")

!!!! XOR -> IEOR, not to be confused with .XOR.

expected  = IEOR(op1, op2)
expected_expr = mult*expected

output_const    = 3 .XOR. 5
output_vars     = op1 .XOR. op2
output_expr     = mult * (op1 .XOR. op2)

call assert(expected, output_vars,       "( ) XOR")
call assert(expected, output_const,      "(c) XOR")
call assert(expected_expr, output_expr,  "(x) XOR")

end
