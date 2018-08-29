! { dg-do run }
! { dg-options "-fdec" }
!
! Runtime tests to verify bitwise ops perform appropriate conversions
! with -fdec.
!

subroutine assert(expected, actual, str)
  implicit none
  character(*), intent(in) :: str
  integer, intent(in)      :: expected, actual(9)
  integer :: i
  do i=1,9
    if (expected .ne. actual(i)) then
      write (*, '(A,I8,I8)') str, expected, actual(i)
      STOP 1
    endif
  enddo
end subroutine

implicit none

logical(1), volatile :: op1_1l
integer(1), volatile :: op1_1, op2_1

logical(2), volatile :: op1_2l
integer(2), volatile :: op1_2, op2_2

logical(4), volatile :: op1_4l
integer(4), volatile :: op1_4, op2_4

integer, volatile :: expect, outs(9)


op1_1l = .true.
op1_2l = .true.
op1_4l = .true.
op1_1 = 117_1
op1_2 = 117_2
op1_4 = 117_4
op2_1 =  49_1
op2_2 =  49_2
op2_4 =  49_4

!!! Explicit integer operands

expect = IAND(op1_1, op2_1)
outs(1) = op1_1 .AND. op2_1
outs(2) = op1_1 .AND. op2_2
outs(3) = op1_1 .AND. op2_4
outs(4) = op1_2 .AND. op2_1
outs(5) = op1_2 .AND. op2_2
outs(6) = op1_2 .AND. op2_4
outs(7) = op1_4 .AND. op2_1
outs(8) = op1_4 .AND. op2_2
outs(9) = op1_4 .AND. op2_4
call assert(expect, outs, "AND")

expect = IOR(op1_1, op2_1)
outs(1) = op1_1 .OR. op2_1
outs(2) = op1_1 .OR. op2_2
outs(3) = op1_1 .OR. op2_4
outs(4) = op1_2 .OR. op2_1
outs(5) = op1_2 .OR. op2_2
outs(6) = op1_2 .OR. op2_4
outs(7) = op1_4 .OR. op2_1
outs(8) = op1_4 .OR. op2_2
outs(9) = op1_4 .OR. op2_4

call assert(expect, outs, "OR")

expect = NOT(IEOR(op1_1, op2_1))
outs(1) = op1_1 .EQV. op2_1
outs(2) = op1_1 .EQV. op2_2
outs(3) = op1_1 .EQV. op2_4
outs(4) = op1_2 .EQV. op2_1
outs(5) = op1_2 .EQV. op2_2
outs(6) = op1_2 .EQV. op2_4
outs(7) = op1_4 .EQV. op2_1
outs(8) = op1_4 .EQV. op2_2
outs(9) = op1_4 .EQV. op2_4

call assert(expect, outs, "EQV")

expect = IEOR(op1_1, op2_1)
outs(1) = op1_1 .NEQV. op2_1
outs(2) = op1_1 .NEQV. op2_2
outs(3) = op1_1 .NEQV. op2_4
outs(4) = op1_2 .NEQV. op2_1
outs(5) = op1_2 .NEQV. op2_2
outs(6) = op1_2 .NEQV. op2_4
outs(7) = op1_4 .NEQV. op2_1
outs(8) = op1_4 .NEQV. op2_2
outs(9) = op1_4 .NEQV. op2_4

call assert(expect, outs, "NEQV")

!!! Logical -> Integer operand conversions
op1_1 = op1_1l
op1_2 = op1_2l
op1_4 = op1_4l

expect = IAND(op1_1, op2_1)
outs(1) = op1_1l .AND. op2_1 ! implicit conversions
outs(2) = op1_1l .AND. op2_2
outs(3) = op1_1l .AND. op2_4
outs(4) = op1_2l .AND. op2_1
outs(5) = op1_2l .AND. op2_2
outs(6) = op1_2l .AND. op2_4
outs(7) = op1_4l .AND. op2_1
outs(8) = op1_4l .AND. op2_2
outs(9) = op1_4l .AND. op2_4
call assert(expect, outs, "AND")

expect = IOR(op1_1, op2_1)
outs(1) = op1_1l .OR. op2_1 ! implicit conversions
outs(2) = op1_1l .OR. op2_2
outs(3) = op1_1l .OR. op2_4
outs(4) = op1_2l .OR. op2_1
outs(5) = op1_2l .OR. op2_2
outs(6) = op1_2l .OR. op2_4
outs(7) = op1_4l .OR. op2_1
outs(8) = op1_4l .OR. op2_2
outs(9) = op1_4l .OR. op2_4

call assert(expect, outs, "OR")

expect = NOT(IEOR(op1_1, op2_1))
outs(1) = op1_1l .EQV. op2_1 ! implicit conversions
outs(2) = op1_1l .EQV. op2_2
outs(3) = op1_1l .EQV. op2_4
outs(4) = op1_2l .EQV. op2_1
outs(5) = op1_2l .EQV. op2_2
outs(6) = op1_2l .EQV. op2_4
outs(7) = op1_4l .EQV. op2_1
outs(8) = op1_4l .EQV. op2_2
outs(9) = op1_4l .EQV. op2_4

call assert(expect, outs, "EQV")

expect = IEOR(op1_1, op2_1)
outs(1) = op1_1l .NEQV. op2_1 ! implicit conversions
outs(2) = op1_1l .NEQV. op2_2
outs(3) = op1_1l .NEQV. op2_4
outs(4) = op1_2l .NEQV. op2_1
outs(5) = op1_2l .NEQV. op2_2
outs(6) = op1_2l .NEQV. op2_4
outs(7) = op1_4l .NEQV. op2_1
outs(8) = op1_4l .NEQV. op2_2
outs(9) = op1_4l .NEQV. op2_4

call assert(expect, outs, "NEQV")


end
