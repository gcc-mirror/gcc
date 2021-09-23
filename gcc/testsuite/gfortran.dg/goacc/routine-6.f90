! Check for invalid syntax with !$ACC ROUTINE.

module m
  integer m1int
contains
  subroutine subr5 (x) 
  implicit none
  !$acc routine (m) ! { dg-error "Invalid NAME 'm' in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  !$acc routine (subr5)
  !$acc routine (m1int) ! { dg-error "Invalid NAME 'm1int' in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  integer f_1 ! Referenced.
  !$acc routine (f_1)
  integer f_2 ! Not referenced.
  !$acc routine (f_2) ! { dg-error "NAME 'f_2' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  integer v_1
  !$acc routine (v_1) ! { dg-error "NAME 'v_1' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  integer, intent(inout) :: x
  !$acc routine (x) ! { dg-error "NAME 'x' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  v_1 = x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
     x = f_1(x) + v_1
  end if
  end subroutine subr5
end module m

program main
  implicit none
  !$acc routine (main) ! { dg-error "PROGRAM attribute conflicts with SUBROUTINE attribute in 'main'" }
  interface
    function subr6 (x) 
    !$acc routine (subr6) ! { dg-error "without list is allowed in interface" }
    integer, intent (in) :: x
    integer :: subr6
    end function subr6
  end interface
  integer, parameter :: n = 10
  integer :: a(n), i
  !$acc routine (n) ! { dg-error "NAME 'n' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  !$acc routine (a) ! { dg-error "NAME 'a' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  !$acc routine (i) ! { dg-error "NAME 'i' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  !$acc routine (subr1) ! { dg-error "Invalid NAME 'subr1' in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  external :: subr2
  !$acc routine (subr2)

  external :: R1, R2
  !$acc routine (R1 R2 R3) ! { dg-error "Syntax error in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\), expecting .\\). after NAME" }
  !$acc routine (R1, R2, R3) ! { dg-error "Syntax error in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\), expecting .\\). after NAME" }
  !$acc routine (R1)
  !$acc routine (R2)

  !$acc parallel
  !$acc loop
  do i = 1, n
     call subr1 (i)
     call subr2 (i)
  end do
  !$acc end parallel
end program main

! Ensure that we recover from incomplete function definitions.

integer function f1 ! { dg-error "Expected formal argument list in function definition" }
  !$acc routine ! { dg-error "Unclassifiable OpenACC directive" }
end function f1 ! { dg-error "Expecting END PROGRAM statement" }

subroutine subr1 (x) 
  !$acc routine
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr1

subroutine subr2 (x) 
  !$acc routine (subr1) ! { dg-error "Invalid NAME 'subr1' in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  integer, intent(inout) :: x
  !$acc routine (x) ! { dg-error "NAME 'x' does not refer to a subroutine or function in \\!\\\$ACC ROUTINE \\( NAME \\)" }
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr2

subroutine subr3 (x) 
  !$acc routine (subr3)
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     call subr4 (x)
  end if
end subroutine subr3

subroutine subr4 (x) 
  !$acc routine (subr4)
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr4

subroutine subr10 (x)
  !$acc routine (subr10) device ! { dg-error "Failed to match clause" }
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr10

subroutine subr20 (x)
  !$acc routine (subr20) nohost nohost ! { dg-error "Duplicated 'nohost' clause" }
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr20
