
module m
  integer m1int
contains
  subroutine subr5 (x) 
  implicit none
  !$acc routine (subr5)
  !$acc routine (m1int) ! { dg-error "invalid function name" }
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
  end subroutine subr5
end module m

program main
  implicit none
  interface
    function subr6 (x) 
    !$acc routine (subr6) ! { dg-error "without list is allowed in interface" }
    integer, intent (in) :: x
    integer :: subr6
    end function subr6
  end interface
  integer, parameter :: n = 10
  integer :: a(n), i
  !$acc routine (subr1) ! { dg-error "invalid function name" }
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
  !$acc routine (subr1) ! { dg-error "invalid function name" }
  integer, intent(inout) :: x
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
  !$acc routine (subr10) device ! { dg-error "Unclassifiable OpenACC directive" }
  integer, intent(inout) :: x
  if (x < 1) then
     x = 1
  else
     x = x * x - 1
  end if
end subroutine subr10
