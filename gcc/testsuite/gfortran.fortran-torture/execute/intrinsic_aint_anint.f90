! Program to test AINT and ANINT intrinsics

subroutine real4test (op, res1, res2)
   implicit none
   real(kind=4) :: op
   real(kind=4) :: res1, res2

   if (diff(aint(op), res1) .or. &
       diff(anint(op), res2)) call abort
contains
function diff(a, b)
  real(kind=4) :: a, b
  logical diff

  diff = (abs (a - b) .gt. abs(a * 1e-6))
end function
end subroutine

subroutine real8test (op, res1, res2)
   implicit none
   real(kind=8) :: op
   real(kind=8) :: res1, res2

   if (diff(aint(op), res1) .or. &
       diff(anint(op), res2)) call abort
contains
function diff(a, b)
  real(kind=8) :: a, b
  logical diff

  diff = (abs(a - b) .gt. abs(a * 1e-6))
end function
end subroutine

program aint_aninttest
   implicit none

   call real4test (3.456, 3.0, 3.0)
   call real4test (-2.798, -2.0, -3.0)
   call real4test (3.678, 3.0, 4.0)
   call real4test (-1.375, -1.0, -1.0)
   call real4test (-0.5, 0.0,-1.0)
   call real4test (0.4, 0.0,0.0)

   call real8test (3.456_8, 3.0_8, 3.0_8)
   call real8test (-2.798_8, -2.0_8, -3.0_8)
   call real8test (3.678_8, 3.0_8, 4.0_8)
   call real8test (-1.375_8, -1.0_8, -1.0_8)
   call real8test (-0.5_8, 0.0_8,-1.0_8)
   call real8test (0.4_8, 0.0_8,0.0_8)

   ! Check large numbers
   call real4test (2e34, 2e34, 2e34)
   call real4test (-2e34, -2e34, -2e34)
end program
