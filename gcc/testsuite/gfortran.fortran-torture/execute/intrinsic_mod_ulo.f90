! Program to test MOD and MODULO intrinsics
subroutine integertest (ops, res)
   implicit none
   integer, dimension(2) :: ops
   integer, dimension(2) :: res

   if ((mod(ops(1), ops(2)) .ne. res(1)) .or. &
       (modulo(ops(1), ops(2)) .ne. res(2))) call abort
end subroutine

subroutine real4test (ops, res)
   implicit none
   real(kind=4), dimension(2) :: ops
   real(kind=4), dimension(2) :: res

   if (diff(mod(ops(1), ops(2)), res(1)) .or. &
       diff(modulo(ops(1), ops(2)), res(2))) call abort
contains
function diff(a, b)
  real(kind=4) :: a, b
  logical diff

  diff = (abs (a - b) .gt. abs(a * 1e-6))
end function
end subroutine

subroutine real8test (ops, res)
   implicit none
   real(kind=8), dimension(2) :: ops
   real(kind=8), dimension(2) :: res

   if (diff(mod(ops(1), ops(2)), res(1)) .or. &
       diff(modulo(ops(1), ops(2)), res(2))) call abort
contains
function diff(a, b)
  real(kind=8) :: a, b
  logical diff

  diff = (abs(a - b) .gt. abs(a * 1e-6))
end function
end subroutine

program mod_modulotest
   implicit none

   call integertest ((/8, 5/), (/3, 3/))
   call integertest ((/-8, 5/), (/-3, 2/))
   call integertest ((/8, -5/), (/3, -2/))
   call integertest ((/-8, -5/), (/-3, -3/))
   call integertest ((/ 2, -1/), (/0, 0/))

   call real4test ((/3.0, 2.5/), (/0.5, 0.5/))
   call real4test ((/-3.0, 2.5/), (/-0.5, 2.0/))
   call real4test ((/3.0, -2.5/), (/0.5, -2.0/))
   call real4test ((/-3.0, -2.5/), (/-0.5, -0.5/))
   call real4test ((/ 2.0, -1.0/), (/ 0.0, 0.0 /))

   call real8test ((/3.0_8, 2.5_8/), (/0.5_8, 0.5_8/))
   call real8test ((/-3.0_8, 2.5_8/), (/-0.5_8, 2.0_8/))
   call real8test ((/3.0_8, -2.5_8/), (/0.5_8, -2.0_8/))
   call real8test ((/-3.0_8, -2.5_8/), (/-0.5_8, -0.5_8/))
   call real8test ((/ 2.0_8, -1.0_8/), (/ 0.0_8, 0.0_8 /))
   
   ! Check large numbers
   call real4test ((/2e34, 1.0/), (/0.0, 0.0/))
   call real4test ((/2e34, 1.5e34/), (/0.5e34, 0.5e34/))
end program
