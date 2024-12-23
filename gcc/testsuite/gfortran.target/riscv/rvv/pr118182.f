! { dg-do run }
! { dg-options "-fno-vect-cost-model" }

	program dqnorm_calculator
	implicit none

	  ! Declare variables
	  integer, parameter :: nx = 33, ny = 33, nz =16
	  real(8) :: dq(5, nx, ny, nz)
	  real(8) :: result, expected_result, tolerance
	  integer :: i, j, k, l
	
	  ! Initialize the dq array with values calculated as k + j + i + 5
	  do k = 1, nz
	     do j = 1, ny
	        do i = 1, nx
	           do l = 1, 5
	              dq(l, i, j, k) = k + j + i + 5
	           end do
	        end do
	   end do
	end do

	! Call the subroutine to calculate the norm
	call redsum(dq, nx, ny, nz, result)

	  ! Check the result
	  expected_result = 214213560.0d0
	  tolerance = 0.0001d0
	  if (abs(result - expected_result) > tolerance) then
	     print *, "Result is incorrect: ", result
	     call abort()
	  end if
	end

	subroutine redsum(dq, nx, ny, nz, result)
	implicit none

	  ! Declare arguments and local variables
	  integer, intent(in) :: nx, ny, nz
	  real(8), intent(in) :: dq(5, nx, ny, nz)
	  real(8), intent(out) :: result
	  real(8) :: dqnorm
	  integer :: i, j, k, l

	  ! Initialize dqnorm
	  dqnorm = 0.0d0

	  ! Compute the sum of squares of dq elements
	  do k = 1, nz
	     do j = 1, ny
	        do i = 1, nx
	           do l = 1, 5
	              dqnorm = dqnorm + dq(l, i, j, k) * dq(l, i, j, k)
	           end do
	        end do
	     end do
	  end do

	  result = dqnorm

	end subroutine redsum

