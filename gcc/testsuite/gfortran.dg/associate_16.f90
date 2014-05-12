! { dg-do compile }
! PR 60834 - this used to ICE.

module m
  implicit none
  type :: t
    real :: diffusion=1.
  end type
contains
  subroutine solve(this, x)
    class(t), intent(in) :: this
    real, intent(in) :: x(:)
    integer :: i
    integer, parameter :: n(1:5)=[(i,i=1, 5)]

    associate( nu=>this%diffusion)
      associate( exponential=>exp(-(x(i)-n) ))
        do i = 1, size(x)
        end do
      end associate
    end associate
  end subroutine solve
end module m
