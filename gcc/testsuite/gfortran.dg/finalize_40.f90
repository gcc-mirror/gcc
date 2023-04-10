! { dg-do run }
!
! Test that PR67471 is fixed. Used not to call the finalizer.
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
!
module test_final_mod
  implicit none
  type :: my_final
    integer :: n = 1
  contains
    final :: destroy_scalar, destroy_rank1_array
  end type my_final
  integer :: final_calls = 0
contains
  subroutine destroy_rank1_array(self)
    type(my_final), intent(inout) :: self(:)
    if (size(self) /= 0) then
      if (size(self) /= 2) stop 1
      if (any (self%n /= [3,4])) stop 2
    else
      stop 3
    end if
    final_calls = final_calls + 1
  end subroutine destroy_rank1_array

! Eliminate the warning about the lack of a scalar finalizer.
  subroutine destroy_scalar(self)
    type(my_final), intent(inout) :: self
    final_calls = final_calls + self%n
  end subroutine destroy_scalar

end module test_final_mod

program test_finalizer
  use test_final_mod
  implicit none
  type(my_final) :: b(4), c(2)

  b%n = [2, 3, 4, 5]
  c%n = [6, 7]
  b(2:3) = c
  if (final_calls /= 1) stop 4
end program test_finalizer
