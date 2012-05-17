! { dg-do compile }
!
! PR 44962: [OOP] ICE with specification expression SIZE(<CLASS>)
!
! Contributed by Satish.BD <bdsatish@gmail.com>


module array

type :: t_array
  real, dimension(10) :: coeff
contains
  procedure :: get_coeff
end type t_array

contains

function get_coeff(self) result(coeff)
  class(t_array), intent(in) :: self
  real, dimension(size(self%coeff)) :: coeff !! The SIZE here carashes !!
end function get_coeff

end module array
