! Check that inquiry functions are allowed as specification expressions.
subroutine inquiry(x1)
  implicit none
  real, dimension(1:), intent(out) :: x1
  real, dimension(1:size(x1)) :: x3
  x3 = 0
  x1 = x3
end subroutine
