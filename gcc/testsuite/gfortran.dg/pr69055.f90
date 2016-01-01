! { dg-do compile }
! { dg-options "-fsanitize=float-cast-overflow" }

subroutine pr69055
  implicit none
  integer :: n
  real(8) :: b
  b = huge(1.0D0)
  n = b
end subroutine pr69055
