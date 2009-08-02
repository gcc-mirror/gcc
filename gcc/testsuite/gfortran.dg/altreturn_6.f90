! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR 32938
subroutine r (*)
  integer(kind=8) :: i
  return i
end
