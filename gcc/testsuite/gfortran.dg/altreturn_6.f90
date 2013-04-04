! { dg-do compile }
! { dg-options "-std=gnu" }
!
! PR 32938
subroutine r (*)
  integer(kind=8) :: i
  return i
end
