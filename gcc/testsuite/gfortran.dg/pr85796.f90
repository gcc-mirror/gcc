! { dg-do compile }
! PR fortran/85796 - Floating point exception with implied do in data statement

program p
  implicit none
  integer :: i, j, x(2,2)
  data ((x(i,j),i=1,2,j-1),j=1,2) /3*789/ ! { dg-error "step of implied-do loop" }
end
