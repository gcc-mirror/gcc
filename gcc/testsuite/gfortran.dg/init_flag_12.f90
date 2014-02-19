! { dg-do compile }
! { dg-options "-fno-automatic -finit-local-zero" }
!
! PR 55907: [4.7/4.8/4.9 Regression] ICE with -fno-automatic -finit-local-zero
!
! Contributed by J.R. Garcia <garcia.espinosa.jr@gmail.com>

subroutine cchaine (i)
  implicit none
  integer :: i
  character(len=i) :: chaine
  write(*,*) chaine
end subroutine 
