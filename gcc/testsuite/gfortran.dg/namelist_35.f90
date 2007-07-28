! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/31818 - accepts namelists with assumed-shape arrays
!

subroutine test(cha)
  implicit none
  character(len=10) :: cha(:)
  namelist /z/  cha             ! { dg-error "must not have assumed shape" }
end subroutine test
