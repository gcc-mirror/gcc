! { dg-do compile }
!
! PR fortran/94120
!
! BLOCK is not supported in OpenACC <= 3.0
!
subroutine f()
  block
    integer :: k
    !$acc declare copy(j)  ! { dg-error "Sorry, !.ACC DECLARE at .1. is not allowed in BLOCK construct" }
  end block
end
