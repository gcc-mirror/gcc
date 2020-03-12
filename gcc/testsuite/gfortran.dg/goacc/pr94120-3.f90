! { dg-do compile }
!
! PR fortran/94120
!
! Note: BLOCK is not supported in OpenACC <= 3.0 – but the following check comes earlier:
! It is also invalid because the variable is in a different scoping unit
!
subroutine g()
  integer :: k
  block
    !$acc declare copy(k)  ! { dg-error "Variable 'k' shall be declared in the same scoping unit as !.ACC DECLARE" }
  end block
end
