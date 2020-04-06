! { dg-do compile }
!
! PR fortran/94120
!
implicit none
integer :: i
contains
  subroutine f()
    !$acc declare copy(i)  ! { dg-error "Variable 'i' shall be declared in the same scoping unit as !.ACC DECLARE" }
  end
end
