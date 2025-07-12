! { dg-do compile }

subroutine x
  integer :: var
  integer, parameter :: ilog = 0
  integer, parameter :: array(*) = [11,22,33]
  !$ACC DECLARE COPYIN(ilog, array, var, array) ! { dg-error "Symbol 'array' present on multiple clauses" }
end subroutine x

integer :: a
integer, parameter :: b = 4
integer, parameter :: c(*) = [1,2,3]

!$acc parallel copy(a,c,b,c)  ! { dg-error "Symbol 'c' present on multiple clauses" }
!$acc end parallel
end
