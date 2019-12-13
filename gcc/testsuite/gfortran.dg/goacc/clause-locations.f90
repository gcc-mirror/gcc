! Verify that the location information for clauses is correct.
! See also PR 92793.

subroutine check_clause_columns ()
  implicit none (type, external)
  integer :: i, j, sum, diff

  !$acc parallel
    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:diff) reduction(-:sum)  ! { dg-warning "47: conflicting reduction operations for .sum." }
      do j = 1, 10
            sum = 1
      end do
    end do
  !$acc end parallel
end subroutine check_clause_columns

