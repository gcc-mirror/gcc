! Test if variable appearing in multiple clauses are errors.

! { dg-do compile }

program combined
  implicit none
  integer a(100), i, j

  !$acc parallel loop reduction (+:j) copy (j) copyout(j) ! { dg-error "Symbol 'j' present on multiple clauses" }
  do i = 1, 100
  end do
  !$acc end parallel loop
end program combined
