! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR39998: Procedure Pointer Assignments: Statement Functions & Internal Functions.
!
! Contributed by Tobias Burnus <burnus@net-b.de>

  procedure(), pointer :: p
  f(x) = x**2  ! { dg-warning "Obsolescent feature" }
  p => f  ! { dg-error "invalid in procedure pointer assignment" }
  p => sub  ! { dg-error "invalid in procedure pointer assignment" }
contains
  subroutine sub
  end subroutine sub
end

