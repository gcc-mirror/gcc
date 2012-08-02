! { dg-do compile }
!
! PR 51081: [F03] Proc-pointer assignment: Rejects valid internal proc
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

procedure(), pointer :: p1
procedure(real), pointer :: p2
p1 => int2
p2 => scale   ! { dg-error "is invalid in procedure pointer assignment" }
contains
  subroutine int2()
    print *,"..."
  end subroutine
end
