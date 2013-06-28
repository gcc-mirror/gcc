! { dg-do compile }
!
! PR 39735: procedure pointer assignments: return value is not checked
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none
procedure(real(4)), pointer :: p1
procedure(integer), pointer :: p2
procedure(sub), pointer :: p3
procedure(), pointer :: p4
procedure(real(8)),pointer :: p5
real(4), external, pointer :: p6

! valid
p2 => iabs
p3 => sub
p4 => p3
p6 => p1

! invalid
p1 => iabs   ! { dg-error "Type mismatch in function result" }
p1 => p2     ! { dg-error "Type mismatch in function result" }
p1 => p5     ! { dg-error "Type mismatch in function result" }
p6 => iabs   ! { dg-error "Type mismatch in function result" }
p4 => p2     ! { dg-error "is not a subroutine" }

contains

  subroutine sub(i)
    integer :: i
  end subroutine

end
