! { dg-do compile }
!
! PR fortran/39414: PROCEDURE statement double declaration bug
!
! Discovered by Paul Thomas <pault@gcc.gnu.org>
! Modified by Janus Weil <janus@gcc.gnu.org>


! forbidden

procedure(integer) :: a
integer :: a   ! { dg-error "already has basic type of" }

integer :: b
procedure(integer) :: b	  ! { dg-error "already has basic type of" }

procedure(iabs) :: c
integer :: c   ! { dg-error "may not have basic type of" }

integer :: d
procedure(iabs) :: d   ! { dg-error "already has basic type of" }

! allowed

integer :: e
procedure() :: e

procedure() :: f
integer :: f

end

