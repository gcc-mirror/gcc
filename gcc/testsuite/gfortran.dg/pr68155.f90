! { dg-do run }
!
! Fix for PR68155 in which initializers of constant length, character
! components of derived types were not being padded if they were too short.
! Originally, mismatched lengths caused ICEs. This seems to have been fixed
! in 9-branch.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
program p
  implicit none
  type t
    character(3) :: c1(2) = [                 'b', 'c']          ! OK
    character(3) :: c2(2) = [ character(1) :: 'b', 'c'] // ""    ! OK
    character(3) :: c3(2) = [                 'b', 'c'] // ""    ! was not padded
    character(3) :: c4(2) = [                 '' , '' ] // ""    ! was not padded
    character(3) :: c5(2) = [                 'b', 'c'] // 'a'   ! was not padded
    character(3) :: c6(2) = [                 'b', 'c'] // 'ax'  ! OK
    character(3) :: c7(2) = [                 'b', 'c'] // 'axy' ! OK trimmed
  end type t
  type(t)      :: z
  if (z%c1(2) .ne. 'c  ') stop 1
  if (z%c2(2) .ne. 'c  ') stop 2
  if (z%c3(2) .ne. 'c  ') stop 3
  if (z%c4(2) .ne. '   ') stop 4
  if (z%c5(2) .ne. 'ca ') stop 5
  if (z%c6(2) .ne. 'cax') stop 6
  if (z%c7(2) .ne. 'cax') stop 7
end
