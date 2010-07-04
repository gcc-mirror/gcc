! { dg-do compile }
!
! PR 43244: Invalid statement misinterpreted as FINAL declaration
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none         
type particle
  integer :: ID
end type
type(particle), dimension(1,1:3)  :: finalState
finalstate(1,(/1:2/))%ID = (/1,103/)  ! { dg-error "Syntax error in array constructor" }
end
