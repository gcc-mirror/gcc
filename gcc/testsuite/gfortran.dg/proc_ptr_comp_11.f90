! { dg-do run }
!
! PR 40427: Procedure Pointer Components with OPTIONAL arguments
!
! Original test case by John McFarland <john.mcfarland@swri.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

PROGRAM prog

 ABSTRACT INTERFACE
 SUBROUTINE sub_template(i,j,o)
   INTEGER, INTENT(in) :: i
   INTEGER, INTENT(in), OPTIONAL :: j, o
 END SUBROUTINE sub_template
 END INTERFACE

 TYPE container
   PROCEDURE(sub_template), POINTER, NOPASS :: s
 END TYPE container

 PROCEDURE(sub_template), POINTER :: f
 TYPE (container) :: c

 c%s => sub
 f => sub

 CALL f(2,o=4)
 CALL c%s(3,o=6)

CONTAINS

 SUBROUTINE sub(i,arg2,arg3)
   INTEGER, INTENT(in) :: i
   INTEGER, INTENT(in), OPTIONAL :: arg2, arg3
   if (present(arg2)) STOP 1
   if (.not. present(arg3)) STOP 2
   if (2*i/=arg3) STOP 3
 END SUBROUTINE sub

END PROGRAM prog

