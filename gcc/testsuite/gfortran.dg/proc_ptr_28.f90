! { dg-do compile }
!
! PR 44718: Procedure-pointer name is wrongly regarded as "external procedure"
!
! Contributed by John McFarland <john.mcfarland@swri.org>

MODULE m

 IMPLICIT NONE

CONTAINS

 FUNCTION func(x) RESULT(y)
   INTEGER :: x,y
   y = x *2
 END FUNCTION func

 SUBROUTINE sub(x)
   INTEGER :: x
   PRINT*, x
 END SUBROUTINE sub


 SUBROUTINE use_func()
   PROCEDURE(func), POINTER :: f
   INTEGER :: y
   f => func
   y = f(2)
 END SUBROUTINE use_func

 SUBROUTINE use_sub()
   PROCEDURE(sub), POINTER :: f
   f => sub
   CALL f(2)
 END SUBROUTINE use_sub

END MODULE m 

! { dg-final { cleanup-modules "m" } }
