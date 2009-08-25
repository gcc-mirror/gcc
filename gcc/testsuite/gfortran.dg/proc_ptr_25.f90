! { dg-do run }
!
! PR 41139: [4.5 Regression] a procedure pointer call as actual argument
!
! Original test case by Barron Bichon <barron.bichon@swri.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

PROGRAM test

 PROCEDURE(add), POINTER :: f
 logical :: g

 ! Passing the function works
 g=greater(4.,add(1.,2.))
 if (.not. g) call abort()

 ! Passing the procedure pointer fails
 f => add
 g=greater(4.,f(1.,2.))
 if (.not. g) call abort()

CONTAINS

 REAL FUNCTION add(x,y)
   REAL, INTENT(in) :: x,y
   print *,"add:",x,y
   add = x+y
 END FUNCTION add

 LOGICAL FUNCTION greater(x,y)
   REAL, INTENT(in) :: x, y
   greater = (x > y)
 END FUNCTION greater

END PROGRAM test

