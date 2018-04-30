! { dg-do run }
!
! PR 41139: [4.5 Regression] a procedure pointer call as actual argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

PROGRAM test

 type :: t
   PROCEDURE(add), POINTER, nopass :: f
 end type
 type(t) :: o
 logical :: g

 o%f => add
 g=greater(4.,o%f(1.,2.))
 if (.not. g) STOP 1

CONTAINS

 REAL FUNCTION add(x,y)
   REAL, INTENT(in) :: x,y
   add = x+y
 END FUNCTION add

 LOGICAL FUNCTION greater(x,y)
   REAL, INTENT(in) :: x, y
   print *,"greater:",x,y
   greater = (x > y)
 END FUNCTION greater

END PROGRAM test

