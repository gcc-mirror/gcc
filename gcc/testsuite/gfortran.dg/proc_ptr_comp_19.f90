! { dg-do run }
!
! PR 41139: [4.5 Regression] a procedure pointer call as actual argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

PROGRAM test

 type :: t
   PROCEDURE(three), POINTER, nopass :: f
 end type
 type(t) :: o
 logical :: g

 o%f => three
 g=greater(4.,o%f())
 if (.not. g) STOP 1

CONTAINS

 REAL FUNCTION three()
   three = 3.
 END FUNCTION

 LOGICAL FUNCTION greater(x,y)
   REAL, INTENT(in) :: x, y
   print *,"greater:",x,y
   greater = (x > y)
 END FUNCTION greater

END PROGRAM test

