! { dg-do run }
!
! Verifying the runtime behavior of the intrinsic function EXTENDS_TYPE_OF.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 implicit none

 intrinsic :: extends_type_of

 type :: t1
   integer :: i = 42
 end type

 type, extends(t1) :: t2
   integer :: j = 43
 end type

 type, extends(t2) :: t3
   class(t1),pointer :: cc
 end type

 class(t1), pointer :: c1,c2
 type(t1), target :: x
 type(t2), target :: y
 type(t3), target :: z
 
 c1 => x
 c2 => y
 z%cc => y

 if (.not. extends_type_of (c1, c1)) STOP 1
 if (      extends_type_of (c1, c2)) STOP 2
 if (.not. extends_type_of (c2, c1)) STOP 3

 if (.not. extends_type_of (x, x)) STOP 4
 if (      extends_type_of (x, y)) STOP 5
 if (.not. extends_type_of (y, x)) STOP 6

 if (.not. extends_type_of (c1, x)) STOP 7
 if (      extends_type_of (c1, y)) STOP 8
 if (.not. extends_type_of (x, c1)) STOP 9
 if (.not. extends_type_of (y, c1)) STOP 10

 if (.not. extends_type_of (z,   c1)) STOP 11
 if (      extends_type_of (z%cc, z)) STOP 12

end
