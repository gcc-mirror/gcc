! { dg-do run }
!
! Verifying the runtime behavior of the intrinsic function SAME_TYPE_AS.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t1
  integer :: i
 end type 

 type, extends(t1) :: t2
  integer :: j
 end type

 CLASS(t1), pointer :: c1,c2
 TYPE(t1), target :: x1
 TYPE(t2) ,target :: x2

 intrinsic :: SAME_TYPE_AS
 logical :: l

 c1 => NULL()

 l = SAME_TYPE_AS (x1,x1)
 print *,l
 if (.not.l) call abort()
 l = SAME_TYPE_AS (x1,x2)
 print *,l
 if (l) call abort()

 c1 => x1
 l = SAME_TYPE_AS (c1,x1)
 print *,l
 if (.not.l) call abort()
 l = SAME_TYPE_AS (c1,x2)
 print *,l
 if (l) call abort()

 c1 => x2
 c2 => x2
 l = SAME_TYPE_AS (c1,c2)
 print *,l
 if (.not.l) call abort()

 c1 => x1
 c2 => x2
 l = SAME_TYPE_AS (c1,c2)
 print *,l
 if (l) call abort()

end
