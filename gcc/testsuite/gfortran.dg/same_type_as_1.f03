! { dg-do compile }
!
! Error checking for the intrinsic function SAME_TYPE_AS.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t1
  integer :: i
 end type 

 type :: ts
  sequence
  integer :: j
 end type

 TYPE(t1) :: x1
 TYPE(ts) :: x2

 integer :: i

 print *, SAME_TYPE_AS (l,x1)   ! { dg-error "must be of a derived type" }
 print *, SAME_TYPE_AS (x1,x2)  ! { dg-error "must be of an extensible type" }

end
