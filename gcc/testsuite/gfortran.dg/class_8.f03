! { dg-do compile }
! Test fixes for PR41618.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
!
 type t1
   integer :: comp
   class(t1),pointer :: cc
 end type

 class(t1) :: x ! { dg-error "must be dummy, allocatable or pointer" }

 x%comp = 3
 print *,x%comp

end
