! { dg-do run }
! PR62125  Nested select type not accepted (rejects valid) 
module m
 implicit none
 type, abstract :: t1
  logical :: l
 end type t1
 type, extends(t1), abstract :: t2
  integer :: i
 end type t2
 type, extends(t2) :: t3
  real :: x
 end type t3
contains
 subroutine s(u)
  class(t1), intent(in) :: u
  if(.not.u%l) STOP 1
   select type(u); class is(t2)
     if(u%i.ne.2) STOP 2
     select type(u); class is(t3)
       if(u%x.ne.3.5) STOP 3
     end select
   end select
 end subroutine s
end module m

program p
 use m
 implicit none
 type(t3) :: var = t3( l=.true. , i=2 , x=3.5 )
 call s(var)
end program p
