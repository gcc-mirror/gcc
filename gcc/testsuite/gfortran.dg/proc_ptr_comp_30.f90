! { dg-do compile }
!
! PR 47768: ICE: printing a derived-type variable with proc-pointer components
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
  integer :: i = 3
  procedure(type(t)), pointer, nopass :: ppc
end type 

type(t) :: x

print *,x  ! { dg-error "cannot have procedure pointer components" }
end
