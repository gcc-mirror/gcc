! { dg-do run }

! PR fortran/44047
! Double free happened, check that it works now.

! Contributed by Janus Weil, janus@gcc.gnu.org.

implicit none
type t0
 integer :: j = 42
end type t0
type t
 integer :: i
 class(t0), allocatable :: foo
end type t
type(t) :: m
allocate(t0 :: m%foo)
m%i = 5
select type(bar => m%foo)
type is(t0)
 print *, bar
 if (bar%j /= 42) STOP 1
end select
end
