! { dg-do run }
!
! PR 39630: [F03] Procedure Pointer Components with PASS
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
 type :: t
  sequence
  integer :: i
  procedure(foo), pointer,pass(y) :: foo
 end type t
contains
 subroutine foo(x,y)
  type(t),optional :: x
  type(t) :: y
  if(present(x)) then
    print *, 'foo', x%i, y%i
    if (mod(x%i+y%i,3)/=2) STOP 1
  else
    print *, 'foo', y%i
    if (mod(y%i,3)/=1) STOP 2
  end if
 end subroutine foo
end module m

use m
type(t) :: t1, t2
t1%i = 4
t2%i = 7
t1%foo => foo
t2%foo => t1%foo
call t1%foo()
call t2%foo()
call t2%foo(t1)
end
