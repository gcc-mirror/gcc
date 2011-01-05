! { dg-do run }
!
! PR 47180: [OOP] EXTENDS_TYPE_OF returns the wrong result for disassociated polymorphic pointers 
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none

type t1
  integer :: a
end type t1

type, extends(t1):: t11
  integer :: b
end type t11

type(t1)  , target  :: a1
type(t11) , target  :: a11
class(t1) , pointer :: b1
class(t11), pointer :: b11

b1  => NULL()
b11 => NULL()

if (.not. extends_type_of(b1 , a1)) call abort()
if (.not. extends_type_of(b11, a1)) call abort()
if (.not. extends_type_of(b11,a11)) call abort()

b1  => a1
b11 => a11

if (.not. extends_type_of(b1 , a1)) call abort()
if (.not. extends_type_of(b11, a1)) call abort()
if (.not. extends_type_of(b11,a11)) call abort()

end
