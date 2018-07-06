! { dg-do run }
!
! PR 41714: [OOP] ALLOCATE SOURCE= does not properly copy the value from SOURCE
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

type t
  integer :: i
end type t
type, extends(t) :: t2
  integer :: j
end type t2

class(t), allocatable :: a
allocate(a, source=t2(1,2))
print *,a%i
if(a%i /= 1) STOP 1
select type (a)
  type is (t2)
     print *,a%j
     if(a%j /= 2) STOP 2
end select
end
