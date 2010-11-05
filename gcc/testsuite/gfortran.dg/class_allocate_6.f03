! { dg-do run }
!
! PR 46174: [OOP] ALLOCATE with SOURCE: Deep copy missing
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
type t
end type t

type, extends(t) :: t2
  integer, allocatable :: a(:)
end type t2

class(t), allocatable :: x, y
integer :: i

allocate(t2 :: x)
select type(x)
 type is (t2)
   allocate(x%a(10))
   x%a = [ (i, i = 1,10) ]
   print '(*(i3))', x%a
 class default
   call abort()
end select

allocate(y, source=x)

select type(x)
 type is (t2)
   x%a = [ (i, i = 11,20) ]
   print '(*(i3))', x%a
 class default
   call abort()
end select

select type(y)
 type is (t2)
   print '(*(i3))', y%a
   if (any (y%a /= [ (i, i = 1,10) ])) call abort()
 class default
   call abort()
end select

end
