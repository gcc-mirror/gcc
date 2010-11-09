! { dg-do compile }
!
! PR 46313: [OOP] OOP-ABI issue, ALLOCATE issue, CLASS renaming issue
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m1
  type mytype
    real :: a(10) = 2
  end type
end module m1

module m2
  type mytype
    real :: b(10) = 8
  end type
end module m2

program p
use m1, t1 => mytype
use m2, t2 => mytype
implicit none

class(t1), allocatable :: x
class(t2), allocatable :: y

allocate (t1 :: x)
allocate (t2 :: y)

print *, x%a
print *, y%b
end

! { dg-final { cleanup-modules "m1 m2" } }
