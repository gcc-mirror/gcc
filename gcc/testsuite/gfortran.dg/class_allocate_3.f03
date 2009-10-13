! { dg-do run }
!
! PR 41581: [OOP] Allocation of a CLASS with SOURCE=<class> does not work
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

 type t
 end type t

 type,extends(t) :: t2
   integer :: i = 54
   real :: r = 384.02
 end type t2

 class(t), allocatable :: m1, m2

 allocate(t2 :: m2)
 select type(m2)
 type is (t2)
   print *, m2%i, m2%r
   if (m2%i/=54) call abort()
   if (abs(m2%r-384.02)>1E-3) call abort()
   m2%i = 42
   m2%r = -4.0
 class default
   call abort()
 end select

 allocate(m1, source=m2)
 select type(m1)
 type is (t2)
   print *, m1%i, m1%r
   if (m1%i/=42) call abort()
   if (abs(m1%r+4.0)>1E-3) call abort()
 class default
   call abort()
 end select

end
