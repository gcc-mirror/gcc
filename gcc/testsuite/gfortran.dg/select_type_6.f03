! { dg-do run }
!
! PR 41579: [OOP/Polymorphism] Nesting of SELECT TYPE
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

 type t1
 end type t1

 type, extends(t1) :: t2
  integer :: i
 end type t2

 type, extends(t1) :: t3
  integer :: j
 end type t3

 class(t1), allocatable :: mt2, mt3
 allocate(t2 :: mt2)
 allocate(t3 :: mt3)

 select type (mt2)
 type is(t2)
   mt2%i = 5
   print *,mt2%i
   select type(mt3)
   type is(t3)
     mt3%j = 2*mt2%i
     print *,mt3%j
     if (mt3%j /= 10) call abort()
   class default
     call abort()
   end select
 class default
   call abort()
 end select

end
