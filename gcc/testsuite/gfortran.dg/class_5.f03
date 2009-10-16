! { dg-do compile }
!
! PR 41719: [OOP] invalid: Intrinsic assignment involving polymorphic variables
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

 implicit none

 type t1
   integer :: a
 end type

 type, extends(t1) :: t2
   integer :: b
 end type

 class(t1),pointer :: cp
 type(t2) :: x

 x = t2(45,478)
 allocate(t2 :: cp)

 cp = x   ! { dg-error "Variable must not be polymorphic" }

 select type (cp)
 type is (t2)
   print *, cp%a, cp%b
 end select

end
 