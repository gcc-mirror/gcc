! { dg-do run }
!
! Allocating CLASS variables.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 implicit none

 type t1
   integer :: comp = 5
   class(t1),pointer :: cc
 end type

 type, extends(t1) :: t2
   integer :: j
 end type

 type, extends(t2) :: t3
   integer :: k
 end type

 class(t1),pointer :: cp, cp2
 type(t3) :: x
 integer :: i


 ! (1) check that vindex is set correctly (for different cases)

 i = 0
 allocate(cp)
 select type (cp)
 type is (t1)
   i = 1
 type is (t2)
   i = 2
 type is (t3)
   i = 3
 end select
 deallocate(cp)
 if (i /= 1) call abort()

 i = 0
 allocate(t2 :: cp)
 select type (cp)
 type is (t1)
   i = 1
 type is (t2)
   i = 2
 type is (t3)
   i = 3
 end select
 deallocate(cp)
 if (i /= 2) call abort()

 i = 0
 allocate(cp, source = x)
 select type (cp)
 type is (t1)
   i = 1
 type is (t2)
   i = 2
 type is (t3)
   i = 3
 end select
 deallocate(cp)
 if (i /= 3) call abort()

 i = 0
 allocate(t2 :: cp2)
 allocate(cp, source = cp2)  ! { dg-warning "not supported yet" }
 select type (cp)
 type is (t1)
   i = 1
 type is (t2)
   i = 2
 type is (t3)
   i = 3
 end select
 deallocate(cp)
 deallocate(cp2)
 if (i /= 2) call abort()


 ! (2) check initialization (default initialization vs. SOURCE)

 allocate(cp)
 if (cp%comp /= 5) call abort()
 deallocate(cp)

 x%comp = 4
 allocate(cp, source=x)
 if (cp%comp /= 4) call abort()
 deallocate(cp)

end
