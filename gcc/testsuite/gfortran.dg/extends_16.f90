! { dg-do run }
!
! PR 57562: [OOP] ICE due to extended derived type with PARAMETER attribute
!
! Contributed by <helvio.vairinhos@gmail.com>

   type :: Parent
      integer :: member1 = 0
   end type

   type, extends(Parent) :: Child
      integer :: member2 = 0
   end type

   type, extends(Child) :: Grandchild
      integer :: member3 = 0
   end type

   type(Grandchild), parameter :: object = Grandchild(23, 42, -99)

   if (object%member1 /= 23) STOP 1
   if (object%member2 /= 42) STOP 1
   if (object%member3 /= -99) STOP 1

end
