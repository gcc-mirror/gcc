! { dg-do run }
! PR 92755 - this used to cause an ICE (see dependency_57.f90)
! PR83118 - fixed so that it would run :-)
! Original test case by Gerhard Steinmetz
program p
   type t
     integer :: i
   end type
   type t2
      class(t), allocatable :: a(:)
   end type
   type(t2) :: z
   z%a = [t(1),t(2),t(3)]
   z%a = [z%a]
   select type (y => z%a)
     type is (t)
       if (any (y%i .ne. [1, 2, 3])) stop 1
   end select
end
