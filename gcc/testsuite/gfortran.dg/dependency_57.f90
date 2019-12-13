! { dg-do compile }
! PR 92755 - this used to cause an ICE.
! Original test case by Gerhard Steinmetz
program p
   type t
   end type
   type t2
      class(t), allocatable :: a(:)
   end type
   type(t2) :: z
   z%a = [z%a]
end
