! { dg-do compile }
! { dg-options "-g" }
program p
   type t
      integer :: a
      integer :: b(0)
   end type
   type(t), parameter :: z = t(1, [2])
   print *, z
end
