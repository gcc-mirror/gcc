! { dg-do compile }
program p
   type t
      integer, allocatable :: a(:)
   end type
   type u
      real x
      type(t) y
   end type
   type(t) :: z
   type(u) :: q
   data z%a(1) / 789 /     ! { dg-error "Allocatable component" }
   data q%y%a(1) / 789 /   ! { dg-error "Allocatable component" }
end
