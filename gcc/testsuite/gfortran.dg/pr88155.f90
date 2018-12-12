! { dg-do compile }
program p
   type t
      integer :: a
   end type
   type(t) :: x
   data x /t()1/     ! { dg-error "No initializer for component" }
   print *, x
end
