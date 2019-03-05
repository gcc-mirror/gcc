! { dg-do compile }
program p
   type t
   end type
   class(t) :: x     ! { dg-error "must be dummy, allocatable or pointer" }
   associate (y => x)
   end associate
end
