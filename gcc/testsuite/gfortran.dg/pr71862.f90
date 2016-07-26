! { dg-do compile }
program p
   type t
      integer :: n = 0
      integer, pointer :: q => null()
   end type
   type(t) :: x
   print *, associated(x%q)
   x = f()
   print *, associated(x%q)
contains
   function f() result (z)    ! { dg-error "must be dummy, allocatable or pointer" }
      class(t) :: z
      print *, associated(z%q)
   end
end
