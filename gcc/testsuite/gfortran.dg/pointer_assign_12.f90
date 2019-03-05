! { dg-do compile }
! PR 70260 - this used to ICE
! Original test case by Gehard Steinmetz
module m
   interface gkind
      procedure g
   end interface
contains
   integer function g()
      g => 1 ! { dg-error "Pointer assignment target cannot be a constant" }
   end
   subroutine f(x)
      character(kind=kind(gkind())) :: x
   end
end
