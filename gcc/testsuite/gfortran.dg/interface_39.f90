! { dg-do compile }
! PR68442
module m
   interface gkind
      procedure g
   end interface
contains
   subroutine f(x)
      character(kind=gkind()) :: x ! { dg-error "must be an intrinsic" }
   end
   integer function g()
      g = 1
   end
end
