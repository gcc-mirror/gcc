! { dg-do compile }
! { dg-options "-fcoarray=lib -lcaf_single " }
!
! Test the fix for PR83076
!
module m
   type t
      integer, pointer :: z
   end type
   type(t) :: ptr
contains
   function g(x)
      type(t) :: x[*]
      if (associated (x%z, ptr%z)) deallocate (x%z) ! This used to ICE with -fcoarray=lib
   end
end module

  use m
contains
   function f(x)
      type(t) :: x[*]
      if (associated (x%z, ptr%z)) deallocate (x%z)
   end
end
