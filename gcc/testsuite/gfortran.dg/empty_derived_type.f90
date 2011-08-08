! { dg-do compile }
module stuff
   implicit none
   type, bind(C) :: junk ! { dg-warning "may be inaccessible by the C companion" }
      ! Empty!
   end type junk
end module stuff 

! { dg-final { cleanup-modules "stuff" } }
