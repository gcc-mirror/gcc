! { dg-do compile }
! { dg-options "" }
module stuff
   implicit none
   type, bind(C) :: junk ! { dg-warning "may be inaccessible by the C companion" }
      ! Empty!
   end type junk
end module stuff 
