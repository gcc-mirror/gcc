! Tests the fix for PR33334, in which the TYPE in the function
! declaration cannot be legally accessed.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module types
 implicit none
 type t
   integer :: i = 99
 end type t
end module

module x
 use types
 interface
   type(t) function bar() ! { dg-error "is not accessible" }
   end function
 end interface
end module
! { dg-final { cleanup-modules "types x" } }

