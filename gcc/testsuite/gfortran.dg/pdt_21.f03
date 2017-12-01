! { dg-do compile }
!
! Tests the fix for PR82606 comment #1.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t(a, b, *) ! { dg-error "A parameter name is required" }
      integer, kind :: a
      integer, len :: b
      real(a) :: r(b)
   end type
   type(t(8, 3)) :: x
   real(x%a) :: y
end
