! { dg-do compile }
!
! Test the fix for all three errors in PR82586
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
module m
   type t(a)                 ! { dg-error "does not have a component" }
   end type
end

program p
   type t(a                  ! { dg-error "Expected parameter list" }
      integer, kind :: a
   end type
   type u(a, a)              ! { dg-error "Duplicate name" }
      integer, kind :: a     ! { dg-error "already declared" }
      integer, len :: a      ! { dg-error "already declared" }
   end type
end
