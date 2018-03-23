! { dg-do compile }
!
! Test the fix for PR83866.f90
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
program p
  type private
  end type
   type t
      class(t), pointer :: a
   end type
   type extends(t) :: t2 ! { dg-error "Garbage after | does not have a component" }
   end type
end
