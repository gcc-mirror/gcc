! { dg-do compile }
!
! Tests the fix for PR83999, where the invalid function 'f' caused an ICE.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer :: a
   end type
   type(t) :: x(3)
   x = f()
   print *, x
contains
   elemental function f() result(z) ! { dg-error "must have a scalar result" }
      type(t), pointer :: z(:)
   end
end
