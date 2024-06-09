! { dg-do compile }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), pointer :: a
   end type
   type(t) :: z
   character((0.)/0), target :: c = 'abc' ! { dg-error "Arithmetic NaN" }
   z%a => c
! The associate statement was not needed to trigger the ICE.
   associate (y => z%a)
      print *, y
   end associate
end
