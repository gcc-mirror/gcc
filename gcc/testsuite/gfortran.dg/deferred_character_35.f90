! { dg-do compile }
!
! Test the fix for PR93794, where the ASSOCIATE statement ICED on the
! deferred character length, pointer component.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), pointer :: a
   end type
   type(t) :: z
   character(4), target :: c = 'abcd'
   z%a => c
   associate (y => z%a)
      print *, y
   end associate
end
