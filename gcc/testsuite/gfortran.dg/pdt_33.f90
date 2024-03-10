! { dg-do compile }
!
! PR fortran/106050
! The following used to trigger an error recovery ICE by releasing
! the symbol T before the symbol K which was leading to releasing
! K twice as it's in T's namespace.
!
! Contributed by G. Steinmetz <gscfq@t-online.de>

program p
   a = 1
   type t(k)                  ! { dg-error "Unexpected derived type declaration" }
      integer, kind :: k = 4  ! { dg-error "not allowed outside a TYPE definition" }
   end type                   ! { dg-error "Expecting END PROGRAM" }
end
