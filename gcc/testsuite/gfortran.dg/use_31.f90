! { dg-do compile }
!
! PR fortran/107426
! This example used to generate an ICE, caused by the use stmt from the nested
! procedure declaration setting the result of the C_LOC global intrinsic symbol
! to the symbol of C_PTR from ISO_C_BINDING being imported, before freeing the
! latter symbol because of the rejection of the use statement.
!
! Contributed by Gerhard Steinmetz <gscfq@t-online.de>

module m
contains
   subroutine p() bind(c)
      use, intrinsic :: iso_c_binding
      integer, target :: a = 1
      type(c_ptr) :: z
      interface
         subroutine s(x) bind(cc)            ! { dg-error "Missing closing paren" }
            use, intrinsic :: iso_c_binding  ! { dg-error "Unexpected USE statement in INTERFACE block" }
            integer(c_int), value :: x       ! { dg-error "Parameter 'c_int' at .1. has not been declared" }
         end                                 ! { dg-error "END INTERFACE statement expected" }
      end interface
      z = c_loc(a)
      call s(z)
   end
end
