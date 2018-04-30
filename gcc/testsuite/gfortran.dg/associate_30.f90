! { dg-do compile }
!
! Test the fix for PR67543
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
   subroutine s1
      associate (x => null())   ! { dg-error "cannot be NULL()" }
      end associate
   end subroutine

   subroutine s2
      associate (x => [null()]) ! { dg-error "has no type" }
      end associate
   end subroutine
