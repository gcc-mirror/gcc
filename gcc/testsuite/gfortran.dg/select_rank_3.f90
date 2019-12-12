! { dg-do compile }
!
! Test the fix for PR91729
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
subroutine s(x)
   integer :: x(..)
   select rank (-x)       ! { dg-error "must be an assumed rank" }
     rank (1)             ! { dg-error "Unexpected RANK statement" }
       print *, x         ! { dg-error "may only be used as actual argument" }
   end select             ! { dg-error "Expecting END SUBROUTINE" }
end

subroutine t(x)
   integer :: x(..)
   select rank (z => -x)  ! { dg-error "must be an assumed rank" }
     rank (1)             ! { dg-error "Unexpected RANK statement" }
       print *, z
   end select             ! { dg-error "Expecting END SUBROUTINE" }
end
