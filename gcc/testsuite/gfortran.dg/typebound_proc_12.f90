! { dg-do compile }
! Test the fix for PR41258, where an ICE was caused by a search
! for a typebound procedure to resolve d%c%e
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  TYPE a
    TYPE(b), DIMENSION(:), POINTER :: c  ! { dg-error "type that has not been declared" }
  END TYPE
  TYPE(a), POINTER :: d
  CALL X(d%c%e)         ! { dg-error "before it is defined" }
end
