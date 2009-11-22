! { dg-do compile }
! Test the fix for PR40402, in which it was not detected that X
! is not a constant and so the DATA statement did not have
! a constant value expression.
!
! Modified dg-error for PR41807
!
! Contributed by Philippe Marguinaud <philippe.marguinaud@meteo.fr>
!
      TYPE POINT
        REAL :: X 
      ENDTYPE
      TYPE(POINT) :: P
      DATA P / POINT(1.+X) / ! { dg-error "non-constant initialization" }
      print *, p
      END
