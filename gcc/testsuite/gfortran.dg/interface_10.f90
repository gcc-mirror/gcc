! { dg-do compile }
! Test the fix for PR20896 in which the ambiguous use
! of p was not detected.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  INTERFACE g
    SUBROUTINE s1(p) ! { dg-error "is already being used" }
      INTERFACE
        SUBROUTINE p
        END
      END INTERFACE
    END
    SUBROUTINE s2(p) ! { dg-error "Global name" }
      INTERFACE
        REAL FUNCTION p()
        END
      END INTERFACE
    END
  END INTERFACE

      INTERFACE
        REAL FUNCTION x()
        END
      END INTERFACE
      INTERFACE
        SUBROUTINE y
        END
      END INTERFACE
  call g (x)
  call g (y)
  END

