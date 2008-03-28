! { dg-do compile }
! This checks the fix for PR34910, in which the invalid reference
! below caused an ICE.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
MODULE foo
CONTAINS
  INTEGER FUNCTION f()
  f = 42
  CONTAINS
    LOGICAL FUNCTION f1()
      f1 = .TRUE.
    END FUNCTION

    LOGICAL FUNCTION f2()
      f1 = .FALSE.  ! { dg-error "is not a variable" }
    END FUNCTION
  END FUNCTION
END MODULE
! { dg-final { cleanup-modules "foo" } }
