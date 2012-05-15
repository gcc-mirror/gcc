! { dg-do compile }
! This tests the fix for PR25089 in which it was noted that a
! NAMELIST member that is an internal(or module) procedure gave
! no error if the NAMELIST declaration appeared before the
! procedure declaration. Not mentioned in the PR is that any
! reference to the NAMELIST object would cause a segfault.
!
! Based on the contribution from Joost VanderVondele
!
module M1
CONTAINS
! This is the original PR
  INTEGER FUNCTION G1()
    NAMELIST /NML1/ G2 ! { dg-error "PROCEDURE attribute conflicts" }
    G1=1
  END FUNCTION
  INTEGER FUNCTION G2()
    G2=1
  END FUNCTION
! This has always been picked up - namelist after function
  INTEGER FUNCTION G3()
    NAMELIST /NML2/ G1 ! { dg-error "PROCEDURE attribute conflicts" }
    G3=1
  END FUNCTION
END module M1
 
program P1
CONTAINS
! This has the additional wrinkle of a reference to the object.
  INTEGER FUNCTION F1()
    NAMELIST /NML3/ F2 ! { dg-error "PROCEDURE attribute conflicts" }
! Used to ICE here
    f2 = 1             ! { dg-error "is not a VALUE" }
    F1=1
  END FUNCTION
  INTEGER FUNCTION F2()
    F2=1
  END FUNCTION
END

