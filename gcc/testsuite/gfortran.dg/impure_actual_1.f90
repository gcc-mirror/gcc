! { dg-do compile }
! Tests the fix for PR25056 in which a non-PURE procedure could be
! passed as the actual argument to a PURE procedure.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE M1
CONTAINS
 FUNCTION L()
  L=1
 END FUNCTION L
 PURE FUNCTION J(K)
   INTERFACE
     PURE FUNCTION K()
     END FUNCTION K
   END INTERFACE
   J=K()
 END FUNCTION J
END MODULE M1
USE M1
 write(6,*) J(L) ! { dg-error "Mismatch in PURE attribute" }
END

! { dg-final { cleanup-modules "m1" } }

