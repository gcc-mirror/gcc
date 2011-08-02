! { dg-do compile }
! Tests fix for PR25058 in which references to dummy
! parameters before the entry would be missed.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE M1
CONTAINS
FUNCTION F1(I) RESULT(RF1)
 INTEGER :: I,K,RE1,RF1
 RE1=K ! { dg-error "before the ENTRY statement" }
 RETURN
 ENTRY E1(K) RESULT(RE1)
 RE1=-I
 RETURN
END FUNCTION F1
END  MODULE M1
END

! { dg-final { cleanup-modules "m1" } }
