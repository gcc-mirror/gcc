! { dg-do compile }
! Tests the fix for 20871, in which pure non-intrinsic procedures were permitted to
! be dummy arguments.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE TT
CONTAINS
   ELEMENTAL INTEGER FUNCTION two(N)
     INTEGER, INTENT(IN) :: N
     two=2**N
   END FUNCTION
END MODULE
USE TT
 INTEGER, EXTERNAL  :: SUB
 write(6,*) SUB(two)    ! { dg-error "not allowed as an actual argument " }
END
INTEGER FUNCTION SUB(XX)
  INTEGER :: XX
  SUB=XX()
END

! { dg-final { cleanup-modules "TT" } }
