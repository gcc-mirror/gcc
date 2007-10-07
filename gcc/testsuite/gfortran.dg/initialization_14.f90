! { dg-do compile }
! PR 20851
! Dummy arguments are disallowed in initialization expressions in
! elemental functions except as arguments to the intrinsic functions
! BIT_SIZE, KIND, LEN, or to the numeric inquiry functions listed
! in 13.11.8
MODULE TT
INTEGER M
CONTAINS
   ELEMENTAL REAL FUNCTION two(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(N) :: scr ! { dg-error "Dummy argument 'n' not allowed in expression" }
   END FUNCTION

   ELEMENTAL REAL FUNCTION twopointfive(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(MAX(N,2)) :: scr ! { dg-error "Dummy argument 'n' not allowed in expression" }
   end FUNCTION twopointfive

   REAL FUNCTION three(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(N) :: scr ! this time it's valid
   END FUNCTION

   ELEMENTAL REAL FUNCTION four(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(bit_size(N)) :: scr ! another valid variant
   END FUNCTION

   ELEMENTAL REAL FUNCTION gofourit(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(MIN(HUGE(N),1)) :: scr ! another valid variant
   END FUNCTION

   ELEMENTAL REAL FUNCTION fourplusone(N)
     INTEGER, INTENT(IN) :: N
     INTEGER, DIMENSION(M) :: scr ! another valid variant
   END FUNCTION

   ELEMENTAL REAL FUNCTION five(X)
     real, intent(in) :: x
     CHARACTER(LEN=PRECISION(X)) :: C ! valid again
   END FUNCTION
END MODULE
END
