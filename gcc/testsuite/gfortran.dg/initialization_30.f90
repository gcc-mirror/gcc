! { dg-do compile }
! { dg-options "-std=f95" }
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
END MODULE
END
