! Program to test array valued dummy functions
SUBROUTINE dummyfn(deriv)
   implicit none
   INTERFACE
      FUNCTION deriv()
         REAL :: deriv(4)
      END FUNCTION deriv
   END INTERFACE

   REAL :: dx(4)

   dx = deriv()
END SUBROUTINE
