! { dg-do compile }
! Tests the fix for PR30554, the USE statements in potential_energy
! would cause a segfault because the pointer_info for nfree coming
! from constraint would not find the existing symtree coming directly
! from atom.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

MODULE ATOMS
INTEGER :: NFREE = 0
END MODULE ATOMS

MODULE CONSTRAINT
USE ATOMS, ONLY: NFREE
CONTAINS
   SUBROUTINE ENERGY_CONSTRAINT ( HESSIAN )
   REAL , DIMENSION(1:(3*NFREE*(3*NFREE+1))/2):: HESSIAN
   END SUBROUTINE ENERGY_CONSTRAINT
END MODULE CONSTRAINT

MODULE POTENTIAL_ENERGY
USE ATOMS
USE CONSTRAINT,         ONLY : ENERGY_CONSTRAINT
END MODULE POTENTIAL_ENERGY
! { dg-final { cleanup-modules "atoms constraint potential_energy" } }
