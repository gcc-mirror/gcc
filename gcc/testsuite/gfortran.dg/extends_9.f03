! { dg-do compile }
!
! PR42257: [OOP] Compiler segmentation fault due missing public statement
!
! Contributed by Oystein Olsen <oystein.olsen@astro.uio.no>

MODULE run_example_fortran03
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: epoch

  INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15,307)

  TYPE epoch
     INTEGER(I4B) :: i = 2451545
     REAL(DP)     :: f = 0.5_DP
  END TYPE

  TYPE, EXTENDS(epoch) :: time
     REAL(DP) :: t = 0.0_DP   
  END TYPE
END MODULE


  USE  run_example_fortran03
  IMPLICIT NONE

  CLASS(epoch), ALLOCATABLE :: e4

  ALLOCATE(epoch::e4)
  WRITE(*,*) e4%i, e4%f

END
 
