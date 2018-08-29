! { dg-do compile }
! Tests the fix for PR30407, in which operator assignments did not work
! in WHERE blocks or simple WHERE statements.  This is the test provided
! by the reporter.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!==============================================================================

MODULE kind_mod

   IMPLICIT NONE

   PRIVATE

   INTEGER, PUBLIC, PARAMETER :: I4=SELECTED_INT_KIND(9)
   INTEGER, PUBLIC, PARAMETER :: TF=KIND(.TRUE._I4)

END MODULE kind_mod

!==============================================================================

MODULE pointer_mod

   USE kind_mod, ONLY : I4

   IMPLICIT NONE

   PRIVATE

   TYPE, PUBLIC :: pvt
      INTEGER(I4), POINTER, DIMENSION(:) :: vect
   END TYPE pvt

   INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE p_to_p
   END INTERFACE

   PUBLIC :: ASSIGNMENT(=)

CONTAINS

   !---------------------------------------------------------------------------

   PURE ELEMENTAL SUBROUTINE p_to_p(a1, a2)
      IMPLICIT NONE
      TYPE(pvt), INTENT(OUT) :: a1
      TYPE(pvt), INTENT(IN) :: a2
      a1%vect = a2%vect
   END SUBROUTINE p_to_p

   !---------------------------------------------------------------------------

END MODULE pointer_mod

!==============================================================================

PROGRAM test_prog

   USE pointer_mod, ONLY : pvt, ASSIGNMENT(=)

   USE kind_mod, ONLY : I4, TF

   IMPLICIT NONE

   INTEGER(I4), DIMENSION(12_I4), TARGET :: ia
   LOGICAL(TF), DIMENSION(2_I4,3_I4) :: la
   TYPE(pvt), DIMENSION(6_I4) :: pv
   INTEGER(I4) :: i

   ! Initialisation...
   la(:,1_I4:3_I4:2_I4)=.TRUE._TF
   la(:,2_I4)=.FALSE._TF

   DO i=1_I4,6_I4
      pv(i)%vect => ia((2_I4*i-1_I4):(2_I4*i))
   END DO

   ia=0_I4

   DO i=1_I4,3_I4
      WHERE(la((/1_I4,2_I4/),i))
         pv((2_I4*i-1_I4):(2_I4*i))= iaef((/(2_I4*i-1_I4),(2_I4*i)/))
      ELSEWHERE
         pv((2_I4*i-1_I4):(2_I4*i))= iaef((/0_I4,0_I4/))
      END WHERE
   END DO

   if (any (ia .ne. (/1,-1,2,-2,0,0,0,0,5,-5,6,-6/))) STOP 1

CONTAINS

   TYPE(pvt) ELEMENTAL FUNCTION iaef(index) RESULT(ans)

      USE kind_mod, ONLY :  I4
      USE pointer_mod, ONLY : pvt, ASSIGNMENT(=)

      IMPLICIT NONE

      INTEGER(I4), INTENT(IN) :: index

      ALLOCATE(ans%vect(2_I4))
      ans%vect=(/index,-index/)

   END FUNCTION iaef

END PROGRAM test_prog
