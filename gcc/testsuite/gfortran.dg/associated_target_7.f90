! { dg-do run }
!
! associated_target_7.f90: Test the fix for PR98565.
!
! Contributed by Yves Secretan  <yves.secretan@ete.inrs.ca>
!
MODULE PS_SN0N_M

   IMPLICIT NONE
   PRIVATE

   TYPE, PUBLIC :: DT_GRID_T
       INTEGER :: NNT
   CONTAINS
       ! PASS
   END TYPE DT_GRID_T

   TYPE, PUBLIC :: LM_ELEM_T
       CLASS(DT_GRID_T), POINTER :: PGRID
   CONTAINS
       PROCEDURE, PUBLIC :: REQPGRID => LM_ELEM_REGPGRID
   END TYPE LM_ELEM_T

   TYPE, PUBLIC :: PS_SN0N_T
      CLASS(DT_GRID_T), POINTER :: PGRID

   CONTAINS
      PROCEDURE, PUBLIC :: ASGOELE  => PS_SN0N_ASGOELE
   END TYPE PS_SN0N_T


CONTAINS
   !------------------------------------------------------------------------
   !------------------------------------------------------------------------
   FUNCTION LM_ELEM_REGPGRID(SELF) RESULT(PGRID)
   CLASS(DT_GRID_T), POINTER :: PGRID
   CLASS(LM_ELEM_T), INTENT(IN) :: SELF
   PGRID => SELF%PGRID
   RETURN
   END FUNCTION LM_ELEM_REGPGRID

   !------------------------------------------------------------------------
   !------------------------------------------------------------------------
   FUNCTION PS_SN0N_ASGOELE(SELF, OELE) RESULT(ERMSG)

   INTEGER :: ERMSG
   CLASS(PS_SN0N_T), INTENT(IN) :: SELF
   CLASS(LM_ELEM_T), INTENT(IN) :: OELE

   !CLASS(DT_GRID_T), POINTER :: PGRID
   LOGICAL :: ISOK
   !------------------------------------------------------------------------

   ! ASSOCIATED with temp variable compiles
   !PGRID => OELE%REQPGRID()
   !ISOK = ASSOCIATED(SELF%PGRID, PGRID)

   ! ASSOCIATE without temp variable crashes with ICE
   ISOK = ASSOCIATED(SELF%PGRID, OELE%REQPGRID())
   ERMSG = 0
   IF (ISOK) ERMSG = 1

   RETURN
   END FUNCTION PS_SN0N_ASGOELE

END MODULE PS_SN0N_M


   USE PS_SN0N_M
   CLASS(PS_SN0N_T), ALLOCATABLE :: SELF
   CLASS(LM_ELEM_T), ALLOCATABLE :: OELE
   TYPE (DT_GRID_T), TARGET :: GRID1 = DT_GRID_T (42)
   TYPE (DT_GRID_T), TARGET :: GRID2 = DT_GRID_T (84)

   ALLOCATE (PS_SN0N_T :: SELF)
   ALLOCATE (LM_ELEM_T :: OELE)
   SELF%PGRID => GRID1

   OELE%PGRID => NULL ()
   IF (SELF%ASGOELE (OELE) .NE. 0) STOP 1

   OELE%PGRID => GRID2
   IF (SELF%ASGOELE (OELE) .NE. 0) STOP 2

   OELE%PGRID => GRID1
   IF (SELF%ASGOELE (OELE) .NE. 1) STOP 3
END
