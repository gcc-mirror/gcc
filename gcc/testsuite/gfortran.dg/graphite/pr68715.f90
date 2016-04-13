! { dg-do compile }
! { dg-options "-floop-nest-optimize -O1" }

SUBROUTINE se_core_core_interaction(calculate_forces)
  INTEGER, PARAMETER :: dp=8
  LOGICAL, INTENT(in)		 :: calculate_forces
  REAL(KIND=dp), DIMENSION(3)	 :: force_ab, rij
  LOGICAL :: lfoo,kfoo,mfoo,nfoo,ffoo
  INTEGER, PARAMETER :: mi2=42
  CALL dummy(lfoo,kfoo,mfoo,nfoo,method_id,core_core)
  IF (lfoo) THEN
     DO WHILE (ffoo())
	IF (lfoo) CYCLE
	IF (kfoo) CYCLE
	dr1 = DOT_PRODUCT(rij,rij)
	IF (dr1 > rij_threshold) THEN
	   SELECT CASE (method_id)
	   CASE (mi2)
	      IF (calculate_forces) THEN
		 CALL dummy2(force_ab)
		 IF (nfoo) THEN
		    force_ab = force_ab + core_core*dr3inv
		 END IF
	      END IF
	   END SELECT
	END IF
	enuclear = enuclear + enucij
     END DO
     CALL dummy3(enuclear)
  END IF
END SUBROUTINE se_core_core_interaction
