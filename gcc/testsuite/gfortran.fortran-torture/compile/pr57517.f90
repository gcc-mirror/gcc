SUBROUTINE cal_helicity (uh, ph, phb, wavg, ims, ime, its, ite)
  INTEGER, INTENT( IN ) :: ims, ime, its, ite
  REAL, DIMENSION( ims:ime), INTENT( IN ) :: ph, phb, wavg
  REAL, DIMENSION( ims:ime), INTENT( INOUT ) :: uh
  INTEGER :: i
  REAL :: zu
  DO i = its, ite
    zu =  (ph(i ) + phb(i)) + (ph(i-1) + phb(i-1))
    IF (wavg(i) .GT. 0) THEN
      uh(i) = uh(i) + zu 
    ENDIF
  END DO
END SUBROUTINE cal_helicity
