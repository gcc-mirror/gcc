SUBROUTINE XXX (IL, IU)
  implicit none
  integer, INTENT(IN) :: IL, IU

  integer :: NXX (14) = (/ 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14 /)
  integer :: ivvv, ia, ja, iaii
  logical :: qop

  QOP=.FALSE.

  DO IA=IL,IU
    JA=NXX(IA)
    IF (.NOT. QOP .and. JA.GT.0) THEN
      IAII=IA
      QOP=.TRUE.
    ENDIF

    IF (QOP) THEN
      ivvv=IA-IAII+1       ! mis-compiled
    ENDIF
  ENDDO

  IF (ivvv.NE.2) THEN
    STOP 1
  ENDIF
END subroutine

program p
  implicit none
  CALL XXX (1, 3)
end
