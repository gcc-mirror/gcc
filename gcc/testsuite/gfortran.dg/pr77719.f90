! PR middle-end/77719
! { dg-do compile }
! { dg-options "-O3 -ffast-math" }

SUBROUTINE urep_egr(erep,derep,surr)
  INTEGER, PARAMETER :: dp=8
  REAL(dp), INTENT(inout)                  :: erep, derep(3)
  REAL(dp), INTENT(in)                     :: surr(2)
  REAL(dp)                                 :: de_z, rz
  INTEGER :: isp,spdim,jsp,nsp
  IF (n_urpoly > 0) THEN
    IF (r < spxr(1,1)) THEN
      ispg: DO isp = 1,spdim ! condition ca)
        IF (isp /= spdim) THEN
          nsp = 5 ! condition cb
          DO jsp = 0,nsp
            IF( jsp <= 3 ) THEN
            ELSE
              erep = erep + surr(jsp-3)*rz**(jsp)
            ENDIF
          END DO
        END IF
      END DO ispg
    END IF
  END IF
END SUBROUTINE urep_egr
