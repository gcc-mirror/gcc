! { dg-do compile } 

SUBROUTINE check_dnucint_ana (dcore)
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=8
  REAL(dp), DIMENSION(10, 2), INTENT(IN),&
       OPTIONAL                            :: dcore
  INTEGER                                  :: i, j
  REAL(dp)                                 :: delta, nssss, od, rn, ssssm, &
       ssssp
  REAL(dp), DIMENSION(10, 2)               :: corem, corep, ncore
  LOGICAL                                  :: check_value

  delta = 1.0E-8_dp
  od = 0.5_dp/delta
  ncore = od * (corep - corem)
  nssss = od * (ssssp - ssssm)
  IF (PRESENT(dcore)) THEN
     DO i = 1, 2
        DO j = 1, 10
           IF (.NOT.check_value(ncore(j,i), dcore(j,i), delta, 0.1_dp)) THEN
           END IF
        END DO
     END DO
  END IF
END SUBROUTINE check_dnucint_ana

! { dg-final { cleanup-tree-dump "vect" } } 
