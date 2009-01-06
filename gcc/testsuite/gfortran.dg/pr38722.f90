! PR rtl-optimization/38722
! { dg-do compile }
! { dg-options "-O1" }
SUBROUTINE foo(x, n, ga, gc, vr)
  TYPE pt
    DOUBLE PRECISION, DIMENSION (:, :, :), POINTER :: cr
  END TYPE pt
  TYPE pu
    TYPE(pt), POINTER :: pw
  END TYPE pu
  LOGICAL, INTENT(in) :: x, ga, gc
  INTEGER :: i, n
  LOGICAL :: dd, ep, fe
  TYPE(pu) :: vr
  TYPE(pu), DIMENSION(:), POINTER :: v
  IF (.NOT. fe) THEN
     IF (ga) THEN
        CALL bar (dd, ep, gc)
     END IF
     IF (x .AND. .NOT. ga) THEN
        IF (gc) THEN
          DO i=1,n
            CALL baz (v(i), x, gc)
            v(i)%pw%cr = 1.0
          END DO
          DO i=1,n
             IF (ep) THEN
                IF (dd) THEN
                   IF (i==1) THEN
                      v(i)%pw%cr=v(i)%pw%cr + vr%pw%cr
                   ENDIF
                END IF
             END IF
          END DO
        END IF
     ENDIF
  END IF
END SUBROUTINE foo
