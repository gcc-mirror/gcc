! { dg-do compile }
! { dg-options "-O3 -fprefetch-loop-arrays" }

MODULE cp_lbfgs
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE mainlb(n, m, x, l, u, nbd, f, g, factr, pgtol, ws, wy, &
       csave, lsave, isave, dsave)
    REAL(KIND=dp)                            :: x(n), l(n), u(n)
    REAL(KIND=dp) :: f, g(n), factr, pgtol, ws(n, m), wy(n, m), sy(m, m), &
      ss(m, m), wt(m, m), wn(2*m, 2*m), snd(2*m, 2*m), z(n), r(n), d(n), &
      t(n), wa(8*m)
    CHARACTER(len=60)                        :: task
    IF (task == 'START') THEN
       IF (task(1:5) == 'FG_LN') GOTO 666
    ENDIF
222 CONTINUE
    DO 40 i = 1, n
       d(i) = z(i) - x(i)
40  ENDDO
666 CONTINUE
    IF (info /= 0 .OR. iback >= 20) THEN
       CALL dcopy(n,r,1,g,1)
    ENDIF
    GOTO 222
  END SUBROUTINE mainlb
END MODULE cp_lbfgs

