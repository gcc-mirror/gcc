! { dg-do compile }
! { dg-options "-march=rv64gcv -mabi=lp64d -Ofast -std=legacy" }

MODULE a
  REAL b
CONTAINS
  SUBROUTINE c(d,KTE)
    REAL,DIMENSION(KTE) :: d,e,f,g
    REAL,DIMENSION(KTE) :: h
    i : DO j=1,b
       z=k
       DO l=m,n
          IF(o>=p)THEN
             IF(l<L0)THEN
                q=z/0
             ENDIF
             e=q
             f=EXP(r            )
          ENDIF
       ENDDO
       s : DO t=1,2
          DO l=m,u
             v=v+l
          ENDDO
          IF(w<=x)THEN
             DO l=w,x
                g=y
             ENDDO
          ENDIF
       ENDDO  s
       aa=v
       ab=ac/aa
       k=ad/ab
    ENDDO  i
    IF(ae>af)THEN
       DO l=m,n
          d=h
       ENDDO
    ENDIF
  END SUBROUTINE c
END MODULE a
