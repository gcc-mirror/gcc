! test various forms of the DO statement
! inspired by PR14066
LOGICAL L
DO i=1,10
END DO
DO 10 i=1,20
   DO 20,j=1,10,2
20 CONTINUE
10 END DO
L = .TRUE.
DO WHILE(L)
   L = .FALSE.
END DO
DO 50 WHILE(.NOT.L)
   L = .TRUE.
50 CONTINUE
DO
   DO 30
      DO 40
40    CONTINUE
30 END DO
END DO
outer: DO i=1,20
   inner: DO,j=i,30
      IF (j.EQ.2*i) CYCLE outer
   END DO inner
END DO outer
END
