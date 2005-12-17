! { dg-do run }
! PR 18710 : We used to not read and write the imaginary part of 
! complex numbers
       COMPLEX C, D
       COMPLEX(KIND=8) E, F

       OPEN(UNIT=9,FILE='PR18710',ACCESS='DIRECT',RECL=132)

       C = (120.0,240.0)
       WRITE(9,REC=1)C
       READ(9,REC=1)D
       if (c /= d) call abort()

       E = (120.0,240.0)
       WRITE(9,REC=1)E
       READ(9,REC=1)F
       if (E /= F) call abort()

       CLOSE(UNIT=9,STATUS='DELETE')
       END
