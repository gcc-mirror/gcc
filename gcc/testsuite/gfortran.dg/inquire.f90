! { dg-do run }
! check to see that you cannot open a direct access file
! for sequential i/o.
! derived from NIST test fm910.for
        IMPLICIT NONE
        CHARACTER*10 D4VK
        OPEN(UNIT=7, ACCESS='DIRECT',RECL=132,STATUS='SCRATCH')
        INQUIRE(UNIT=7,SEQUENTIAL=D4VK)
        CLOSE(UNIT=7,STATUS='DELETE')
        IF (D4VK.NE.'NO') CALL ABORT
        END
