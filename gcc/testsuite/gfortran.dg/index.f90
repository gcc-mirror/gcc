! { dg-do run }
! pr35940
      program FA1031
      implicit none
      integer I
      INTEGER IDA1(10)
      LOGICAL GDA1(10)
      INTEGER RSLT(10)
      DATA RSLT /4,1,4,1,4,1,4,1,4,1/
      IDA1   = 0
      gda1 = (/ (i/2*2 .ne. I, i=1,10) /)

      IDA1 = INDEX ( 'DEFDEF' , 'DEF', GDA1 )    !fails
      do I = 1, 10
         if (IDA1(i).NE.RSLT(i)) STOP 1
      end do
      IDA1 = INDEX ( (/ ('DEFDEF',i=1,10) /) , 'DEF', GDA1 )    !works
      do I = 1, 10
         if (IDA1(i).NE.RSLT(i)) STOP 2
      end do

      END
