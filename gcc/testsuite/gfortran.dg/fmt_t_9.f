! { dg-options "-ffixed-line-length-none -std=gnu" }
! { dg-do run }
! PR78123 Short reads with T edit descriptor not padding correctly
      PROGRAM tformat
C
      INTEGER  MXFLTL
      PARAMETER (MXFLTL = 99999)
      INTEGER   IFLGHT, NFLCYC, IFLTSQ(MXFLTL), IDXBLK, LMAX, LMIN, I
C
      OPEN(29, status='scratch')
      WRITE(29, '(a)') "   1   1   1  TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT      72  122     4"
      WRITE(29, '(a)') ""
      WRITE(29, '(a)') "  451   402012011201120112011200120112011201120112011201120111971201120112011201120112011201"
      WRITE(29, '(a)') "  451   4020 866 866 866 866 866 866 866 866 865 866 865 866 866 866 866 866 866 866 865 866"
      REWIND(29)
C     The error occurs in the following loop:
 10   CONTINUE
         READ(29,1010   )  IDXBLK, LMAX, LMIN
1010     FORMAT(8X,I4,T51,2I5) ! wrong if this format is used
c         write(6,fmt='("IDXBLK,LMAX,LMIN=",3I5)')IDXBLK,LMAX,LMIN
         IF (IDXBLK .EQ. 0)  GO TO 20
      GO TO 10
C
 20   CONTINUE
      READ(29,1040,END=100)  IFLGHT, NFLCYC,
     &              (IFLTSQ(I), I=1,NFLCYC)
1040  FORMAT(I5,I5,2X,(T13,20I4))
c      write(6,fmt='(2i6)') IFLGHT,NFLCYC
c      write(6,fmt='(20I4)') (IFLTSQ(I), I=1,NFLCYC)
c      write(6,*) "Program is correct"
      close(29)
      if (IFLGHT.ne.451) STOP 1
      if (NFLCYC.ne.40) STOP 2
      stop
C
 100  CONTINUE
C      write(6,*) "End of file encountered (wrong)"
      close (29)
      STOP 3
      STOP
      END
