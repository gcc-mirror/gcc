! { dg-do run { target fd_truncate } }
!
! Test the fix for PR34875, in which the read with a vector index
! used to do nothing.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
Program QH0008

  REAL(4) QDA(10)
  REAL(4) QDA1(10)
! Scramble the vector up a bit to make the test more interesting
  integer, dimension(10) ::  nfv1 = (/9,2,1,3,5,4,6,8,7,10/)
! Set qda1 in ordinal order
  qda1(nfv1) = nfv1
  qda = -100
  OPEN (UNIT = 47,                &
        STATUS = 'SCRATCH',       &
        FORM = 'UNFORMATTED',     &
        ACTION = 'READWRITE')
  ISTAT = -314
  REWIND (47, IOSTAT = ISTAT)
  IF (ISTAT .NE. 0) STOP 1
  ISTAT = -314
! write qda1
  WRITE (47,IOSTAT = ISTAT) QDA1
  IF (ISTAT .NE. 0) STOP 2
  ISTAT = -314
  REWIND (47, IOSTAT = ISTAT)
  IF (ISTAT .NE. 0) STOP 3
! Do the vector index read that used to fail
  READ (47,IOSTAT = ISTAT) QDA(NFV1)
  IF (ISTAT .NE. 0) STOP 4
! Unscramble qda using the vector index
  IF (ANY (QDA(nfv1) .ne. QDA1) ) print *, qda, qda1
  ISTAT = -314
  REWIND (47, IOSTAT = ISTAT)
  IF (ISTAT .NE. 0) STOP 5
  qda = -200
! Do the subscript read that was OK
  READ (47,IOSTAT = ISTAT) QDA(1:10)
  IF (ISTAT .NE. 0) STOP 6
  IF (ANY (QDA .ne. QDA1) ) STOP 7
END

