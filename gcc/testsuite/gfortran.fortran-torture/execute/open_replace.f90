! pr 16196
! open with 'REPLACE' creates the file if it does not exist.
      PROGRAM iobug
      OPEN(UNIT=10,FILE='gfcoutput.txt',status='REPLACE')
      CLOSE(10,status='DELETE')
      END PROGRAM iobug
