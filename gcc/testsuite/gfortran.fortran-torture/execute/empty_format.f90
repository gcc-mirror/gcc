! from NIST test FM406.FOR
         CHARACTER*10 A10VK
         A10VK = 'XXXXXXXXXX'
         WRITE(A10VK,39110) 
39110    FORMAT() 
!  
!  the empty format should fill the target of the internal
!  write with blanks.
!
         IF (A10VK.NE.'') THEN
!          PRINT*,A10VK 
           STOP 1
         ENDIF
         END
