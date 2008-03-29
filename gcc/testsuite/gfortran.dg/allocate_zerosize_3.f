C { dg-do run }
C Test the fix for PR35698, in which the negative size dimension would
C throw out the subsequent bounds.
C
C Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
C
      program try_lf0030
      call LF0030(10)
      end

      SUBROUTINE LF0030(nf10)
      INTEGER ILA1(7)
      INTEGER ILA2(7)
      LOGICAL LLA(:,:,:,:,:,:,:)
      INTEGER ICA(7)
      ALLOCATABLE LLA


      ALLOCATE (LLA(2:3, 4, 0:5,
     $          NF10:1, -2:7, -3:8,
     $          -4:9))

      ILA1 = LBOUND(LLA)
      ILA2 = UBOUND(LLA)
C     CORRECT FOR THE ZERO DIMENSIONED TERM TO ALLOW AN EASIER VERIFY
      ILA1(4) = ILA1(4) - 2    !   1 - 2 = -1
      ILA2(4) = ILA2(4) + 6    !   0 + 6 = 6     

      DO J1 = 1,7
      IVAL = 3-J1
      IF (ILA1(J1) .NE. IVAL) call abort ()
  100 ENDDO

      DO J1 = 1,7
      IVAL = 2+J1
      IF (ILA2(J1) .NE. IVAL) call abort ()
  101 ENDDO

      END SUBROUTINE
      