! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 20163, first half:  Trailing blanks on an option to
!                        open used to cause an error
      CHARACTER*8 ST
      ST = 'SCRATCH '
      OPEN(UNIT=10,STATUS=ST)
      END
