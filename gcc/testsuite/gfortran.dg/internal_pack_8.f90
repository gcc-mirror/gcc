! { dg-do run }
!
! Test the fix for PR43111, in which necessary calls to
! internal PACK/UNPACK were not being generated because
! of an over agressive fix to PR41113/7.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
SUBROUTINE S2(I)
 INTEGER :: I(4)
 !write(6,*) I
 IF (ANY(I.NE.(/3,5,7,9/))) CALL ABORT()
END SUBROUTINE S2

MODULE M1
 TYPE T1
  INTEGER, POINTER, DIMENSION(:) :: data
 END TYPE T1
CONTAINS
 SUBROUTINE S1()
   TYPE(T1) :: d
   INTEGER, TARGET, DIMENSION(10) :: scratch=(/(i,i=1,10)/)
   INTEGER :: i=2
   d%data=>scratch(1:9:2)
!   write(6,*) d%data(i:)
   CALL S2(d%data(i:))
 END SUBROUTINE S1
END MODULE M1

USE M1
CALL S1
END
! { dg-final { cleanup-modules "M1" } }
