! { dg-do compile }
! Tests the fix for PR29232, in which the invalid code below was not
! diagnosed.
!
! Contributed by Tobias Burnus  <tobias.burnus@physik.fu-berlin.de>
!
MODULE test
     TYPE vertex
           INTEGER :: k
     END TYPE vertex
CONTAINS
     SUBROUTINE S1()
         TYPE(vertex) :: a  ! { dg-error "cannot be host associated" }
         vertex : DO i=1,2  ! { dg-error "incompatible object of the same name" }
         ENDDO vertex
     END SUBROUTINE
END MODULE test
! { dg-final { cleanup-modules "test" } }
