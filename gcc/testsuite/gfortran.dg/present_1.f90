! { dg-do compile }
! Test the fix for PR25097, in which subobjects of the optional dummy argument
! could appear as argument A of the PRESENT intrinsic.
! 
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
 MODULE M1
  TYPE T1
   INTEGER :: I
  END TYPE T1
 CONTAINS
  SUBROUTINE S1(D1)
   TYPE(T1), OPTIONAL :: D1(4)
   write(6,*) PRESENT(D1%I)  ! { dg-error "must not be a sub-object" }
   write(6,*) PRESENT(D1(1)) ! { dg-error "must not be a sub-object" }
   write(6,*) PRESENT(D1)
  END SUBROUTINE S1
 END MODULE
 END

