! { dg-do compile }
! { dg-options "-std=gnu" }
!
! Tests the fix for PR25102, which did not diagnose the aberrant interface
! assignement below.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE TT
 TYPE data_type
   INTEGER :: I
 END TYPE data_type
 INTERFACE ASSIGNMENT (=)
   MODULE PROCEDURE set
 END INTERFACE
CONTAINS
  PURE SUBROUTINE set(x1,*) ! { dg-error "Alternate return cannot appear" }
    TYPE(data_type), INTENT(OUT) :: x1
    x1%i=0
  END SUBROUTINE set
END MODULE
