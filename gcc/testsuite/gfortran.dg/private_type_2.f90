! { dg-do compile }
! { dg-options "-std=f95" }
! PR16404 test 6 - If a component of a derived type is of a type declared to
! be private, either the derived type definition must contain the PRIVATE
! statement, or the derived type must be private.
! Modified on 20051105 to test PR24534.
! Modified on 20090419 to use -std=f95, since F2003 allows public types
! with private components.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
MODULE TEST
  PRIVATE
  TYPE :: info_type
   INTEGER :: value
  END TYPE info_type
  TYPE :: all_type! { dg-error "PRIVATE type and cannot be a component" }
    TYPE(info_type) :: info
  END TYPE
  TYPE :: any_type! This is OK because of the PRIVATE statement.
    PRIVATE
    TYPE(info_type) :: info
  END TYPE
  public  all_type, any_type
END MODULE
END

! { dg-final { cleanup-modules "test" } }
