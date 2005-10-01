! { dg-do compile }
! PR16404 test 6 - A public type cannot have private-type components.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
MODULE TEST
  PRIVATE
  TYPE :: info_type
   INTEGER :: value
  END TYPE info_type
  TYPE :: all_type! { dg-error "PRIVATE type and cannot be a component" }
    TYPE(info_type) :: info
  END TYPE
  public  all_type
END MODULE
END

