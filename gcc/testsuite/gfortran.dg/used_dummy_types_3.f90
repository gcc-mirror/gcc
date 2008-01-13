! { dg-do compile }
! This checks the fix for PR20864 in which same name, USE associated
! derived types from different modules, with private components were
! not recognised to be different.
!
! Contributed by Joost VandVondele  <jv244@cam.ac.uk>
!==============
  MODULE T1
    TYPE data_type
      SEQUENCE
  ! private causes the types in T1 and T2 to be different 4.4.2
      PRIVATE
      INTEGER :: I
    END TYPE
  END MODULE

  MODULE T2
    TYPE data_type
      SEQUENCE
      PRIVATE
      INTEGER :: I
    END TYPE

  CONTAINS

    SUBROUTINE TEST(x)
      TYPE(data_type) :: x
    END SUBROUTINE TEST
  END MODULE

    USE T1
    USE T2 , ONLY : TEST
    TYPE(data_type) :: x
    CALL TEST(x)         ! { dg-error "Type mismatch in argument" }
  END

! { dg-final { cleanup-modules "T1 T2" } }
