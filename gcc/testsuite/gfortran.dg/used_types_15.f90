! { dg-do compile }
! Tests the fix for PR31086 in which the chained derived types
! was not being associated.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
MODULE class_dummy_atom_types
TYPE :: dummy_atom_list
  TYPE(dummy_atom), DIMENSION(:), POINTER :: table
END TYPE

TYPE :: dummy_atom
  TYPE(dummy_atom_list) :: neighbours
END TYPE

TYPE :: dummy_atom_model
  TYPE(dummy_atom_list) :: atoms
END TYPE
END MODULE

MODULE test_class_intensity_private
CONTAINS
  SUBROUTINE change_phase(atom)
    USE class_dummy_atom_types
    TYPE(dummy_atom), INTENT(inout) :: atom
  END SUBROUTINE

  SUBROUTINE simulate_cube()
    USE class_dummy_atom_types
    TYPE(dummy_atom)       :: atom
    TYPE(dummy_atom_model) :: dam
    atom = dam%atoms%table(1)
  END SUBROUTINE
END MODULE
! { dg-final { cleanup-modules "class_dummy_atom_types test_class_intensity_private" } }
