! { dg-do compile }
! Tests the fix for PR31550 in which pointers to derived type components
! were being TREE-SSA declared in the wrong order and so in the incorrect
! context.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
MODULE class_dummy_atom_types
TYPE :: dummy_atom_list
  TYPE(dummy_atom), DIMENSION(:), POINTER :: table
  INTEGER                                 :: nused
END TYPE

TYPE :: dummy_atom
  TYPE(dummy_atom_private), POINTER :: p
END TYPE

TYPE :: dummy_atom_private
  TYPE(dummy_atom_list)       :: neighbors
END TYPE
END MODULE

MODULE class_dummy_atom_list
USE class_dummy_atom_types, ONLY: dummy_atom_list

INTERFACE 
  SUBROUTINE dummy_atom_list_init_copy(this, other)
    USE class_dummy_atom_types, ONLY: dummy_atom_list
    TYPE(dummy_atom_list), INTENT(out) :: this
    TYPE(dummy_atom_list), INTENT(in)  :: other
  END SUBROUTINE
END INTERFACE

INTERFACE
  SUBROUTINE dummy_atom_list_merge(this, other)
    USE class_dummy_atom_types, ONLY: dummy_atom_list
    TYPE(dummy_atom_list), INTENT(inout) :: this
    TYPE(dummy_atom_list), INTENT(in)    :: other
  END SUBROUTINE
END INTERFACE
END MODULE

SUBROUTINE dummy_atom_list_init_copy(this, other)
  USE class_dummy_atom_list, ONLY: dummy_atom_list, dummy_atom_list_merge

  TYPE(dummy_atom_list), INTENT(out) :: this
  TYPE(dummy_atom_list), INTENT(in)  :: other

  this%table(1:this%nused) = other%table(1:other%nused)
END SUBROUTINE
