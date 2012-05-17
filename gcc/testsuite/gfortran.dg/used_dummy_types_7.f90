! { dg-do compile }
! This tests a patch for a regression caused by the second part of
! the fix for PR30554.  The linked derived types dummy_atom and
! dummy_atom_list caused a segment fault because they do not have
! a namespace.
!
! Contributed by Daniel Franke <franke.daniel@gmail.com>
! 
MODULE types
TYPE :: dummy_atom_list
  TYPE(dummy_atom), DIMENSION(:), POINTER :: table => null()
END TYPE

TYPE :: dummy_atom
  TYPE(dummy_atom_private), POINTER :: p => null()
END TYPE

TYPE :: dummy_atom_private
  INTEGER                     :: id
END TYPE
END MODULE

MODULE atom
USE types, ONLY: dummy_atom
INTERFACE
  SUBROUTINE dummy_atom_insert_symmetry_mate(this, other)
    USE types, ONLY: dummy_atom
    TYPE(dummy_atom), INTENT(inout) :: this
    TYPE(dummy_atom), INTENT(in)    :: other
  END SUBROUTINE
END INTERFACE
END MODULE

MODULE list
INTERFACE
  SUBROUTINE dummy_atom_list_insert(this, atom2)
    USE types, ONLY: dummy_atom_list
    USE atom, ONLY: dummy_atom

    TYPE(dummy_atom_list), INTENT(inout) :: this
    TYPE(dummy_atom), INTENT(in)         :: atom2
  END SUBROUTINE
END INTERFACE
END MODULE
