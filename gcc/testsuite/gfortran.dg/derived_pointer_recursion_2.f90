! { dg-do run }
!
! PR 40594: [4.5 Regression] wrong-code
!
! Original test case by Daniel Franke <dfranke@gcc.gnu.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

MODULE atom_types

TYPE :: atom_list
  TYPE(atom_private), DIMENSION(:), pointer :: table
END TYPE

TYPE :: atom_private
  TYPE(atom_list) :: neighbors
  LOGICAL         :: initialized = .true.
END TYPE

TYPE :: atom_model
  TYPE(atom_list) :: atoms
  integer         :: dummy
END TYPE

contains

  SUBROUTINE init(this)
    TYPE(atom_private) :: this
    this%initialized = .FALSE.
  END SUBROUTINE

END MODULE


program pr40594

  USE atom_types
  TYPE(atom_model) :: am
  type(atom_private) :: ap

  am%dummy = 0

  call init(ap)
  if (ap%initialized .neqv. .false.) STOP 1

END
