! { dg-do compile }
! This checks the fix for PR34813 in which the error at line 17
! was not detected.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
SUBROUTINE kd_tree_init_default()
  TYPE :: kd_tree_node
    INTEGER :: dummy
  END TYPE

  TYPE :: kd_tree
    TYPE(kd_tree_node) :: root
  END TYPE

  TYPE(kd_tree)  :: tree
  tree = kd_tree(null()) ! { dg-error "neither a POINTER nor ALLOCATABLE" }
END SUBROUTINE
