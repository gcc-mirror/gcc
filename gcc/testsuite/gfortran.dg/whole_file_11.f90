! { dg-do compile }
! { dg-options "-fwhole-file" }
!
! Tests the fix PR40011 comment 16 in which the derived type lists in
! different program units were getting mixed up.
!
! Contributed by Daniel Franck  <dfranke@gcc.gnu.org>
!
MODULE module_foo
  TYPE :: foo_node
    TYPE(foo_node_private), POINTER :: p
  END TYPE

  TYPE :: foo_node_private
    TYPE(foo_node), DIMENSION(-1:1) :: link
  END TYPE

  TYPE :: foo
    TYPE(foo_node) :: root
  END TYPE
END MODULE

FUNCTION foo_insert()
  USE module_foo, ONLY: foo, foo_node

  INTEGER :: foo_insert
  TYPE(foo_node) :: parent, current
  INTEGER :: cmp

  parent  = current
  current = current%p%link(cmp)
END FUNCTION

FUNCTION foo_count()
  USE module_foo, ONLY: foo
  INTEGER :: foo_count
END FUNCTION
