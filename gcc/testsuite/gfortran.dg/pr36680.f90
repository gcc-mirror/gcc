! PR target/36680
! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-options "-O1 -fschedule-insns" }

MODULE class_dummy_atom_kdtree_types
  TYPE dummy_atom_kdtree_data
    INTEGER :: dummy
  END TYPE

  TYPE :: dummy_atom_kdtree_node
    TYPE(dummy_atom_kdtree_node_private), POINTER :: p
  END TYPE

  TYPE :: dummy_atom_kdtree_node_private
    TYPE(dummy_atom_kdtree_data)                  :: data
  END TYPE

  TYPE :: dummy_atom_kdtree
    TYPE(dummy_atom_kdtree_node) :: root
  END TYPE
END MODULE

FUNCTION dummy_atom_kdtree_insert(this, item)
  USE class_dummy_atom_kdtree_types

  TYPE(dummy_atom_kdtree), INTENT(inout)   :: this
  TYPE(dummy_atom_kdtree_data), INTENT(in) :: item

  TYPE(dummy_atom_kdtree_node) :: parent, current
  INTEGER :: cmp, level, discriminator

  parent        = dummy_atom_kdtree_node(null())
  current       = this%root
  level         = 1
  discriminator = 1

  DO WHILE (ASSOCIATED( current%p ))
    discriminator = MODULO(level-1, 3) + 1
    cmp = dummy_atom_kdtree_data_compare(item, current%p%data, discriminator)
    level   = level + 1
  END DO

END FUNCTION
