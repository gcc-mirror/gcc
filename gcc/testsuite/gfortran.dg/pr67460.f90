! Bogus "all warnings being treated as errors"
! { dg-do compile }
! { dg-options "-std=f2003 -Werror" }
MODULE btree_i8_k_sp2d_v
  TYPE btree_node
     INTEGER id
     TYPE(btree_node_p), DIMENSION(:), POINTER :: subtrees
     TYPE(btree_node), POINTER :: parent
  END TYPE btree_node
  TYPE btree_node_p
     TYPE(btree_node), POINTER :: node
  END TYPE btree_node_p
CONTAINS
  RECURSIVE SUBROUTINE btree_verify_node (tree, node, level, nids, lastv,&
               count, num_nodes, max_leaf_level, min_leaf_level, printing)
    TYPE(btree_node), INTENT(IN)             :: node
    INTEGER                                  :: branch
    IF (ASSOCIATED (node%subtrees(branch)%node)) THEN
       IF (node%subtrees(branch)%node%parent%id .NE. node%id) THEN
          WRITE(*,*)'foo'
       ENDIF
    ENDIF
  END SUBROUTINE btree_verify_node
END MODULE btree_i8_k_sp2d_v
