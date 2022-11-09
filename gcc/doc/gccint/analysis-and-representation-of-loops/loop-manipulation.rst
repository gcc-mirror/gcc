..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Loop manipulation

.. _loop-manipulation:

Loop manipulation
*****************

The loops tree can be manipulated using the following functions:

* ``flow_loop_tree_node_add`` : Adds a node to the tree.

* ``flow_loop_tree_node_remove`` : Removes a node from the tree.

* ``add_bb_to_loop`` : Adds a basic block to a loop.

* ``remove_bb_from_loops`` : Removes a basic block from loops.

Most low-level CFG functions update loops automatically.  The following
functions handle some more complicated cases of CFG manipulations:

* ``remove_path`` : Removes an edge and all blocks it dominates.

* ``split_loop_exit_edge`` : Splits exit edge of the loop,
  ensuring that PHI node arguments remain in the loop (this ensures that
  loop-closed SSA form is preserved).  Only useful on GIMPLE.

Finally, there are some higher-level loop transformations implemented.
While some of them are written so that they should work on non-innermost
loops, they are mostly untested in that case, and at the moment, they
are only reliable for the innermost loops:

* ``create_iv`` : Creates a new induction variable.  Only works on
  GIMPLE.  ``standard_iv_increment_position`` can be used to find a
  suitable place for the iv increment.

* ``duplicate_loop_body_to_header_edge``,
  ``tree_duplicate_loop_body_to_header_edge`` : These functions (on RTL and
  on GIMPLE) duplicate the body of the loop prescribed number of times on
  one of the edges entering loop header, thus performing either loop
  unrolling or loop peeling.  ``can_duplicate_loop_p``
  (``can_unroll_loop_p`` on GIMPLE) must be true for the duplicated
  loop.

* ``loop_version`` : This function creates a copy of a loop, and
  a branch before them that selects one of them depending on the
  prescribed condition.  This is useful for optimizations that need to
  verify some assumptions in runtime (one of the copies of the loop is
  usually left unchanged, while the other one is transformed in some way).

* ``tree_unroll_loop`` : Unrolls the loop, including peeling the
  extra iterations to make the number of iterations divisible by unroll
  factor, updating the exit condition, and removing the exits that now
  cannot be taken.  Works only on GIMPLE.
