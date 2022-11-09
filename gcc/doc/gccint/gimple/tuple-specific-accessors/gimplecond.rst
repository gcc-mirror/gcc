..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_COND

GIMPLE_COND
^^^^^^^^^^^

.. function:: gcond *gimple_build_cond ( enum tree_code pred_code, tree lhs, tree rhs, tree t_label, tree f_label)

  Build a ``GIMPLE_COND`` statement.  ``A`` ``GIMPLE_COND`` statement compares
  ``LHS`` and ``RHS`` and if the condition in ``PRED_CODE`` is true, jump to
  the label in ``t_label``, otherwise jump to the label in ``f_label``.
  ``PRED_CODE`` are relational operator tree codes like ``EQ_EXPR``,
  ``LT_EXPR``, ``LE_EXPR``, ``NE_EXPR``, etc.

.. function:: gcond *gimple_build_cond_from_tree (tree cond, tree t_label, tree f_label)

  Build a ``GIMPLE_COND`` statement from the conditional expression
  tree ``COND``.  ``T_LABEL`` and ``F_LABEL`` are as in ``gimple_build_cond``.

.. function:: enum tree_code gimple_cond_code (gimple g)

  Return the code of the predicate computed by conditional
  statement ``G``.

.. function:: void gimple_cond_set_code (gcond *g, enum tree_code code)

  Set ``CODE`` to be the predicate code for the conditional statement
  ``G``.

.. function:: tree gimple_cond_lhs (gimple g)

  Return the ``LHS`` of the predicate computed by conditional statement
  ``G``.

.. function:: void gimple_cond_set_lhs (gcond *g, tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of the predicate computed by
  conditional statement ``G``.

.. function:: tree gimple_cond_rhs (gimple g)

  Return the ``RHS`` operand of the predicate computed by conditional
  ``G``.

.. function:: void gimple_cond_set_rhs (gcond *g, tree rhs)

  Set ``RHS`` to be the ``RHS`` operand of the predicate computed by
  conditional statement ``G``.

.. function:: tree gimple_cond_true_label (const gcond *g)

  Return the label used by conditional statement ``G`` when its
  predicate evaluates to true.

.. function:: void gimple_cond_set_true_label (gcond *g, tree label)

  Set ``LABEL`` to be the label used by conditional statement ``G`` when
  its predicate evaluates to true.

.. function:: void gimple_cond_set_false_label (gcond *g, tree label)

  Set ``LABEL`` to be the label used by conditional statement ``G`` when
  its predicate evaluates to false.

.. function:: tree gimple_cond_false_label (const gcond *g)

  Return the label used by conditional statement ``G`` when its
  predicate evaluates to false.

.. function:: void gimple_cond_make_false (gcond *g)

  Set the conditional ``COND_STMT`` to be of the form 'if (1 == 0)'.

.. function:: void gimple_cond_make_true (gcond *g)

  Set the conditional ``COND_STMT`` to be of the form 'if (1 == 1)'.
