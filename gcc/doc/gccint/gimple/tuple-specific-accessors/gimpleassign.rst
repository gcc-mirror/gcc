..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_ASSIGN

GIMPLE_ASSIGN
^^^^^^^^^^^^^

.. function:: gassign *gimple_build_assign (tree lhs, tree rhs)

  Build a ``GIMPLE_ASSIGN`` statement.  The left-hand side is an lvalue
  passed in lhs.  The right-hand side can be either a unary or
  binary tree expression.  The expression tree rhs will be
  flattened and its operands assigned to the corresponding operand
  slots in the new statement.  This function is useful when you
  already have a tree expression that you want to convert into a
  tuple.  However, try to avoid building expression trees for the
  sole purpose of calling this function.  If you already have the
  operands in separate trees, it is better to use
  ``gimple_build_assign`` with ``enum tree_code`` argument and separate
  arguments for each operand.

.. function:: gassign *gimple_build_assign (tree lhs, enum tree_code subcode, tree op1, tree op2, tree op3)

  This function is similar to two operand ``gimple_build_assign``,
  but is used to build a ``GIMPLE_ASSIGN`` statement when the operands of the
  right-hand side of the assignment are already split into
  different operands.

  The left-hand side is an lvalue passed in lhs.  Subcode is the
  ``tree_code`` for the right-hand side of the assignment.  Op1, op2 and op3
  are the operands.

.. function:: gassign *gimple_build_assign (tree lhs, enum tree_code subcode, tree op1, tree op2)

  Like the above 5 operand ``gimple_build_assign``, but with the last
  argument ``NULL`` - this overload should not be used for
  ``GIMPLE_TERNARY_RHS`` assignments.

.. function:: gassign *gimple_build_assign (tree lhs, enum tree_code subcode, tree op1)

  Like the above 4 operand ``gimple_build_assign``, but with the last
  argument ``NULL`` - this overload should be used only for
  ``GIMPLE_UNARY_RHS`` and ``GIMPLE_SINGLE_RHS`` assignments.

.. function:: gimple gimplify_assign (tree dst, tree src, gimple_seq *seq_p)

  Build a new ``GIMPLE_ASSIGN`` tuple and append it to the end of
  ``*SEQ_P``.

``DST`` / ``SRC`` are the destination and source respectively.  You can
pass ungimplified trees in ``DST`` or ``SRC``, in which
case they will be converted to a gimple operand if necessary.

This function returns the newly created ``GIMPLE_ASSIGN`` tuple.

.. function:: enum tree_code gimple_assign_rhs_code (gimple g)

  Return the code of the expression computed on the ``RHS`` of
  assignment statement ``G``.

.. function:: enum gimple_rhs_class gimple_assign_rhs_class (gimple g)

  Return the gimple rhs class of the code for the expression
  computed on the rhs of assignment statement ``G``.  This will never
  return ``GIMPLE_INVALID_RHS``.

.. function:: tree gimple_assign_lhs (gimple g)

  Return the ``LHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_lhs_ptr (gimple g)

  Return a pointer to the ``LHS`` of assignment statement ``G``.

.. function:: tree gimple_assign_rhs1 (gimple g)

  Return the first operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs1_ptr (gimple g)

  Return the address of the first operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: tree gimple_assign_rhs2 (gimple g)

  Return the second operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs2_ptr (gimple g)

  Return the address of the second operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: tree gimple_assign_rhs3 (gimple g)

  Return the third operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs3_ptr (gimple g)

  Return the address of the third operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_lhs (gimple g, tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of assignment statement ``G``.

.. function:: void gimple_assign_set_rhs1 (gimple g, tree rhs)

  Set ``RHS`` to be the first operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_rhs2 (gimple g, tree rhs)

  Set ``RHS`` to be the second operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_rhs3 (gimple g, tree rhs)

  Set ``RHS`` to be the third operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: bool gimple_assign_cast_p (const_gimple s)

  Return true if ``S`` is a type-cast assignment.