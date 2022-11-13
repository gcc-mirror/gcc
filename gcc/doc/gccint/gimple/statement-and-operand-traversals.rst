..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Statement and operand traversals

.. _statement-and-operand-traversals:

Statement and operand traversals
********************************

There are two functions available for walking statements and
sequences: ``walk_gimple_stmt`` and ``walk_gimple_seq``,
accordingly, and a third function for walking the operands in a
statement: ``walk_gimple_op``.

.. function:: tree walk_gimple_stmt (gimple_stmt_iterator *gsi,   walk_stmt_fn callback_stmt, walk_tree_fn callback_op, struct walk_stmt_info *wi)

  This function is used to walk the current statement in ``GSI``,
  optionally using traversal state stored in ``WI``.  If ``WI`` is ``NULL``, no
  state is kept during the traversal.

  The callback ``CALLBACK_STMT`` is called.  If ``CALLBACK_STMT`` returns
  true, it means that the callback function has handled all the
  operands of the statement and it is not necessary to walk its
  operands.

  If ``CALLBACK_STMT`` is ``NULL`` or it returns false, ``CALLBACK_OP`` is
  called on each operand of the statement via ``walk_gimple_op``.  If
  ``walk_gimple_op`` returns non- ``NULL`` for any operand, the remaining
  operands are not scanned.

  The return value is that returned by the last call to
  ``walk_gimple_op``, or ``NULL_TREE`` if no ``CALLBACK_OP`` is specified.

.. function:: tree walk_gimple_op (gimple stmt,   walk_tree_fn callback_op, struct walk_stmt_info *wi)

  Use this function to walk the operands of statement ``STMT``.  Every
  operand is walked via ``walk_tree`` with optional state information
  in ``WI``.

  ``CALLBACK_OP`` is called on each operand of ``STMT`` via ``walk_tree``.
  Additional parameters to ``walk_tree`` must be stored in ``WI``.  For
  each operand ``OP``, ``walk_tree`` is called as:

  .. code-block:: c++

    walk_tree (&OP, CALLBACK_OP, WI, PSET)

  If ``CALLBACK_OP`` returns non- ``NULL`` for an operand, the remaining
  operands are not scanned.  The return value is that returned by
  the last call to ``walk_tree``, or ``NULL_TREE`` if no ``CALLBACK_OP`` is
  specified.

.. function:: tree walk_gimple_seq (gimple_seq seq,   walk_stmt_fn callback_stmt, walk_tree_fn callback_op, struct walk_stmt_info *wi)

  This function walks all the statements in the sequence ``SEQ``
  calling ``walk_gimple_stmt`` on each one.  ``WI`` is as in
  ``walk_gimple_stmt``.  If ``walk_gimple_stmt`` returns non- ``NULL``, the walk
  is stopped and the value returned.  Otherwise, all the statements
  are walked and ``NULL_TREE`` returned.