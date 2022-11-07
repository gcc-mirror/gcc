..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_BIND

GIMPLE_BIND
^^^^^^^^^^^

.. function:: gbind *gimple_build_bind (tree vars, gimple_seq body)

  Build a ``GIMPLE_BIND`` statement with a list of variables in ``VARS``
  and a body of statements in sequence ``BODY``.

.. function:: tree gimple_bind_vars (const gbind *g)

  Return the variables declared in the ``GIMPLE_BIND`` statement ``G``.

.. function:: void gimple_bind_set_vars (gbind *g, tree vars)

  Set ``VARS`` to be the set of variables declared in the ``GIMPLE_BIND``
  statement ``G``.

.. function:: void gimple_bind_append_vars (gbind *g, tree vars)

  Append ``VARS`` to the set of variables declared in the ``GIMPLE_BIND``
  statement ``G``.

.. function:: gimple_seq gimple_bind_body (gbind *g)

  Return the GIMPLE sequence contained in the ``GIMPLE_BIND`` statement
  ``G``.

.. function:: void gimple_bind_set_body (gbind *g, gimple_seq seq)

  Set ``SEQ`` to be sequence contained in the ``GIMPLE_BIND`` statement ``G``.

.. function:: void gimple_bind_add_stmt (gbind *gs, gimple stmt)

  Append a statement to the end of a ``GIMPLE_BIND`` 's body.

.. function:: void gimple_bind_add_seq (gbind *gs, gimple_seq seq)

  Append a sequence of statements to the end of a ``GIMPLE_BIND`` 's
  body.

.. function:: tree gimple_bind_block (const gbind *g)

  Return the ``TREE_BLOCK`` node associated with ``GIMPLE_BIND`` statement
  ``G``. This is analogous to the ``BIND_EXPR_BLOCK`` field in trees.

.. function:: void gimple_bind_set_block (gbind *g, tree block)

  Set ``BLOCK`` to be the ``TREE_BLOCK`` node associated with ``GIMPLE_BIND``
  statement ``G``.