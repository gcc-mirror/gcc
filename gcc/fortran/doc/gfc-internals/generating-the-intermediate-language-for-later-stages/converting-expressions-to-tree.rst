..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _converting-expressions:

Converting Expressions to tree
******************************

Converting expressions to ``tree`` is done by functions called
``gfc_conv_*``.

The central data structure for a GENERIC expression is the
``gfc_se`` structure.  Its ``expr`` member is a ``tree`` that
holds the value of the expression.  A ``gfc_se`` structure is
initialized using ``gfc_init_se`` ; it needs to be embedded in an
outer ``gfc_se``.

Evaluating Fortran expressions often require things to be done before
and after evaluation of the expression, for example code for the
allocation of a temporary variable and its subsequent deallocation.
Therefore, ``gfc_se`` contains the members ``pre`` and
``post``, which point to ``stmt_block`` blocks for code that
needs to be executed before and after evaluation of the expression.

When using a local ``gfc_se`` to convert some expression, it is
often necessary to add the generated ``pre`` and ``post`` blocks
to the ``pre`` or ``post`` blocks of the outer ``gfc_se``.
Code like this (lifted from :samp:`trans-expr.cc`) is fairly common:

.. code-block:: c++

  gfc_se cont_se;
  tree cont_var;

  /* cont_var = is_contiguous (expr); .  */
  gfc_init_se (&cont_se, parmse);
  gfc_conv_is_contiguous_expr (&cont_se, expr);
  gfc_add_block_to_block (&se->pre, &(&cont_se)->pre);
  gfc_add_modify (&se->pre, cont_var, cont_se.expr);
  gfc_add_block_to_block (&se->pre, &(&cont_se)->post);

Conversion functions which need a ``gfc_se`` structure will have a
corresponding argument.

``gfc_se`` also contains pointers to a ``gfc_ss`` and a
``gfc_loopinfo`` structure.  These are needed by the scalarizer.