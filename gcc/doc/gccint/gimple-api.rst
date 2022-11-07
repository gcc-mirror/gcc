..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE API

.. _gimple-api:

GIMPLE API
**********

.. function:: tree gimple_simplify (enum tree_code, tree, tree, gimple_seq *, tree (*)(tree))
              tree gimple_simplify (enum tree_code, tree, tree, tree, gimple_seq *, tree (*)(tree))
              tree gimple_simplify (enum tree_code, tree, tree, tree, tree, gimple_seq *, tree (*)(tree))
              tree gimple_simplify (enum built_in_function, tree, tree, gimple_seq *, tree (*)(tree))
              tree gimple_simplify (enum built_in_function, tree, tree, tree, gimple_seq *, tree (*)(tree))
              tree gimple_simplify (enum built_in_function, tree, tree, tree, tree, gimple_seq *, tree (*)(tree))

  The main GIMPLE API entry to the expression simplifications mimicking
  that of the GENERIC fold_{unary,binary,ternary} functions.

thus providing n-ary overloads for operation or function.  The
additional arguments are a gimple_seq where built statements are
inserted on (if ``NULL`` then simplifications requiring new statements
are not performed) and a valueization hook that can be used to
tie simplifications to a SSA lattice.

In addition to those APIs ``fold_stmt`` is overloaded with
a valueization hook:

.. function:: fold_stmt (gimple_stmt_iterator *, tree (*)(tree));

On top of these a ``fold_buildN`` -like API for GIMPLE is introduced:

.. function:: tree gimple_build (gimple_seq *, location_t, enum tree_code, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_build (gimple_seq *, location_t, enum tree_code, tree, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_build (gimple_seq *, location_t, enum tree_code, tree, tree, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_build (gimple_seq *, location_t, enum built_in_function, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_build (gimple_seq *, location_t, enum built_in_function, tree, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_build (gimple_seq *, location_t, enum built_in_function, tree, tree, tree, tree, tree (*valueize) (tree) = NULL);
              tree gimple_convert (gimple_seq *, location_t, tree, tree);

which is supposed to replace ``force_gimple_operand (fold_buildN (...), ...)``
and calls to ``fold_convert``.  Overloads without the ``location_t``
argument exist.  Built statements are inserted on the provided sequence
and simplification is performed using the optional valueization hook.