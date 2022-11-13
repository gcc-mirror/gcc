..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_TRY

GIMPLE_TRY
^^^^^^^^^^

.. function:: gtry *gimple_build_try (gimple_seq eval, gimple_seq cleanup, unsigned int kind)

  Build a ``GIMPLE_TRY`` statement.  ``EVAL`` is a sequence with the
  expression to evaluate.  ``CLEANUP`` is a sequence of statements to
  run at clean-up time.  ``KIND`` is the enumeration value
  ``GIMPLE_TRY_CATCH`` if this statement denotes a try/catch construct
  or ``GIMPLE_TRY_FINALLY`` if this statement denotes a try/finally
  construct.

.. function:: enum gimple_try_flags gimple_try_kind (gimple g)

  Return the kind of try block represented by ``GIMPLE_TRY`` ``G``. This is
  either ``GIMPLE_TRY_CATCH`` or ``GIMPLE_TRY_FINALLY``.

.. function:: bool gimple_try_catch_is_cleanup (gimple g)

  Return the ``GIMPLE_TRY_CATCH_IS_CLEANUP`` flag.

.. function:: gimple_seq gimple_try_eval (gimple g)

  Return the sequence of statements used as the body for ``GIMPLE_TRY``
  ``G``.

.. function:: gimple_seq gimple_try_cleanup (gimple g)

  Return the sequence of statements used as the cleanup body for
  ``GIMPLE_TRY`` ``G``.

.. function:: void gimple_try_set_catch_is_cleanup (gimple g, bool catch_is_cleanup)

  Set the ``GIMPLE_TRY_CATCH_IS_CLEANUP`` flag.

.. function:: void gimple_try_set_eval (gtry *g, gimple_seq eval)

  Set ``EVAL`` to be the sequence of statements to use as the body for
  ``GIMPLE_TRY`` ``G``.

.. function:: void gimple_try_set_cleanup (gtry *g, gimple_seq cleanup)

  Set ``CLEANUP`` to be the sequence of statements to use as the
  cleanup body for ``GIMPLE_TRY`` ``G``.