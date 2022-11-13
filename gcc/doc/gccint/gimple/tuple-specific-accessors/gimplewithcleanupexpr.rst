..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_WITH_CLEANUP_EXPR

GIMPLE_WITH_CLEANUP_EXPR
^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: gimple gimple_build_wce (gimple_seq cleanup)

  Build a ``GIMPLE_WITH_CLEANUP_EXPR`` statement.  ``CLEANUP`` is the
  clean-up expression.

.. function:: gimple_seq gimple_wce_cleanup (gimple g)

  Return the cleanup sequence for cleanup statement ``G``.

.. function:: void gimple_wce_set_cleanup (gimple g, gimple_seq cleanup)

  Set ``CLEANUP`` to be the cleanup sequence for ``G``.

.. function:: bool gimple_wce_cleanup_eh_only (gimple g)

  Return the ``CLEANUP_EH_ONLY`` flag for a ``WCE`` tuple.

.. function:: void gimple_wce_set_cleanup_eh_only (gimple g, bool eh_only_p)

  Set the ``CLEANUP_EH_ONLY`` flag for a ``WCE`` tuple.