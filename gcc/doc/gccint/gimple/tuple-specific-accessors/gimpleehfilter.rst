..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_EH_FILTER

GIMPLE_EH_FILTER
^^^^^^^^^^^^^^^^

.. function:: geh_filter *gimple_build_eh_filter (tree types, gimple_seq failure)

  Build a ``GIMPLE_EH_FILTER`` statement.  ``TYPES`` are the filter's
  types.  ``FAILURE`` is a sequence with the filter's failure action.

.. function:: tree gimple_eh_filter_types (gimple g)

  Return the types handled by ``GIMPLE_EH_FILTER`` statement ``G``.

.. function:: tree * gimple_eh_filter_types_ptr (gimple g)

  Return a pointer to the types handled by ``GIMPLE_EH_FILTER``
  statement ``G``.

.. function:: gimple_seq gimple_eh_filter_failure (gimple g)

  Return the sequence of statement to execute when ``GIMPLE_EH_FILTER``
  statement fails.

.. function:: void gimple_eh_filter_set_types (geh_filter *g, tree types)

  Set ``TYPES`` to be the set of types handled by ``GIMPLE_EH_FILTER`` ``G``.

.. function:: void gimple_eh_filter_set_failure (geh_filter *g, gimple_seq failure)

  Set ``FAILURE`` to be the sequence of statements to execute on
  failure for ``GIMPLE_EH_FILTER`` ``G``.

.. function:: tree gimple_eh_must_not_throw_fndecl ( geh_mnt *eh_mnt_stmt)

  Get the function decl to be called by the MUST_NOT_THROW region.

.. function:: void gimple_eh_must_not_throw_set_fndecl ( geh_mnt *eh_mnt_stmt, tree decl)

  Set the function decl to be called by GS to DECL.