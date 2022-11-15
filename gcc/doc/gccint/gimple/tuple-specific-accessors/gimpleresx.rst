..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_RESX

GIMPLE_RESX
^^^^^^^^^^^

.. function:: gresx *gimple_build_resx (int region)

  Build a ``GIMPLE_RESX`` statement which is a statement.  This
  statement is a placeholder for _Unwind_Resume before we know if a
  function call or a branch is needed.  ``REGION`` is the exception
  region from which control is flowing.

.. function:: int gimple_resx_region (const gresx *g)

  Return the region number for ``GIMPLE_RESX`` ``G``.

.. function:: void gimple_resx_set_region (gresx *g, int region)

  Set ``REGION`` to be the region number for ``GIMPLE_RESX`` ``G``.
