..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_RETURN

GIMPLE_RETURN
^^^^^^^^^^^^^

.. function:: greturn *gimple_build_return (tree retval)

  Build a ``GIMPLE_RETURN`` statement whose return value is retval.

.. function:: tree gimple_return_retval (const greturn *g)

  Return the return value for ``GIMPLE_RETURN`` ``G``.

.. function:: void gimple_return_set_retval (greturn *g, tree retval)

  Set ``RETVAL`` to be the return value for ``GIMPLE_RETURN`` ``G``.