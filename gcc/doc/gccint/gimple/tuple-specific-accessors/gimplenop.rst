..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_NOP

GIMPLE_NOP
^^^^^^^^^^

.. function:: gimple gimple_build_nop (void)

  Build a ``GIMPLE_NOP`` statement.

.. function:: bool gimple_nop_p (gimple g)

  Returns ``TRUE`` if statement ``G`` is a ``GIMPLE_NOP``.
