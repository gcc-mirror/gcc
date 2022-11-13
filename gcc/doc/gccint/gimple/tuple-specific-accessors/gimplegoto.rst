..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_GOTO

GIMPLE_GOTO
^^^^^^^^^^^

.. function:: ggoto *gimple_build_goto (tree dest)

  Build a ``GIMPLE_GOTO`` statement to label ``DEST``.

.. function:: tree gimple_goto_dest (gimple g)

  Return the destination of the unconditional jump ``G``.

.. function:: void gimple_goto_set_dest (ggoto *g, tree dest)

  Set ``DEST`` to be the destination of the unconditional jump ``G``.