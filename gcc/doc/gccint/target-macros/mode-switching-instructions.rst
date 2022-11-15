..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: mode switching

.. _mode-switching:

Mode Switching Instructions
***************************

The following macros control mode switching optimizations:

.. c:macro:: OPTIMIZE_MODE_SWITCHING (entity)

  Define this macro if the port needs extra instructions inserted for mode
  switching in an optimizing compilation.

  For an example, the SH4 can perform both single and double precision
  floating point operations, but to perform a single precision operation,
  the FPSCR PR bit has to be cleared, while for a double precision
  operation, this bit has to be set.  Changing the PR bit requires a general
  purpose register as a scratch register, hence these FPSCR sets have to
  be inserted before reload, i.e. you cannot put this into instruction emitting
  or ``TARGET_MACHINE_DEPENDENT_REORG``.

  You can have multiple entities that are mode-switched, and select at run time
  which entities actually need it.  ``OPTIMIZE_MODE_SWITCHING`` should
  return nonzero for any :samp:`{entity}` that needs mode-switching.
  If you define this macro, you also have to define
  ``NUM_MODES_FOR_MODE_SWITCHING``, ``TARGET_MODE_NEEDED``,
  ``TARGET_MODE_PRIORITY`` and ``TARGET_MODE_EMIT``.
  ``TARGET_MODE_AFTER``, ``TARGET_MODE_ENTRY``, and ``TARGET_MODE_EXIT``
  are optional.

.. c:macro:: NUM_MODES_FOR_MODE_SWITCHING

  If you define ``OPTIMIZE_MODE_SWITCHING``, you have to define this as
  initializer for an array of integers.  Each initializer element
  N refers to an entity that needs mode switching, and specifies the number
  of different modes that might need to be set for this entity.
  The position of the initializer in the initializer---starting counting at
  zero---determines the integer that is used to refer to the mode-switched
  entity in question.
  In macros that take mode arguments / yield a mode result, modes are
  represented as numbers 0 ... N - 1.  N is used to specify that no mode
  switch is needed / supplied.

.. include:: tm.rst.in
  :start-after: [TARGET_MODE_EMIT]
  :end-before: [TARGET_MODE_EMIT]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_NEEDED]
  :end-before: [TARGET_MODE_NEEDED]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_AFTER]
  :end-before: [TARGET_MODE_AFTER]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_ENTRY]
  :end-before: [TARGET_MODE_ENTRY]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_EXIT]
  :end-before: [TARGET_MODE_EXIT]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_PRIORITY]
  :end-before: [TARGET_MODE_PRIORITY]
