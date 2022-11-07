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

.. function:: void TARGET_MODE_EMIT (int entity, int mode, int prev_mode, HARD_REG_SET regs_live)

  .. hook-start:TARGET_MODE_EMIT

  Generate one or more insns to set :samp:`{entity}` to :samp:`{mode}`.
  :samp:`{hard_reg_live}` is the set of hard registers live at the point where
  the insn(s) are to be inserted. :samp:`{prev_moxde}` indicates the mode
  to switch from. Sets of a lower numbered entity will be emitted before
  sets of a higher numbered entity to a mode of the same or lower priority.

.. hook-end

.. function:: int TARGET_MODE_NEEDED (int entity, rtx_insn *insn)

  .. hook-start:TARGET_MODE_NEEDED

  :samp:`{entity}` is an integer specifying a mode-switched entity.
  If ``OPTIMIZE_MODE_SWITCHING`` is defined, you must define this macro
  to return an integer value not larger than the corresponding element
  in ``NUM_MODES_FOR_MODE_SWITCHING``, to denote the mode that :samp:`{entity}`
  must be switched into prior to the execution of :samp:`{insn}`.

.. hook-end

.. function:: int TARGET_MODE_AFTER (int entity, int mode, rtx_insn *insn)

  .. hook-start:TARGET_MODE_AFTER

  :samp:`{entity}` is an integer specifying a mode-switched entity.
  If this macro is defined, it is evaluated for every :samp:`{insn}` during mode
  switching.  It determines the mode that an insn results
  in (if different from the incoming mode).

.. hook-end

.. function:: int TARGET_MODE_ENTRY (int entity)

  .. hook-start:TARGET_MODE_ENTRY

  If this macro is defined, it is evaluated for every :samp:`{entity}` that
  needs mode switching.  It should evaluate to an integer, which is a mode
  that :samp:`{entity}` is assumed to be switched to at function entry.
  If ``TARGET_MODE_ENTRY`` is defined then ``TARGET_MODE_EXIT``
  must be defined.

.. hook-end

.. function:: int TARGET_MODE_EXIT (int entity)

  .. hook-start:TARGET_MODE_EXIT

  If this macro is defined, it is evaluated for every :samp:`{entity}` that
  needs mode switching.  It should evaluate to an integer, which is a mode
  that :samp:`{entity}` is assumed to be switched to at function exit.
  If ``TARGET_MODE_EXIT`` is defined then ``TARGET_MODE_ENTRY``
  must be defined.

.. hook-end

.. function:: int TARGET_MODE_PRIORITY (int entity, int n)

  .. hook-start:TARGET_MODE_PRIORITY

  This macro specifies the order in which modes for :samp:`{entity}`
  are processed. 0 is the highest priority,
  ``NUM_MODES_FOR_MODE_SWITCHING[entity] - 1`` the lowest.
  The value of the macro should be an integer designating a mode
  for :samp:`{entity}`.  For any fixed :samp:`{entity}`, ``mode_priority``
  (:samp:`{entity}`, :samp:`{n}`) shall be a bijection in 0 ...
  ``num_modes_for_mode_switching[entity] - 1``.

.. hook-end