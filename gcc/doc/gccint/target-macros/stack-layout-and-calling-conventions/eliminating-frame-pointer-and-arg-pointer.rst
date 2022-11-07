..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _elimination:

Eliminating Frame Pointer and Arg Pointer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This is about eliminating the frame pointer and arg pointer.

.. include:: ../tm.rst.in
  :start-after: [TARGET_FRAME_POINTER_REQUIRED]
  :end-before: [TARGET_FRAME_POINTER_REQUIRED]


.. c:macro:: ELIMINABLE_REGS

  This macro specifies a table of register pairs used to eliminate
  unneeded registers that point into the stack frame.

  The definition of this macro is a list of structure initializations, each
  of which specifies an original and replacement register.

  On some machines, the position of the argument pointer is not known until
  the compilation is completed.  In such a case, a separate hard register
  must be used for the argument pointer.  This register can be eliminated by
  replacing it with either the frame pointer or the argument pointer,
  depending on whether or not the frame pointer has been eliminated.

  In this case, you might specify:

  .. code-block:: c++

    #define ELIMINABLE_REGS  \
    {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
     {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
     {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

  Note that the elimination of the argument pointer with the stack pointer is
  specified first since that is the preferred elimination.

.. include:: ../tm.rst.in
  :start-after: [TARGET_CAN_ELIMINATE]
  :end-before: [TARGET_CAN_ELIMINATE]


.. c:macro:: INITIAL_ELIMINATION_OFFSET (from_reg, to_reg, offset_var)

  This macro returns the initial difference between the specified pair
  of registers.  The value would be computed from information
  such as the result of ``get_frame_size ()`` and the tables of
  registers ``df_regs_ever_live_p`` and ``call_used_regs``.

.. include:: ../tm.rst.in
  :start-after: [TARGET_COMPUTE_FRAME_LAYOUT]
  :end-before: [TARGET_COMPUTE_FRAME_LAYOUT]
