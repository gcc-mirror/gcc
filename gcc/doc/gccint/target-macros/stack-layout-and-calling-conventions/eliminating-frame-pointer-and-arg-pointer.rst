..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _elimination:

Eliminating Frame Pointer and Arg Pointer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This is about eliminating the frame pointer and arg pointer.

.. function:: bool TARGET_FRAME_POINTER_REQUIRED (void)

  .. hook-start:TARGET_FRAME_POINTER_REQUIRED

  This target hook should return ``true`` if a function must have and use
  a frame pointer.  This target hook is called in the reload pass.  If its return
  value is ``true`` the function will have a frame pointer.

  This target hook can in principle examine the current function and decide
  according to the facts, but on most machines the constant ``false`` or the
  constant ``true`` suffices.  Use ``false`` when the machine allows code
  to be generated with no frame pointer, and doing so saves some time or space.
  Use ``true`` when there is no possible advantage to avoiding a frame
  pointer.

  In certain cases, the compiler does not know how to produce valid code
  without a frame pointer.  The compiler recognizes those cases and
  automatically gives the function a frame pointer regardless of what
  ``targetm.frame_pointer_required`` returns.  You don't need to worry about
  them.

  In a function that does not require a frame pointer, the frame pointer
  register can be allocated for ordinary usage, unless you mark it as a
  fixed register.  See ``FIXED_REGISTERS`` for more information.

  Default return value is ``false``.

.. hook-end

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

.. function:: bool TARGET_CAN_ELIMINATE (const int from_reg, const int to_reg)

  .. hook-start:TARGET_CAN_ELIMINATE

  This target hook should return ``true`` if the compiler is allowed to
  try to replace register number :samp:`{from_reg}` with register number
  :samp:`{to_reg}`.  This target hook will usually be ``true``, since most of the
  cases preventing register elimination are things that the compiler already
  knows about.

  Default return value is ``true``.

.. hook-end

.. c:macro:: INITIAL_ELIMINATION_OFFSET (from_reg, to_reg, offset_var)

  This macro returns the initial difference between the specified pair
  of registers.  The value would be computed from information
  such as the result of ``get_frame_size ()`` and the tables of
  registers ``df_regs_ever_live_p`` and ``call_used_regs``.

.. function:: void TARGET_COMPUTE_FRAME_LAYOUT (void)

  .. hook-start:TARGET_COMPUTE_FRAME_LAYOUT

  This target hook is called once each time the frame layout needs to be
  recalculated.  The calculations can be cached by the target and can then
  be used by ``INITIAL_ELIMINATION_OFFSET`` instead of re-computing the
  layout on every invocation of that hook.  This is particularly useful
  for targets that have an expensive frame layout function.  Implementing
  this callback is optional.

.. hook-end