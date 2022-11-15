..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: function entry and exit, prologue, epilogue

.. _function-entry:

Function Entry and Exit
^^^^^^^^^^^^^^^^^^^^^^^

This section describes the macros that output function entry
(:dfn:`prologue`) and exit (:dfn:`epilogue`) code.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY]
  :end-before: [TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_PROLOGUE]
  :end-before: [TARGET_ASM_FUNCTION_PROLOGUE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_END_PROLOGUE]
  :end-before: [TARGET_ASM_FUNCTION_END_PROLOGUE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_BEGIN_EPILOGUE]
  :end-before: [TARGET_ASM_FUNCTION_BEGIN_EPILOGUE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_EPILOGUE]
  :end-before: [TARGET_ASM_FUNCTION_EPILOGUE]


*
  .. index:: pretend_args_size, crtl->args.pretend_args_size

  A region of ``crtl->args.pretend_args_size`` bytes of
  uninitialized space just underneath the first argument arriving on the
  stack.  (This may not be at the very start of the allocated stack region
  if the calling sequence has pushed anything else since pushing the stack
  arguments.  But usually, on such machines, nothing else has been pushed
  yet, because the function prologue itself does all the pushing.)  This
  region is used on machines where an argument may be passed partly in
  registers and partly in memory, and, in some cases to support the
  features in ``<stdarg.h>``.

* An area of memory used to save certain registers used by the function.
  The size of this area, which may also include space for such things as
  the return address and pointers to previous stack frames, is
  machine-specific and usually depends on which registers have been used
  in the function.  Machines with register windows often do not require
  a save area.

* A region of at least :samp:`{size}` bytes, possibly rounded up to an allocation
  boundary, to contain the local variables of the function.  On some machines,
  this region and the save area may occur in the opposite order, with the
  save area closer to the top of the stack.

.. index:: ACCUMULATE_OUTGOING_ARGS and stack frames

* Optionally, when ``ACCUMULATE_OUTGOING_ARGS`` is defined, a region of
  ``crtl->outgoing_args_size`` bytes to be used for outgoing
  argument lists of the function.  See :ref:`stack-arguments`.

.. c:macro:: EXIT_IGNORE_STACK

  Define this macro as a C expression that is nonzero if the return
  instruction or the function epilogue ignores the value of the stack
  pointer; in other words, if it is safe to delete an instruction to
  adjust the stack pointer before a return from the function.  The
  default is 0.

  Note that this macro's value is relevant only for functions for which
  frame pointers are maintained.  It is never safe to delete a final
  stack adjustment in a function that has no frame pointer, and the
  compiler knows this regardless of ``EXIT_IGNORE_STACK``.

.. c:macro:: EPILOGUE_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the epilogue or the :samp:`return` pattern.  The stack and frame
  pointer registers are already assumed to be used as needed.

.. c:macro:: EH_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the exception handling mechanism, and so should be considered live
  on entry to an exception edge.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_OUTPUT_MI_THUNK]
  :end-before: [TARGET_ASM_OUTPUT_MI_THUNK]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_CAN_OUTPUT_MI_THUNK]
  :end-before: [TARGET_ASM_CAN_OUTPUT_MI_THUNK]
