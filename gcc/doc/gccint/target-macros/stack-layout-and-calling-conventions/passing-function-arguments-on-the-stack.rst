..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: arguments on stack, stack arguments

.. _stack-arguments:

Passing Function Arguments on the Stack
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The macros in this section control how arguments are passed
on the stack.  See the following section for other macros that
control passing certain arguments in registers.

.. include:: ../tm.rst.in
  :start-after: [TARGET_PROMOTE_PROTOTYPES]
  :end-before: [TARGET_PROMOTE_PROTOTYPES]


.. include:: ../tm.rst.in
  :start-after: [TARGET_PUSH_ARGUMENT]
  :end-before: [TARGET_PUSH_ARGUMENT]


.. c:macro:: PUSH_ARGS_REVERSED

  A C expression.  If nonzero, function arguments will be evaluated from
  last to first, rather than from first to last.  If this macro is not
  defined, it defaults to ``PUSH_ARGS`` on targets where the stack
  and args grow in opposite directions, and 0 otherwise.

.. c:macro:: PUSH_ROUNDING (npushed)

  A C expression that is the number of bytes actually pushed onto the
  stack when an instruction attempts to push :samp:`{npushed}` bytes.

  On some machines, the definition

  .. code-block:: c++

    #define PUSH_ROUNDING(BYTES) (BYTES)

  will suffice.  But on other machines, instructions that appear
  to push one byte actually push two bytes in an attempt to maintain
  alignment.  Then the definition should be

  .. code-block:: c++

    #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)

  If the value of this macro has a type, it should be an unsigned type.

.. index:: outgoing_args_size, crtl->outgoing_args_size

.. c:macro:: ACCUMULATE_OUTGOING_ARGS

  A C expression.  If nonzero, the maximum amount of space required for outgoing arguments
  will be computed and placed into
  ``crtl->outgoing_args_size``.  No space will be pushed
  onto the stack for each call; instead, the function prologue should
  increase the stack frame size by this amount.

  Setting both ``PUSH_ARGS`` and ``ACCUMULATE_OUTGOING_ARGS``
  is not proper.

.. c:macro:: REG_PARM_STACK_SPACE (fndecl)

  Define this macro if functions should assume that stack space has been
  allocated for arguments even when their values are passed in
  registers.

  The value of this macro is the size, in bytes, of the area reserved for
  arguments passed in registers for the function represented by :samp:`{fndecl}`,
  which can be zero if GCC is calling a library function.
  The argument :samp:`{fndecl}` can be the FUNCTION_DECL, or the type itself
  of the function.

  This space can be allocated by the caller, or be a part of the
  machine-dependent stack frame: ``OUTGOING_REG_PARM_STACK_SPACE`` says
  which.

.. above is overfull.  not sure what to do.  -mew 5feb93  did

.. something, not sure if it looks good.  -mew 10feb93

.. c:macro:: INCOMING_REG_PARM_STACK_SPACE (fndecl)

  Like ``REG_PARM_STACK_SPACE``, but for incoming register arguments.
  Define this macro if space guaranteed when compiling a function body
  is different to space required when making a call, a situation that
  can arise with K&R style function definitions.

.. c:macro:: OUTGOING_REG_PARM_STACK_SPACE (fntype)

  Define this to a nonzero value if it is the responsibility of the
  caller to allocate the area reserved for arguments passed in registers
  when calling a function of :samp:`{fntype}`.  :samp:`{fntype}` may be NULL
  if the function called is a library function.

  If ``ACCUMULATE_OUTGOING_ARGS`` is defined, this macro controls
  whether the space for these arguments counts in the value of
  ``crtl->outgoing_args_size``.

.. c:macro:: STACK_PARMS_IN_REG_PARM_AREA

  Define this macro if ``REG_PARM_STACK_SPACE`` is defined, but the
  stack parameters don't skip the area specified by it.

  .. i changed this, makes more sens and it should have taken care of the

  .. overfull.. not as specific, tho.  -mew 5feb93

  Normally, when a parameter is not passed in registers, it is placed on the
  stack beyond the ``REG_PARM_STACK_SPACE`` area.  Defining this macro
  suppresses this behavior and causes the parameter to be passed on the
  stack in its natural location.

.. include:: ../tm.rst.in
  :start-after: [TARGET_RETURN_POPS_ARGS]
  :end-before: [TARGET_RETURN_POPS_ARGS]


.. c:macro:: CALL_POPS_ARGS (cum)

  A C expression that should indicate the number of bytes a call sequence
  pops off the stack.  It is added to the value of ``RETURN_POPS_ARGS``
  when compiling a function call.

  :samp:`{cum}` is the variable in which all arguments to the called function
  have been accumulated.

  On certain architectures, such as the SH5, a call trampoline is used
  that pops certain registers off the stack, depending on the arguments
  that have been passed to the function.  Since this is a property of the
  call site, not of the called function, ``RETURN_POPS_ARGS`` is not
  appropriate.