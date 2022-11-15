..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _stack-checking:

Specifying How Stack Checking is Done
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC will check that stack references are within the boundaries of the
stack, if the option :option:`-fstack-check` is specified, in one of
three ways:

* If the value of the ``STACK_CHECK_BUILTIN`` macro is nonzero, GCC
  will assume that you have arranged for full stack checking to be done
  at appropriate places in the configuration files.  GCC will not do
  other special processing.

* If ``STACK_CHECK_BUILTIN`` is zero and the value of the
  ``STACK_CHECK_STATIC_BUILTIN`` macro is nonzero, GCC will assume
  that you have arranged for static stack checking (checking of the
  static stack frame of functions) to be done at appropriate places
  in the configuration files.  GCC will only emit code to do dynamic
  stack checking (checking on dynamic stack allocations) using the third
  approach below.

* If neither of the above are true, GCC will generate code to periodically
  'probe' the stack pointer using the values of the macros defined below.

If neither STACK_CHECK_BUILTIN nor STACK_CHECK_STATIC_BUILTIN is defined,
GCC will change its allocation strategy for large objects if the option
:option:`-fstack-check` is specified: they will always be allocated
dynamically if their size exceeds ``STACK_CHECK_MAX_VAR_SIZE`` bytes.

.. c:macro:: STACK_CHECK_BUILTIN

  A nonzero value if stack checking is done by the configuration files in a
  machine-dependent manner.  You should define this macro if stack checking
  is required by the ABI of your machine or if you would like to do stack
  checking in some more efficient way than the generic approach.  The default
  value of this macro is zero.

.. c:macro:: STACK_CHECK_STATIC_BUILTIN

  A nonzero value if static stack checking is done by the configuration files
  in a machine-dependent manner.  You should define this macro if you would
  like to do static stack checking in some more efficient way than the generic
  approach.  The default value of this macro is zero.

.. c:macro:: STACK_CHECK_PROBE_INTERVAL_EXP

  An integer specifying the interval at which GCC must generate stack probe
  instructions, defined as 2 raised to this integer.  You will normally
  define this macro so that the interval be no larger than the size of
  the 'guard pages' at the end of a stack area.  The default value
  of 12 (4096-byte interval) is suitable for most systems.

.. c:macro:: STACK_CHECK_MOVING_SP

  An integer which is nonzero if GCC should move the stack pointer page by page
  when doing probes.  This can be necessary on systems where the stack pointer
  contains the bottom address of the memory area accessible to the executing
  thread at any point in time.  In this situation an alternate signal stack
  is required in order to be able to recover from a stack overflow.  The
  default value of this macro is zero.

.. c:macro:: STACK_CHECK_PROTECT

  The number of bytes of stack needed to recover from a stack overflow, for
  languages where such a recovery is supported.  The default value of 4KB/8KB
  with the ``setjmp`` / ``longjmp`` -based exception handling mechanism and
  8KB/12KB with other exception handling mechanisms should be adequate for most
  architectures and operating systems.

The following macros are relevant only if neither STACK_CHECK_BUILTIN
nor STACK_CHECK_STATIC_BUILTIN is defined; you can omit them altogether
in the opposite case.

.. c:macro:: STACK_CHECK_MAX_FRAME_SIZE

  The maximum size of a stack frame, in bytes.  GCC will generate probe
  instructions in non-leaf functions to ensure at least this many bytes of
  stack are available.  If a stack frame is larger than this size, stack
  checking will not be reliable and GCC will issue a warning.  The
  default is chosen so that GCC only generates one instruction on most
  systems.  You should normally not change the default value of this macro.

.. c:macro:: STACK_CHECK_FIXED_FRAME_SIZE

  GCC uses this value to generate the above warning message.  It
  represents the amount of fixed frame used by a function, not including
  space for any callee-saved registers, temporaries and user variables.
  You need only specify an upper bound for this amount and will normally
  use the default of four words.

.. c:macro:: STACK_CHECK_MAX_VAR_SIZE

  The maximum size, in bytes, of an object that GCC will place in the
  fixed area of the stack frame when the user specifies
  :option:`-fstack-check`.
  GCC computed the default from the values of the above macros and you will
  normally not need to override that default.

.. include:: ../tm.rst.in
  :start-after: [TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE]
  :end-before: [TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE]
