..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: stack smashing protection

.. _stack-smashing-protection:

Stack smashing protection
^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: tree TARGET_STACK_PROTECT_GUARD (void)

  .. hook-start:TARGET_STACK_PROTECT_GUARD

  This hook returns a ``DECL`` node for the external variable to use
  for the stack protection guard.  This variable is initialized by the
  runtime to some random value and is used to initialize the guard value
  that is placed at the top of the local stack frame.  The type of this
  variable must be ``ptr_type_node``.

  The default version of this hook creates a variable called
  :samp:`__stack_chk_guard`, which is normally defined in :samp:`libgcc2.c`.

.. hook-end

.. function:: tree TARGET_STACK_PROTECT_FAIL (void)

  .. hook-start:TARGET_STACK_PROTECT_FAIL

  This hook returns a ``CALL_EXPR`` that alerts the runtime that the
  stack protect guard variable has been modified.  This expression should
  involve a call to a ``noreturn`` function.

  The default version of this hook invokes a function called
  :samp:`__stack_chk_fail`, taking no arguments.  This function is
  normally defined in :samp:`libgcc2.c`.

.. hook-end

.. function:: bool TARGET_STACK_PROTECT_RUNTIME_ENABLED_P (void)

  .. hook-start:TARGET_STACK_PROTECT_RUNTIME_ENABLED_P

  Returns true if the target wants GCC's default stack protect runtime support,
  otherwise return false.  The default implementation always returns true.

.. hook-end

.. function:: bool TARGET_SUPPORTS_SPLIT_STACK (bool report, struct gcc_options *opts)

  .. hook-start:TARGET_SUPPORTS_SPLIT_STACK

  Whether this target supports splitting the stack when the options
  described in :samp:`{opts}` have been passed.  This is called
  after options have been parsed, so the target may reject splitting
  the stack in some configurations.  The default version of this hook
  returns false.  If :samp:`{report}` is true, this function may issue a warning
  or error; if :samp:`{report}` is false, it must simply return a value

.. hook-end

.. function:: vec<const char *> TARGET_GET_VALID_OPTION_VALUES (int option_code, const char *prefix)

  .. hook-start:TARGET_GET_VALID_OPTION_VALUES

  The hook is used for options that have a non-trivial list of
  possible option values.  OPTION_CODE is option code of opt_code
  enum type.  PREFIX is used for bash completion and allows an implementation
  to return more specific completion based on the prefix.  All string values
  should be allocated from heap memory and consumers should release them.
  The result will be pruned to cases with PREFIX if not NULL.

.. hook-end