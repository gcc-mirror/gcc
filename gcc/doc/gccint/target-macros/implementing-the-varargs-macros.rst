..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: varargs implementation

.. _varargs:

Implementing the Varargs Macros
*******************************

GCC comes with an implementation of ``<varargs.h>`` and
``<stdarg.h>`` that work without change on machines that pass arguments
on the stack.  Other machines require their own implementations of
varargs, and the two machine independent header files must have
conditionals to include it.

ISO ``<stdarg.h>`` differs from traditional ``<varargs.h>`` mainly in
the calling convention for ``va_start``.  The traditional
implementation takes just one argument, which is the variable in which
to store the argument pointer.  The ISO implementation of
``va_start`` takes an additional second argument.  The user is
supposed to write the last named argument of the function here.

However, ``va_start`` should not use this argument.  The way to find
the end of the named arguments is with the built-in functions described
below.

.. c:macro:: __builtin_saveregs ()

  Use this built-in function to save the argument registers in memory so
  that the varargs mechanism can access them.  Both ISO and traditional
  versions of ``va_start`` must use ``__builtin_saveregs``, unless
  you use ``TARGET_SETUP_INCOMING_VARARGS`` (see below) instead.

  On some machines, ``__builtin_saveregs`` is open-coded under the
  control of the target hook ``TARGET_EXPAND_BUILTIN_SAVEREGS``.  On
  other machines, it calls a routine written in assembler language,
  found in :samp:`libgcc2.c`.

  Code generated for the call to ``__builtin_saveregs`` appears at the
  beginning of the function, as opposed to where the call to
  ``__builtin_saveregs`` is written, regardless of what the code is.
  This is because the registers must be saved before the function starts
  to use them for its own purposes.

  .. i rewrote the first sentence above to fix an overfull hbox. -mew

  .. 10feb93

.. c:macro:: __builtin_next_arg (lastarg)

  This builtin returns the address of the first anonymous stack
  argument, as type ``void *``.  If ``ARGS_GROW_DOWNWARD``, it
  returns the address of the location above the first anonymous stack
  argument.  Use it in ``va_start`` to initialize the pointer for
  fetching arguments from the stack.  Also use it in ``va_start`` to
  verify that the second parameter :samp:`{lastarg}` is the last named argument
  of the current function.

.. c:macro:: __builtin_classify_type (object)

  Since each machine has its own conventions for which data types are
  passed in which kind of register, your implementation of ``va_arg``
  has to embody these conventions.  The easiest way to categorize the
  specified data type is to use ``__builtin_classify_type`` together
  with ``sizeof`` and ``__alignof__``.

  ``__builtin_classify_type`` ignores the value of :samp:`{object}`,
  considering only its data type.  It returns an integer describing what
  kind of type that is---integer, floating, pointer, structure, and so on.

  The file :samp:`typeclass.h` defines an enumeration that you can use to
  interpret the values of ``__builtin_classify_type``.

These machine description macros help implement varargs:

.. function:: rtx TARGET_EXPAND_BUILTIN_SAVEREGS (void)

  .. hook-start:TARGET_EXPAND_BUILTIN_SAVEREGS

  If defined, this hook produces the machine-specific code for a call to
  ``__builtin_saveregs``.  This code will be moved to the very
  beginning of the function, before any parameter access are made.  The
  return value of this function should be an RTX that contains the value
  to use as the return of ``__builtin_saveregs``.

.. hook-end

.. function:: void TARGET_SETUP_INCOMING_VARARGS (cumulative_args_t args_so_far, const function_arg_info &arg, int *pretend_args_size, int second_time)

  .. hook-start:TARGET_SETUP_INCOMING_VARARGS

  This target hook offers an alternative to using
  ``__builtin_saveregs`` and defining the hook
  ``TARGET_EXPAND_BUILTIN_SAVEREGS``.  Use it to store the anonymous
  register arguments into the stack so that all the arguments appear to
  have been passed consecutively on the stack.  Once this is done, you can
  use the standard implementation of varargs that works for machines that
  pass all their arguments on the stack.

  The argument :samp:`{args_so_far}` points to the ``CUMULATIVE_ARGS`` data
  structure, containing the values that are obtained after processing the
  named arguments.  The argument :samp:`{arg}` describes the last of these named
  arguments.  The argument :samp:`{arg}` should not be used if the function type
  satisfies ``TYPE_NO_NAMED_ARGS_STDARG_P``, since in that case there are
  no named arguments and all arguments are accessed with ``va_arg``.

  The target hook should do two things: first, push onto the stack all the
  argument registers *not* used for the named arguments, and second,
  store the size of the data thus pushed into the ``int`` -valued
  variable pointed to by :samp:`{pretend_args_size}`.  The value that you
  store here will serve as additional offset for setting up the stack
  frame.

  Because you must generate code to push the anonymous arguments at
  compile time without knowing their data types,
  ``TARGET_SETUP_INCOMING_VARARGS`` is only useful on machines that
  have just a single category of argument register and use it uniformly
  for all data types.

  If the argument :samp:`{second_time}` is nonzero, it means that the
  arguments of the function are being analyzed for the second time.  This
  happens for an inline function, which is not actually compiled until the
  end of the source file.  The hook ``TARGET_SETUP_INCOMING_VARARGS`` should
  not generate any instructions in this case.

.. hook-end

.. function:: bool TARGET_STRICT_ARGUMENT_NAMING (cumulative_args_t ca)

  .. hook-start:TARGET_STRICT_ARGUMENT_NAMING

  Define this hook to return ``true`` if the location where a function
  argument is passed depends on whether or not it is a named argument.

  This hook controls how the :samp:`{named}` argument to ``TARGET_FUNCTION_ARG``
  is set for varargs and stdarg functions.  If this hook returns
  ``true``, the :samp:`{named}` argument is always true for named
  arguments, and false for unnamed arguments.  If it returns ``false``,
  but ``TARGET_PRETEND_OUTGOING_VARARGS_NAMED`` returns ``true``,
  then all arguments are treated as named.  Otherwise, all named arguments
  except the last are treated as named.

  You need not define this hook if it always returns ``false``.

.. hook-end

.. function:: void TARGET_CALL_ARGS (rtx, tree)

  .. hook-start:TARGET_CALL_ARGS

  While generating RTL for a function call, this target hook is invoked once
  for each argument passed to the function, either a register returned by
  ``TARGET_FUNCTION_ARG`` or a memory location.  It is called just
  before the point where argument registers are stored.  The type of the
  function to be called is also passed as the second argument; it is
  ``NULL_TREE`` for libcalls.  The ``TARGET_END_CALL_ARGS`` hook is
  invoked just after the code to copy the return reg has been emitted.
  This functionality can be used to perform special setup of call argument
  registers if a target needs it.
  For functions without arguments, the hook is called once with ``pc_rtx``
  passed instead of an argument register.
  Most ports do not need to implement anything for this hook.

.. hook-end

.. function:: void TARGET_END_CALL_ARGS (void)

  .. hook-start:TARGET_END_CALL_ARGS

  This target hook is invoked while generating RTL for a function call,
  just after the point where the return reg is copied into a pseudo.  It
  signals that all the call argument and return registers for the just
  emitted call are now no longer in use.
  Most ports do not need to implement anything for this hook.

.. hook-end

.. function:: bool TARGET_PRETEND_OUTGOING_VARARGS_NAMED (cumulative_args_t ca)

  .. hook-start:TARGET_PRETEND_OUTGOING_VARARGS_NAMED

  If you need to conditionally change ABIs so that one works with
  ``TARGET_SETUP_INCOMING_VARARGS``, but the other works like neither
  ``TARGET_SETUP_INCOMING_VARARGS`` nor ``TARGET_STRICT_ARGUMENT_NAMING`` was
  defined, then define this hook to return ``true`` if
  ``TARGET_SETUP_INCOMING_VARARGS`` is used, ``false`` otherwise.
  Otherwise, you should not define this hook.

.. hook-end