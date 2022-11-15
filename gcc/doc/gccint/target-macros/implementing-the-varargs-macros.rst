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

.. include:: tm.rst.in
  :start-after: [TARGET_EXPAND_BUILTIN_SAVEREGS]
  :end-before: [TARGET_EXPAND_BUILTIN_SAVEREGS]


.. include:: tm.rst.in
  :start-after: [TARGET_SETUP_INCOMING_VARARGS]
  :end-before: [TARGET_SETUP_INCOMING_VARARGS]


.. include:: tm.rst.in
  :start-after: [TARGET_STRICT_ARGUMENT_NAMING]
  :end-before: [TARGET_STRICT_ARGUMENT_NAMING]


.. include:: tm.rst.in
  :start-after: [TARGET_CALL_ARGS]
  :end-before: [TARGET_CALL_ARGS]


.. include:: tm.rst.in
  :start-after: [TARGET_END_CALL_ARGS]
  :end-before: [TARGET_END_CALL_ARGS]


.. include:: tm.rst.in
  :start-after: [TARGET_PRETEND_OUTGOING_VARARGS_NAMED]
  :end-before: [TARGET_PRETEND_OUTGOING_VARARGS_NAMED]
