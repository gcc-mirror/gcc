..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: arguments in registers, registers arguments

.. _register-arguments:

Passing Arguments in Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes the macros which let you control how various
types of arguments are passed in registers or how they are arranged in
the stack.

.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG]
  :end-before: [TARGET_FUNCTION_ARG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_MUST_PASS_IN_STACK]
  :end-before: [TARGET_MUST_PASS_IN_STACK]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_INCOMING_ARG]
  :end-before: [TARGET_FUNCTION_INCOMING_ARG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_USE_PSEUDO_PIC_REG]
  :end-before: [TARGET_USE_PSEUDO_PIC_REG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_INIT_PIC_REG]
  :end-before: [TARGET_INIT_PIC_REG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ARG_PARTIAL_BYTES]
  :end-before: [TARGET_ARG_PARTIAL_BYTES]


.. include:: ../tm.rst.in
  :start-after: [TARGET_PASS_BY_REFERENCE]
  :end-before: [TARGET_PASS_BY_REFERENCE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_CALLEE_COPIES]
  :end-before: [TARGET_CALLEE_COPIES]


.. c:macro:: CUMULATIVE_ARGS

  A C type for declaring a variable that is used as the first argument
  of ``TARGET_FUNCTION_ARG`` and other related values.  For some
  target machines, the type ``int`` suffices and can hold the number
  of bytes of argument so far.

  There is no need to record in ``CUMULATIVE_ARGS`` anything about the
  arguments that have been passed on the stack.  The compiler has other
  variables to keep track of that.  For target machines on which all
  arguments are passed on the stack, there is no need to store anything in
  ``CUMULATIVE_ARGS`` ; however, the data structure must exist and
  should not be empty, so use ``int``.

.. c:macro:: OVERRIDE_ABI_FORMAT (fndecl)

  If defined, this macro is called before generating any code for a
  function, but after the :samp:`{cfun}` descriptor for the function has been
  created.  The back end may use this macro to update :samp:`{cfun}` to
  reflect an ABI other than that which would normally be used by default.
  If the compiler is generating code for a compiler-generated function,
  :samp:`{fndecl}` may be ``NULL``.

.. c:macro:: INIT_CUMULATIVE_ARGS (cum, fntype, libname, fndecl, n_named_args)

  A C statement (sans semicolon) for initializing the variable
  :samp:`{cum}` for the state at the beginning of the argument list.  The
  variable has type ``CUMULATIVE_ARGS``.  The value of :samp:`{fntype}`
  is the tree node for the data type of the function which will receive
  the args, or 0 if the args are to a compiler support library function.
  For direct calls that are not libcalls, :samp:`{fndecl}` contain the
  declaration node of the function.  :samp:`{fndecl}` is also set when
  ``INIT_CUMULATIVE_ARGS`` is used to find arguments for the function
  being compiled.  :samp:`{n_named_args}` is set to the number of named
  arguments, including a structure return address if it is passed as a
  parameter, when making a call.  When processing incoming arguments,
  :samp:`{n_named_args}` is set to -1.

  When processing a call to a compiler support library function,
  :samp:`{libname}` identifies which one.  It is a ``symbol_ref`` rtx which
  contains the name of the function, as a string.  :samp:`{libname}` is 0 when
  an ordinary C function call is being processed.  Thus, each time this
  macro is called, either :samp:`{libname}` or :samp:`{fntype}` is nonzero, but
  never both of them at once.

.. c:macro:: INIT_CUMULATIVE_LIBCALL_ARGS (cum, mode, libname)

  Like ``INIT_CUMULATIVE_ARGS`` but only used for outgoing libcalls,
  it gets a ``MODE`` argument instead of :samp:`{fntype}`, that would be
  ``NULL``.  :samp:`{indirect}` would always be zero, too.  If this macro
  is not defined, ``INIT_CUMULATIVE_ARGS (cum, NULL_RTX, libname,
  0)`` is used instead.

.. c:macro:: INIT_CUMULATIVE_INCOMING_ARGS (cum, fntype, libname)

  Like ``INIT_CUMULATIVE_ARGS`` but overrides it for the purposes of
  finding the arguments for the function being compiled.  If this macro is
  undefined, ``INIT_CUMULATIVE_ARGS`` is used instead.

  The value passed for :samp:`{libname}` is always 0, since library routines
  with special calling conventions are never compiled with GCC.  The
  argument :samp:`{libname}` exists for symmetry with
  ``INIT_CUMULATIVE_ARGS``.

  .. could use "this macro" in place of @code{INIT_CUMULATIVE_ARGS}, maybe.

  .. -mew 5feb93   i switched the order of the sentences.  -mew 10feb93

.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG_ADVANCE]
  :end-before: [TARGET_FUNCTION_ARG_ADVANCE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG_OFFSET]
  :end-before: [TARGET_FUNCTION_ARG_OFFSET]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG_PADDING]
  :end-before: [TARGET_FUNCTION_ARG_PADDING]


.. c:macro:: PAD_VARARGS_DOWN

  If defined, a C expression which determines whether the default
  implementation of va_arg will attempt to pad down before reading the
  next argument, if that argument is smaller than its aligned space as
  controlled by ``PARM_BOUNDARY``.  If this macro is not defined, all such
  arguments are padded down if ``BYTES_BIG_ENDIAN`` is true.

.. c:macro:: BLOCK_REG_PADDING (mode, type, first)

  Specify padding for the last element of a block move between registers and
  memory.  :samp:`{first}` is nonzero if this is the only element.  Defining this
  macro allows better control of register function parameters on big-endian
  machines, without using ``PARALLEL`` rtl.  In particular,
  ``MUST_PASS_IN_STACK`` need not test padding and mode of types in
  registers, as there is no longer a "wrong" part of a register;  For example,
  a three byte aggregate may be passed in the high part of a register if so
  required.

.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG_BOUNDARY]
  :end-before: [TARGET_FUNCTION_ARG_BOUNDARY]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_ARG_ROUND_BOUNDARY]
  :end-before: [TARGET_FUNCTION_ARG_ROUND_BOUNDARY]


.. c:macro:: FUNCTION_ARG_REGNO_P (regno)

  A C expression that is nonzero if :samp:`{regno}` is the number of a hard
  register in which function arguments are sometimes passed.  This does
  *not* include implicit arguments such as the static chain and
  the structure-value address.  On many machines, no registers can be
  used for this purpose since all function arguments are pushed on the
  stack.

.. include:: ../tm.rst.in
  :start-after: [TARGET_SPLIT_COMPLEX_ARG]
  :end-before: [TARGET_SPLIT_COMPLEX_ARG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_BUILD_BUILTIN_VA_LIST]
  :end-before: [TARGET_BUILD_BUILTIN_VA_LIST]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ENUM_VA_LIST_P]
  :end-before: [TARGET_ENUM_VA_LIST_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FN_ABI_VA_LIST]
  :end-before: [TARGET_FN_ABI_VA_LIST]


.. include:: ../tm.rst.in
  :start-after: [TARGET_CANONICAL_VA_LIST_TYPE]
  :end-before: [TARGET_CANONICAL_VA_LIST_TYPE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_GIMPLIFY_VA_ARG_EXPR]
  :end-before: [TARGET_GIMPLIFY_VA_ARG_EXPR]


.. include:: ../tm.rst.in
  :start-after: [TARGET_VALID_POINTER_MODE]
  :end-before: [TARGET_VALID_POINTER_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_REF_MAY_ALIAS_ERRNO]
  :end-before: [TARGET_REF_MAY_ALIAS_ERRNO]


.. include:: ../tm.rst.in
  :start-after: [TARGET_TRANSLATE_MODE_ATTRIBUTE]
  :end-before: [TARGET_TRANSLATE_MODE_ATTRIBUTE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SCALAR_MODE_SUPPORTED_P]
  :end-before: [TARGET_SCALAR_MODE_SUPPORTED_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_VECTOR_MODE_SUPPORTED_P]
  :end-before: [TARGET_VECTOR_MODE_SUPPORTED_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_COMPATIBLE_VECTOR_TYPES_P]
  :end-before: [TARGET_COMPATIBLE_VECTOR_TYPES_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ARRAY_MODE]
  :end-before: [TARGET_ARRAY_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ARRAY_MODE_SUPPORTED_P]
  :end-before: [TARGET_ARRAY_MODE_SUPPORTED_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P]
  :end-before: [TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FLOATN_MODE]
  :end-before: [TARGET_FLOATN_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_FLOATN_BUILTIN_P]
  :end-before: [TARGET_FLOATN_BUILTIN_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P]
  :end-before: [TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P]
