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

.. function:: rtx TARGET_FUNCTION_ARG (cumulative_args_t ca, const function_arg_info &arg)

  .. hook-start:TARGET_FUNCTION_ARG

  Return an RTX indicating whether function argument :samp:`{arg}` is passed
  in a register and if so, which register.  Argument :samp:`{ca}` summarizes all
  the previous arguments.

  The return value is usually either a ``reg`` RTX for the hard
  register in which to pass the argument, or zero to pass the argument
  on the stack.

  The value of the expression can also be a ``parallel`` RTX.  This is
  used when an argument is passed in multiple locations.  The mode of the
  ``parallel`` should be the mode of the entire argument.  The
  ``parallel`` holds any number of ``expr_list`` pairs; each one
  describes where part of the argument is passed.  In each
  ``expr_list`` the first operand must be a ``reg`` RTX for the hard
  register in which to pass this part of the argument, and the mode of the
  register RTX indicates how large this part of the argument is.  The
  second operand of the ``expr_list`` is a ``const_int`` which gives
  the offset in bytes into the entire argument of where this part starts.
  As a special exception the first ``expr_list`` in the ``parallel``
  RTX may have a first operand of zero.  This indicates that the entire
  argument is also stored on the stack.

  The last time this hook is called, it is called with ``MODE ==
  VOIDmode``, and its result is passed to the ``call`` or ``call_value``
  pattern as operands 2 and 3 respectively.

  .. index:: stdarg.h and register arguments

  The usual way to make the ISO library :samp:`stdarg.h` work on a
  machine where some arguments are usually passed in registers, is to
  cause nameless arguments to be passed on the stack instead.  This is
  done by making ``TARGET_FUNCTION_ARG`` return 0 whenever
  :samp:`{named}` is ``false``.

  .. index:: TARGET_MUST_PASS_IN_STACK, and TARGET_FUNCTION_ARG, REG_PARM_STACK_SPACE, and TARGET_FUNCTION_ARG

  You may use the hook ``targetm.calls.must_pass_in_stack``
  in the definition of this macro to determine if this argument is of a
  type that must be passed in the stack.  If ``REG_PARM_STACK_SPACE``
  is not defined and ``TARGET_FUNCTION_ARG`` returns nonzero for such an
  argument, the compiler will abort.  If ``REG_PARM_STACK_SPACE`` is
  defined, the argument will be computed in the stack and then loaded into
  a register.

.. hook-end

.. function:: bool TARGET_MUST_PASS_IN_STACK (const function_arg_info &arg)

  .. hook-start:TARGET_MUST_PASS_IN_STACK

  This target hook should return ``true`` if we should not pass :samp:`{arg}`
  solely in registers.  The file :samp:`expr.h` defines a
  definition that is usually appropriate, refer to :samp:`expr.h` for additional
  documentation.

.. hook-end

.. function:: rtx TARGET_FUNCTION_INCOMING_ARG (cumulative_args_t ca, const function_arg_info &arg)

  .. hook-start:TARGET_FUNCTION_INCOMING_ARG

  Define this hook if the caller and callee on the target have different
  views of where arguments are passed.  Also define this hook if there are
  functions that are never directly called, but are invoked by the hardware
  and which have nonstandard calling conventions.

  In this case ``TARGET_FUNCTION_ARG`` computes the register in
  which the caller passes the value, and
  ``TARGET_FUNCTION_INCOMING_ARG`` should be defined in a similar
  fashion to tell the function being called where the arguments will
  arrive.

  ``TARGET_FUNCTION_INCOMING_ARG`` can also return arbitrary address
  computation using hard register, which can be forced into a register,
  so that it can be used to pass special arguments.

  If ``TARGET_FUNCTION_INCOMING_ARG`` is not defined,
  ``TARGET_FUNCTION_ARG`` serves both purposes.

.. hook-end

.. function:: bool TARGET_USE_PSEUDO_PIC_REG (void)

  .. hook-start:TARGET_USE_PSEUDO_PIC_REG

  This hook should return 1 in case pseudo register should be created
  for pic_offset_table_rtx during function expand.

.. hook-end

.. function:: void TARGET_INIT_PIC_REG (void)

  .. hook-start:TARGET_INIT_PIC_REG

  Perform a target dependent initialization of pic_offset_table_rtx.
  This hook is called at the start of register allocation.

.. hook-end

.. function:: int TARGET_ARG_PARTIAL_BYTES (cumulative_args_t cum, const function_arg_info &arg)

  .. hook-start:TARGET_ARG_PARTIAL_BYTES

  This target hook returns the number of bytes at the beginning of an
  argument that must be put in registers.  The value must be zero for
  arguments that are passed entirely in registers or that are entirely
  pushed on the stack.

  On some machines, certain arguments must be passed partially in
  registers and partially in memory.  On these machines, typically the
  first few words of arguments are passed in registers, and the rest
  on the stack.  If a multi-word argument (a ``double`` or a
  structure) crosses that boundary, its first few words must be passed
  in registers and the rest must be pushed.  This macro tells the
  compiler when this occurs, and how many bytes should go in registers.

  ``TARGET_FUNCTION_ARG`` for these arguments should return the first
  register to be used by the caller for this argument; likewise
  ``TARGET_FUNCTION_INCOMING_ARG``, for the called function.

.. hook-end

.. function:: bool TARGET_PASS_BY_REFERENCE (cumulative_args_t cum, const function_arg_info &arg)

  .. hook-start:TARGET_PASS_BY_REFERENCE

  This target hook should return ``true`` if argument :samp:`{arg}` at the
  position indicated by :samp:`{cum}` should be passed by reference.  This
  predicate is queried after target independent reasons for being
  passed by reference, such as ``TREE_ADDRESSABLE (arg.type)``.

  If the hook returns true, a copy of that argument is made in memory and a
  pointer to the argument is passed instead of the argument itself.
  The pointer is passed in whatever way is appropriate for passing a pointer
  to that type.

.. hook-end

.. function:: bool TARGET_CALLEE_COPIES (cumulative_args_t cum, const function_arg_info &arg)

  .. hook-start:TARGET_CALLEE_COPIES

  The function argument described by the parameters to this hook is
  known to be passed by reference.  The hook should return true if the
  function argument should be copied by the callee instead of copied
  by the caller.

  For any argument for which the hook returns true, if it can be
  determined that the argument is not modified, then a copy need
  not be generated.

  The default version of this hook always returns false.

.. hook-end

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

.. function:: void TARGET_FUNCTION_ARG_ADVANCE (cumulative_args_t ca, const function_arg_info &arg)

  .. hook-start:TARGET_FUNCTION_ARG_ADVANCE

  This hook updates the summarizer variable pointed to by :samp:`{ca}` to
  advance past argument :samp:`{arg}` in the argument list.  Once this is done,
  the variable :samp:`{cum}` is suitable for analyzing the *following*
  argument with ``TARGET_FUNCTION_ARG``, etc.

  This hook need not do anything if the argument in question was passed
  on the stack.  The compiler knows how to track the amount of stack space
  used for arguments without any special help.

.. hook-end

.. function:: HOST_WIDE_INT TARGET_FUNCTION_ARG_OFFSET (machine_mode mode, const_tree type)

  .. hook-start:TARGET_FUNCTION_ARG_OFFSET

  This hook returns the number of bytes to add to the offset of an
  argument of type :samp:`{type}` and mode :samp:`{mode}` when passed in memory.
  This is needed for the SPU, which passes ``char`` and ``short``
  arguments in the preferred slot that is in the middle of the quad word
  instead of starting at the top.  The default implementation returns 0.

.. hook-end

.. function:: pad_direction TARGET_FUNCTION_ARG_PADDING (machine_mode mode, const_tree type)

  .. hook-start:TARGET_FUNCTION_ARG_PADDING

  This hook determines whether, and in which direction, to pad out
  an argument of mode :samp:`{mode}` and type :samp:`{type}`.  It returns
  ``PAD_UPWARD`` to insert padding above the argument, ``PAD_DOWNWARD``
  to insert padding below the argument, or ``PAD_NONE`` to inhibit padding.

  The *amount* of padding is not controlled by this hook, but by
  ``TARGET_FUNCTION_ARG_ROUND_BOUNDARY``.  It is always just enough
  to reach the next multiple of that boundary.

  This hook has a default definition that is right for most systems.
  For little-endian machines, the default is to pad upward.  For
  big-endian machines, the default is to pad downward for an argument of
  constant size shorter than an ``int``, and upward otherwise.

.. hook-end

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

.. function:: unsigned int TARGET_FUNCTION_ARG_BOUNDARY (machine_mode mode, const_tree type)

  .. hook-start:TARGET_FUNCTION_ARG_BOUNDARY

  This hook returns the alignment boundary, in bits, of an argument
  with the specified mode and type.  The default hook returns
  ``PARM_BOUNDARY`` for all arguments.

.. hook-end

.. function:: unsigned int TARGET_FUNCTION_ARG_ROUND_BOUNDARY (machine_mode mode, const_tree type)

  .. hook-start:TARGET_FUNCTION_ARG_ROUND_BOUNDARY

  Normally, the size of an argument is rounded up to ``PARM_BOUNDARY``,
  which is the default value for this hook.  You can define this hook to
  return a different value if an argument size must be rounded to a larger
  value.

.. hook-end

.. c:macro:: FUNCTION_ARG_REGNO_P (regno)

  A C expression that is nonzero if :samp:`{regno}` is the number of a hard
  register in which function arguments are sometimes passed.  This does
  *not* include implicit arguments such as the static chain and
  the structure-value address.  On many machines, no registers can be
  used for this purpose since all function arguments are pushed on the
  stack.

.. function:: bool TARGET_SPLIT_COMPLEX_ARG (const_tree type)

  .. hook-start:TARGET_SPLIT_COMPLEX_ARG

  This hook should return true if parameter of type :samp:`{type}` are passed
  as two scalar parameters.  By default, GCC will attempt to pack complex
  arguments into the target's word size.  Some ABIs require complex arguments
  to be split and treated as their individual components.  For example, on
  AIX64, complex floats should be passed in a pair of floating point
  registers, even though a complex float would fit in one 64-bit floating
  point register.

  The default value of this hook is ``NULL``, which is treated as always
  false.

.. hook-end

.. function:: tree TARGET_BUILD_BUILTIN_VA_LIST (void)

  .. hook-start:TARGET_BUILD_BUILTIN_VA_LIST

  This hook returns a type node for ``va_list`` for the target.
  The default version of the hook returns ``void*``.

.. hook-end

.. function:: int TARGET_ENUM_VA_LIST_P (int idx, const char **pname, tree *ptree)

  .. hook-start:TARGET_ENUM_VA_LIST_P

  This target hook is used in function ``c_common_nodes_and_builtins``
  to iterate through the target specific builtin types for va_list. The
  variable :samp:`{idx}` is used as iterator. :samp:`{pname}` has to be a pointer
  to a ``const char *`` and :samp:`{ptree}` a pointer to a ``tree`` typed
  variable.
  The arguments :samp:`{pname}` and :samp:`{ptree}` are used to store the result of
  this macro and are set to the name of the va_list builtin type and its
  internal type.
  If the return value of this macro is zero, then there is no more element.
  Otherwise the :samp:`{IDX}` should be increased for the next call of this
  macro to iterate through all types.

.. hook-end

.. function:: tree TARGET_FN_ABI_VA_LIST (tree fndecl)

  .. hook-start:TARGET_FN_ABI_VA_LIST

  This hook returns the va_list type of the calling convention specified by
  :samp:`{fndecl}`.
  The default version of this hook returns ``va_list_type_node``.

.. hook-end

.. function:: tree TARGET_CANONICAL_VA_LIST_TYPE (tree type)

  .. hook-start:TARGET_CANONICAL_VA_LIST_TYPE

  This hook returns the va_list type of the calling convention specified by the
  type of :samp:`{type}`. If :samp:`{type}` is not a valid va_list type, it returns
  ``NULL_TREE``.

.. hook-end

.. function:: tree TARGET_GIMPLIFY_VA_ARG_EXPR (tree valist, tree type, gimple_seq *pre_p, gimple_seq *post_p)

  .. hook-start:TARGET_GIMPLIFY_VA_ARG_EXPR

  This hook performs target-specific gimplification of
  ``VA_ARG_EXPR``.  The first two parameters correspond to the
  arguments to ``va_arg`` ; the latter two are as in
  ``gimplify.cc:gimplify_expr``.

.. hook-end

.. function:: bool TARGET_VALID_POINTER_MODE (scalar_int_mode mode)

  .. hook-start:TARGET_VALID_POINTER_MODE

  Define this to return nonzero if the port can handle pointers
  with machine mode :samp:`{mode}`.  The default version of this
  hook returns true for both ``ptr_mode`` and ``Pmode``.

.. hook-end

.. function:: bool TARGET_REF_MAY_ALIAS_ERRNO (ao_ref *ref)

  .. hook-start:TARGET_REF_MAY_ALIAS_ERRNO

  Define this to return nonzero if the memory reference :samp:`{ref}`
  may alias with the system C library errno location.  The default
  version of this hook assumes the system C library errno location
  is either a declaration of type int or accessed by dereferencing
  a pointer to int.

.. hook-end

.. function:: machine_mode TARGET_TRANSLATE_MODE_ATTRIBUTE (machine_mode mode)

  .. hook-start:TARGET_TRANSLATE_MODE_ATTRIBUTE

  Define this hook if during mode attribute processing, the port should
  translate machine_mode :samp:`{mode}` to another mode.  For example, rs6000's
  ``KFmode``, when it is the same as ``TFmode``.

  The default version of the hook returns that mode that was passed in.

.. hook-end

.. function:: bool TARGET_SCALAR_MODE_SUPPORTED_P (scalar_mode mode)

  .. hook-start:TARGET_SCALAR_MODE_SUPPORTED_P

  Define this to return nonzero if the port is prepared to handle
  insns involving scalar mode :samp:`{mode}`.  For a scalar mode to be
  considered supported, all the basic arithmetic and comparisons
  must work.

  The default version of this hook returns true for any mode
  required to handle the basic C types (as defined by the port).
  Included here are the double-word arithmetic supported by the
  code in :samp:`optabs.cc`.

.. hook-end

.. function:: bool TARGET_VECTOR_MODE_SUPPORTED_P (machine_mode mode)

  .. hook-start:TARGET_VECTOR_MODE_SUPPORTED_P

  Define this to return nonzero if the port is prepared to handle
  insns involving vector mode :samp:`{mode}`.  At the very least, it
  must have move patterns for this mode.

.. hook-end

.. function:: bool TARGET_COMPATIBLE_VECTOR_TYPES_P (const_tree type1, const_tree type2)

  .. hook-start:TARGET_COMPATIBLE_VECTOR_TYPES_P

  Return true if there is no target-specific reason for treating
  vector types :samp:`{type1}` and :samp:`{type2}` as distinct types.  The caller
  has already checked for target-independent reasons, meaning that the
  types are known to have the same mode, to have the same number of elements,
  and to have what the caller considers to be compatible element types.

  The main reason for defining this hook is to reject pairs of types
  that are handled differently by the target's calling convention.
  For example, when a new :samp:`{N}` -bit vector architecture is added
  to a target, the target may want to handle normal :samp:`{N}` -bit
  ``VECTOR_TYPE`` arguments and return values in the same way as
  before, to maintain backwards compatibility.  However, it may also
  provide new, architecture-specific ``VECTOR_TYPE`` s that are passed
  and returned in a more efficient way.  It is then important to maintain
  a distinction between the 'normal' ``VECTOR_TYPE`` s and the new
  architecture-specific ones.

  The default implementation returns true, which is correct for most targets.

.. hook-end

.. function:: opt_machine_mode TARGET_ARRAY_MODE (machine_mode mode, unsigned HOST_WIDE_INT nelems)

  .. hook-start:TARGET_ARRAY_MODE

  Return the mode that GCC should use for an array that has
  :samp:`{nelems}` elements, with each element having mode :samp:`{mode}`.
  Return no mode if the target has no special requirements.  In the
  latter case, GCC looks for an integer mode of the appropriate size
  if available and uses BLKmode otherwise.  Usually the search for the
  integer mode is limited to ``MAX_FIXED_MODE_SIZE``, but the
  ``TARGET_ARRAY_MODE_SUPPORTED_P`` hook allows a larger mode to be
  used in specific cases.

  The main use of this hook is to specify that an array of vectors should
  also have a vector mode.  The default implementation returns no mode.

.. hook-end

.. function:: bool TARGET_ARRAY_MODE_SUPPORTED_P (machine_mode mode, unsigned HOST_WIDE_INT nelems)

  .. hook-start:TARGET_ARRAY_MODE_SUPPORTED_P

  Return true if GCC should try to use a scalar mode to store an array
  of :samp:`{nelems}` elements, given that each element has mode :samp:`{mode}`.
  Returning true here overrides the usual ``MAX_FIXED_MODE`` limit
  and allows GCC to use any defined integer mode.

  One use of this hook is to support vector load and store operations
  that operate on several homogeneous vectors.  For example, ARM NEON
  has operations like:

  .. code-block:: c++

    int8x8x3_t vld3_s8 (const int8_t *)

  where the return type is defined as:

  .. code-block:: c++

    typedef struct int8x8x3_t
    {
      int8x8_t val[3];
    } int8x8x3_t;

  If this hook allows ``val`` to have a scalar mode, then
  ``int8x8x3_t`` can have the same mode.  GCC can then store
  ``int8x8x3_t`` s in registers rather than forcing them onto the stack.

.. hook-end

.. function:: bool TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P (scalar_float_mode mode)

  .. hook-start:TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P

  Define this to return nonzero if libgcc provides support for the
  floating-point mode :samp:`{mode}`, which is known to pass
  ``TARGET_SCALAR_MODE_SUPPORTED_P``.  The default version of this
  hook returns true for all of ``SFmode``, ``DFmode``,
  ``XFmode`` and ``TFmode``, if such modes exist.

.. hook-end

.. function:: opt_scalar_float_mode TARGET_FLOATN_MODE (int n, bool extended)

  .. hook-start:TARGET_FLOATN_MODE

  Define this to return the machine mode to use for the type
  ``_Floatn``, if :samp:`{extended}` is false, or the type
  ``_Floatnx``, if :samp:`{extended}` is true.  If such a type is not
  supported, return ``opt_scalar_float_mode ()``.  The default version of
  this hook returns ``SFmode`` for ``_Float32``, ``DFmode`` for
  ``_Float64`` and ``_Float32x`` and ``TFmode`` for
  ``_Float128``, if those modes exist and satisfy the requirements for
  those types and pass ``TARGET_SCALAR_MODE_SUPPORTED_P`` and
  ``TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P`` ; for ``_Float64x``, it
  returns the first of ``XFmode`` and ``TFmode`` that exists and
  satisfies the same requirements; for other types, it returns
  ``opt_scalar_float_mode ()``.  The hook is only called for values
  of :samp:`{n}` and :samp:`{extended}` that are valid according to
  ISO/IEC TS 18661-3:2015; that is, :samp:`{n}` is one of 32, 64, 128, or,
  if :samp:`{extended}` is false, 16 or greater than 128 and a multiple of 32.

.. hook-end

.. function:: bool TARGET_FLOATN_BUILTIN_P (int func)

  .. hook-start:TARGET_FLOATN_BUILTIN_P

  Define this to return true if the ``_Floatn`` and
  ``_Floatnx`` built-in functions should implicitly enable the
  built-in function without the ``__builtin_`` prefix in addition to the
  normal built-in function with the ``__builtin_`` prefix.  The default is
  to only enable built-in functions without the ``__builtin_`` prefix for
  the GNU C langauge.  In strict ANSI/ISO mode, the built-in function without
  the ``__builtin_`` prefix is not enabled.  The argument ``FUNC`` is the
  ``enum built_in_function`` id of the function to be enabled.

.. hook-end

.. function:: bool TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P (machine_mode mode)

  .. hook-start:TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P

  Define this to return nonzero for machine modes for which the port has
  small register classes.  If this target hook returns nonzero for a given
  :samp:`{mode}`, the compiler will try to minimize the lifetime of registers
  in :samp:`{mode}`.  The hook may be called with ``VOIDmode`` as argument.
  In this case, the hook is expected to return nonzero if it returns nonzero
  for any mode.

  On some machines, it is risky to let hard registers live across arbitrary
  insns.  Typically, these machines have instructions that require values
  to be in specific registers (like an accumulator), and reload will fail
  if the required hard register is used for another purpose across such an
  insn.

  Passes before reload do not know which hard registers will be used
  in an instruction, but the machine modes of the registers set or used in
  the instruction are already known.  And for some machines, register
  classes are small for, say, integer registers but not for floating point
  registers.  For example, the AMD x86-64 architecture requires specific
  registers for the legacy x86 integer instructions, but there are many
  SSE registers for floating point operations.  On such targets, a good
  strategy may be to return nonzero from this hook for ``INTEGRAL_MODE_P``
  machine modes but zero for the SSE register classes.

  The default version of this hook returns false for any mode.  It is always
  safe to redefine this hook to return with a nonzero value.  But if you
  unnecessarily define it, you will reduce the amount of optimizations
  that can be performed in some cases.  If you do not define this hook
  to return a nonzero value when it is required, the compiler will run out
  of spill registers and print a fatal error message.

.. hook-end