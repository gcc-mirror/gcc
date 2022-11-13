..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: return values in registers, values, returned by functions, scalars, returned as values

.. _scalar-return:

How Scalar Function Values Are Returned
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section discusses the macros that control returning scalars as
values---values that can fit in registers.

.. function:: rtx TARGET_FUNCTION_VALUE (const_tree ret_type, const_tree fn_decl_or_type, bool outgoing)

  .. hook-start:TARGET_FUNCTION_VALUE

  Define this to return an RTX representing the place where a function
  returns or receives a value of data type :samp:`{ret_type}`, a tree node
  representing a data type.  :samp:`{fn_decl_or_type}` is a tree node
  representing ``FUNCTION_DECL`` or ``FUNCTION_TYPE`` of a
  function being called.  If :samp:`{outgoing}` is false, the hook should
  compute the register in which the caller will see the return value.
  Otherwise, the hook should return an RTX representing the place where
  a function returns a value.

  On many machines, only ``TYPE_MODE (ret_type)`` is relevant.
  (Actually, on most machines, scalar values are returned in the same
  place regardless of mode.)  The value of the expression is usually a
  ``reg`` RTX for the hard register where the return value is stored.
  The value can also be a ``parallel`` RTX, if the return value is in
  multiple places.  See ``TARGET_FUNCTION_ARG`` for an explanation of the
  ``parallel`` form.   Note that the callee will populate every
  location specified in the ``parallel``, but if the first element of
  the ``parallel`` contains the whole return value, callers will use
  that element as the canonical location and ignore the others.  The m68k
  port uses this type of ``parallel`` to return pointers in both
  :samp:`%a0` (the canonical location) and :samp:`%d0`.

  If ``TARGET_PROMOTE_FUNCTION_RETURN`` returns true, you must apply
  the same promotion rules specified in ``PROMOTE_MODE`` if
  :samp:`{valtype}` is a scalar type.

  If the precise function being called is known, :samp:`{func}` is a tree
  node (``FUNCTION_DECL``) for it; otherwise, :samp:`{func}` is a null
  pointer.  This makes it possible to use a different value-returning
  convention for specific functions when all their calls are
  known.

  Some target machines have 'register windows' so that the register in
  which a function returns its value is not the same as the one in which
  the caller sees the value.  For such machines, you should return
  different RTX depending on :samp:`{outgoing}`.

  ``TARGET_FUNCTION_VALUE`` is not used for return values with
  aggregate data types, because these are returned in another way.  See
  ``TARGET_STRUCT_VALUE_RTX`` and related macros, below.

.. hook-end

.. c:macro:: FUNCTION_VALUE (valtype, func)

  This macro has been deprecated.  Use ``TARGET_FUNCTION_VALUE`` for
  a new target instead.

.. c:macro:: LIBCALL_VALUE (mode)

  A C expression to create an RTX representing the place where a library
  function returns a value of mode :samp:`{mode}`.

  Note that 'library function' in this context means a compiler
  support routine, used to perform arithmetic, whose name is known
  specially by the compiler and was not mentioned in the C code being
  compiled.

.. function:: rtx TARGET_LIBCALL_VALUE (machine_mode mode, const_rtx fun)

  .. hook-start:TARGET_LIBCALL_VALUE

  Define this hook if the back-end needs to know the name of the libcall
  function in order to determine where the result should be returned.

  The mode of the result is given by :samp:`{mode}` and the name of the called
  library function is given by :samp:`{fun}`.  The hook should return an RTX
  representing the place where the library function result will be returned.

  If this hook is not defined, then LIBCALL_VALUE will be used.

.. hook-end

.. c:macro:: FUNCTION_VALUE_REGNO_P (regno)

  A C expression that is nonzero if :samp:`{regno}` is the number of a hard
  register in which the values of called function may come back.

  A register whose use for returning values is limited to serving as the
  second of a pair (for a value of type ``double``, say) need not be
  recognized by this macro.  So for most machines, this definition
  suffices:

  .. code-block:: c++

    #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

  If the machine has register windows, so that the caller and the called
  function use different registers for the return value, this macro
  should recognize only the caller's register numbers.

  This macro has been deprecated.  Use ``TARGET_FUNCTION_VALUE_REGNO_P``
  for a new target instead.

.. function:: bool TARGET_FUNCTION_VALUE_REGNO_P (const unsigned int regno)

  .. hook-start:TARGET_FUNCTION_VALUE_REGNO_P

  A target hook that return ``true`` if :samp:`{regno}` is the number of a hard
  register in which the values of called function may come back.

  A register whose use for returning values is limited to serving as the
  second of a pair (for a value of type ``double``, say) need not be
  recognized by this target hook.

  If the machine has register windows, so that the caller and the called
  function use different registers for the return value, this target hook
  should recognize only the caller's register numbers.

  If this hook is not defined, then FUNCTION_VALUE_REGNO_P will be used.

.. hook-end

.. c:macro:: APPLY_RESULT_SIZE

  Define this macro if :samp:`untyped_call` and :samp:`untyped_return`
  need more space than is implied by ``FUNCTION_VALUE_REGNO_P`` for
  saving and restoring an arbitrary return value.

.. c:var:: bool TARGET_OMIT_STRUCT_RETURN_REG

  .. hook-start:TARGET_OMIT_STRUCT_RETURN_REG

  Normally, when a function returns a structure by memory, the address
  is passed as an invisible pointer argument, but the compiler also
  arranges to return the address from the function like it would a normal
  pointer return value.  Define this to true if that behavior is
  undesirable on your target.

.. hook-end

.. function:: bool TARGET_RETURN_IN_MSB (const_tree type)

  .. hook-start:TARGET_RETURN_IN_MSB

  This hook should return true if values of type :samp:`{type}` are returned
  at the most significant end of a register (in other words, if they are
  padded at the least significant end).  You can assume that :samp:`{type}`
  is returned in a register; the caller is required to check this.

  Note that the register provided by ``TARGET_FUNCTION_VALUE`` must
  be able to hold the complete return value.  For example, if a 1-, 2-
  or 3-byte structure is returned at the most significant end of a
  4-byte register, ``TARGET_FUNCTION_VALUE`` should provide an
  ``SImode`` rtx.

.. hook-end