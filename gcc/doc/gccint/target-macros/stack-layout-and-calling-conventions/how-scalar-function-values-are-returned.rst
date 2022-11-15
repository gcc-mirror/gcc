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

.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_VALUE]
  :end-before: [TARGET_FUNCTION_VALUE]


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

.. include:: ../tm.rst.in
  :start-after: [TARGET_LIBCALL_VALUE]
  :end-before: [TARGET_LIBCALL_VALUE]


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

.. include:: ../tm.rst.in
  :start-after: [TARGET_FUNCTION_VALUE_REGNO_P]
  :end-before: [TARGET_FUNCTION_VALUE_REGNO_P]


.. c:macro:: APPLY_RESULT_SIZE

  Define this macro if :samp:`untyped_call` and :samp:`untyped_return`
  need more space than is implied by ``FUNCTION_VALUE_REGNO_P`` for
  saving and restoring an arbitrary return value.

.. include:: ../tm.rst.in
  :start-after: [TARGET_OMIT_STRUCT_RETURN_REG]
  :end-before: [TARGET_OMIT_STRUCT_RETURN_REG]


.. include:: ../tm.rst.in
  :start-after: [TARGET_RETURN_IN_MSB]
  :end-before: [TARGET_RETURN_IN_MSB]
