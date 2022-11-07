..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: aggregates as return values, large return values, returning aggregate values, structure value address

.. _aggregate-return:

How Large Values Are Returned
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a function value's mode is ``BLKmode`` (and in some other
cases), the value is not returned according to
``TARGET_FUNCTION_VALUE`` (see :ref:`scalar-return`).  Instead, the
caller passes the address of a block of memory in which the value
should be stored.  This address is called the :dfn:`structure value
address`.

This section describes how to control returning structure values in
memory.

.. function:: bool TARGET_RETURN_IN_MEMORY (const_tree type, const_tree fntype)

  .. hook-start:TARGET_RETURN_IN_MEMORY

  This target hook should return a nonzero value to say to return the
  function value in memory, just as large structures are always returned.
  Here :samp:`{type}` will be the data type of the value, and :samp:`{fntype}`
  will be the type of the function doing the returning, or ``NULL`` for
  libcalls.

  Note that values of mode ``BLKmode`` must be explicitly handled
  by this function.  Also, the option :option:`-fpcc-struct-return`
  takes effect regardless of this macro.  On most systems, it is
  possible to leave the hook undefined; this causes a default
  definition to be used, whose value is the constant 1 for ``BLKmode``
  values, and 0 otherwise.

  Do not use this hook to indicate that structures and unions should always
  be returned in memory.  You should instead use ``DEFAULT_PCC_STRUCT_RETURN``
  to indicate this.

.. hook-end

.. c:macro:: DEFAULT_PCC_STRUCT_RETURN

  Define this macro to be 1 if all structure and union return values must be
  in memory.  Since this results in slower code, this should be defined
  only if needed for compatibility with other compilers or with an ABI.
  If you define this macro to be 0, then the conventions used for structure
  and union return values are decided by the ``TARGET_RETURN_IN_MEMORY``
  target hook.

  If not defined, this defaults to the value 1.

.. function:: rtx TARGET_STRUCT_VALUE_RTX (tree fndecl, int incoming)

  .. hook-start:TARGET_STRUCT_VALUE_RTX

  This target hook should return the location of the structure value
  address (normally a ``mem`` or ``reg``), or 0 if the address is
  passed as an 'invisible' first argument.  Note that :samp:`{fndecl}` may
  be ``NULL``, for libcalls.  You do not need to define this target
  hook if the address is always passed as an 'invisible' first
  argument.

  On some architectures the place where the structure value address
  is found by the called function is not the same place that the
  caller put it.  This can be due to register windows, or it could
  be because the function prologue moves it to a different place.
  :samp:`{incoming}` is ``1`` or ``2`` when the location is needed in
  the context of the called function, and ``0`` in the context of
  the caller.

  If :samp:`{incoming}` is nonzero and the address is to be found on the
  stack, return a ``mem`` which refers to the frame pointer. If
  :samp:`{incoming}` is ``2``, the result is being used to fetch the
  structure value address at the beginning of a function.  If you need
  to emit adjusting code, you should do it at this point.

.. hook-end

.. c:macro:: PCC_STATIC_STRUCT_RETURN

  Define this macro if the usual system convention on the target machine
  for returning structures and unions is for the called function to return
  the address of a static variable containing the value.

  Do not define this if the usual system convention is for the caller to
  pass an address to the subroutine.

  This macro has effect in :option:`-fpcc-struct-return` mode, but it does
  nothing when you use :option:`-freg-struct-return` mode.

.. function:: fixed_size_mode TARGET_GET_RAW_RESULT_MODE (int regno)

  .. hook-start:TARGET_GET_RAW_RESULT_MODE

  This target hook returns the mode to be used when accessing raw return
  registers in ``__builtin_return``.  Define this macro if the value
  in :samp:`{reg_raw_mode}` is not correct.

.. hook-end

.. function:: fixed_size_mode TARGET_GET_RAW_ARG_MODE (int regno)

  .. hook-start:TARGET_GET_RAW_ARG_MODE

  This target hook returns the mode to be used when accessing raw argument
  registers in ``__builtin_apply_args``.  Define this macro if the value
  in :samp:`{reg_raw_mode}` is not correct.

.. hook-end

.. function:: bool TARGET_EMPTY_RECORD_P (const_tree type)

  .. hook-start:TARGET_EMPTY_RECORD_P

  This target hook returns true if the type is an empty record.  The default
  is to return ``false``.

.. hook-end

.. function:: void TARGET_WARN_PARAMETER_PASSING_ABI (cumulative_args_t ca, tree type)

  .. hook-start:TARGET_WARN_PARAMETER_PASSING_ABI

  This target hook warns about the change in empty class parameter passing
  ABI.

.. hook-end