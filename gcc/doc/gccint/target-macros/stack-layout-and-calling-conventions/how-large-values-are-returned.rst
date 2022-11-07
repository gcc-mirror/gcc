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

.. include:: ../tm.rst.in
  :start-after: [TARGET_RETURN_IN_MEMORY]
  :end-before: [TARGET_RETURN_IN_MEMORY]


.. c:macro:: DEFAULT_PCC_STRUCT_RETURN

  Define this macro to be 1 if all structure and union return values must be
  in memory.  Since this results in slower code, this should be defined
  only if needed for compatibility with other compilers or with an ABI.
  If you define this macro to be 0, then the conventions used for structure
  and union return values are decided by the ``TARGET_RETURN_IN_MEMORY``
  target hook.

  If not defined, this defaults to the value 1.

.. include:: ../tm.rst.in
  :start-after: [TARGET_STRUCT_VALUE_RTX]
  :end-before: [TARGET_STRUCT_VALUE_RTX]


.. c:macro:: PCC_STATIC_STRUCT_RETURN

  Define this macro if the usual system convention on the target machine
  for returning structures and unions is for the called function to return
  the address of a static variable containing the value.

  Do not define this if the usual system convention is for the caller to
  pass an address to the subroutine.

  This macro has effect in :option:`-fpcc-struct-return` mode, but it does
  nothing when you use :option:`-freg-struct-return` mode.

.. include:: ../tm.rst.in
  :start-after: [TARGET_GET_RAW_RESULT_MODE]
  :end-before: [TARGET_GET_RAW_RESULT_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_GET_RAW_ARG_MODE]
  :end-before: [TARGET_GET_RAW_ARG_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_EMPTY_RECORD_P]
  :end-before: [TARGET_EMPTY_RECORD_P]


.. include:: ../tm.rst.in
  :start-after: [TARGET_WARN_PARAMETER_PASSING_ABI]
  :end-before: [TARGET_WARN_PARAMETER_PASSING_ABI]
