..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _m32r-d-function-attributes:

M32R/D Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the M32R/D back end:

.. index:: interrupt function attribute, M32R/D

.. m32r-d-fn-attr:: interrupt

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. index:: model function attribute, M32R/D, function addressability on the M32R/D

.. m32r-d-fn-attr:: model (model-name)

  On the M32R/D, use this attribute to set the addressability of an
  object, and of the code generated for a function.  The identifier
  :samp:`{model-name}` is one of ``small``, ``medium``, or
  ``large``, representing each of the code models.

  Small model objects live in the lower 16MB of memory (so that their
  addresses can be loaded with the ``ld24`` instruction), and are
  callable with the ``bl`` instruction.

  Medium model objects may live anywhere in the 32-bit address space (the
  compiler generates ``seth/add3`` instructions to load their addresses),
  and are callable with the ``bl`` instruction.

  Large model objects may live anywhere in the 32-bit address space (the
  compiler generates ``seth/add3`` instructions to load their addresses),
  and may not be reachable with the ``bl`` instruction (the compiler
  generates the much slower ``seth/add3/jl`` instruction sequence).
