..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _xstormy16-function-attributes:

Xstormy16 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Xstormy16 back end:

.. index:: interrupt function attribute, Xstormy16

.. xstormy16-fn-attr:: interrupt

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.
