..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _v850-function-attributes:

V850 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

The V850 back end supports these function attributes:

.. index:: interrupt function attribute, V850, interrupt_handler function attribute, V850

.. v850-fn-attr:: interrupt, interrupt_handler

  Use these attributes to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when either attribute is present.
