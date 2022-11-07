..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _h8-300-function-attributes:

H8/300 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are available for H8/300 targets:

.. index:: function_vector function attribute, H8/300

.. h8-300-fn-attr:: function_vector

  Use this attribute on the H8/300, H8/300H, and H8S to indicate
  that the specified function should be called through the function vector.
  Calling a function through the function vector reduces code size; however,
  the function vector has a limited size (maximum 128 entries on the H8/300
  and 64 entries on the H8/300H and H8S)
  and shares space with the interrupt vector.

.. index:: interrupt_handler function attribute, H8/300

.. h8-300-fn-attr:: interrupt_handler

  Use this attribute on the H8/300, H8/300H, and H8S to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. index:: saveall function attribute, H8/300, save all registers on the H8/300, H8/300H, and H8S

.. h8-300-fn-attr:: saveall

  Use this attribute on the H8/300, H8/300H, and H8S to indicate that
  all registers except the stack pointer should be saved in the prologue
  regardless of whether they are used or not.