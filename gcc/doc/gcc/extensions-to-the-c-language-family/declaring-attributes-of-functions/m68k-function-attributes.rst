..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _m68k-function-attributes:

m68k Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the m68k back end:

.. index:: interrupt function attribute, m68k, interrupt_handler function attribute, m68k

.. m68k-fn-attr:: interrupt, interrupt_handler

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.  Either name may be used.

.. index:: interrupt_thread function attribute, fido

.. m68k-fn-attr:: interrupt_thread

  Use this attribute on fido, a subarchitecture of the m68k, to indicate
  that the specified function is an interrupt handler that is designed
  to run as a thread.  The compiler omits generate prologue/epilogue
  sequences and replaces the return instruction with a ``sleep``
  instruction.  This attribute is available only on fido.