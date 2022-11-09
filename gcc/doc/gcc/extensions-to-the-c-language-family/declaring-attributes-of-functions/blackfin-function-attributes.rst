..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _blackfin-function-attributes:

Blackfin Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Blackfin back end:

.. index:: exception_handler function attribute, exception handler functions, Blackfin

.. blackfin-fn-attr:: exception_handler

  Use this attribute on the Blackfin to indicate that the specified function
  is an exception handler.  The compiler generates function entry and
  exit sequences suitable for use in an exception handler when this
  attribute is present.

.. index:: interrupt_handler function attribute, Blackfin

.. blackfin-fn-attr:: interrupt_handler

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. index:: kspisusp function attribute, Blackfin, User stack pointer in interrupts on the Blackfin

.. blackfin-fn-attr:: kspisusp

  When used together with :blackfin-fn-attr:`interrupt_handler`, :blackfin-fn-attr:`exception_handler`
  or :blackfin-fn-attr:`nmi_handler`, code is generated to load the stack pointer
  from the USP register in the function prologue.

.. index:: l1_text function attribute, Blackfin

.. blackfin-fn-attr:: l1_text

  This attribute specifies a function to be placed into L1 Instruction
  SRAM. The function is put into a specific section named ``.l1.text``.
  With :option:`-mfdpic`, function calls with a such function as the callee
  or caller uses inlined PLT.

.. index:: l2 function attribute, Blackfin

.. blackfin-fn-attr:: l2

  This attribute specifies a function to be placed into L2
  SRAM. The function is put into a specific section named
  ``.l2.text``. With :option:`-mfdpic`, callers of such functions use
  an inlined PLT.

.. index:: indirect calls, Blackfin, longcall function attribute, Blackfin, shortcall function attribute, Blackfin

.. blackfin-fn-attr:: longcall, shortcall

  The :blackfin-fn-attr:`longcall` attribute
  indicates that the function might be far away from the call site and
  require a different (more expensive) calling sequence.  The
  ``shortcall`` attribute indicates that the function is always close
  enough for the shorter calling sequence to be used.  These attributes
  override the :option:`-mlongcall` switch.

.. index:: nesting function attribute, Blackfin, Allow nesting in an interrupt handler on the Blackfin processor

.. blackfin-fn-attr:: nesting

  Use this attribute together with :blackfin-fn-attr:`interrupt_handler`,
  :blackfin-fn-attr:`exception_handler` or :blackfin-fn-attr:`nmi_handler` to indicate that the function
  entry code should enable nested interrupts or exceptions.

.. index:: nmi_handler function attribute, Blackfin, NMI handler functions on the Blackfin processor

.. blackfin-fn-attr:: nmi_handler

  Use this attribute on the Blackfin to indicate that the specified function
  is an NMI handler.  The compiler generates function entry and
  exit sequences suitable for use in an NMI handler when this
  attribute is present.

.. index:: saveall function attribute, Blackfin, save all registers on the Blackfin

.. blackfin-fn-attr:: saveall

  Use this attribute to indicate that
  all registers except the stack pointer should be saved in the prologue
  regardless of whether they are used or not.
