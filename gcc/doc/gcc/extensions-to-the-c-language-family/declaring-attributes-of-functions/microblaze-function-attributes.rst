..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _microblaze-function-attributes:

MicroBlaze Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on MicroBlaze targets:

.. index:: save_volatiles function attribute, MicroBlaze

.. microblaze-fn-attr:: save_volatiles

  Use this attribute to indicate that the function is
  an interrupt handler.  All volatile registers (in addition to non-volatile
  registers) are saved in the function prologue.  If the function is a leaf
  function, only volatiles used by the function are saved.  A normal function
  return is generated instead of a return from interrupt.

.. index:: break_handler function attribute, MicroBlaze, break handler functions

.. microblaze-fn-attr:: break_handler

  Use this attribute to indicate that
  the specified function is a break handler.  The compiler generates function
  entry and exit sequences suitable for use in an break handler when this
  attribute is present. The return from :microblaze-fn-attr:`break_handler` is done through
  the ``rtbd`` instead of ``rtsd``.

  .. code-block:: c++

    void f () __attribute__ ((break_handler));

.. index:: interrupt_handler function attribute, MicroBlaze, fast_interrupt function attribute, MicroBlaze

.. microblaze-fn-attr:: interrupt_handler, fast_interrupt

  These attributes indicate that the specified function is an interrupt
  handler.  Use the :microblaze-fn-attr:`fast_interrupt` attribute to indicate handlers
  used in low-latency interrupt mode, and :microblaze-fn-attr:`interrupt_handler` for
  interrupts that do not use low-latency handlers.  In both cases, GCC
  emits appropriate prologue code and generates a return from the handler
  using ``rtid`` instead of ``rtsd``.
