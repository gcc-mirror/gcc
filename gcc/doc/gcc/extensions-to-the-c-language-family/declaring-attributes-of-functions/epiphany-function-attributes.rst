..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _epiphany-function-attributes:

Epiphany Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Epiphany back end:

.. index:: disinterrupt function attribute, Epiphany

.. epiphany-fn-attr:: disinterrupt

  This attribute causes the compiler to emit
  instructions to disable interrupts for the duration of the given
  function.

.. index:: forwarder_section function attribute, Epiphany

.. epiphany-fn-attr:: forwarder_section

  This attribute modifies the behavior of an interrupt handler.
  The interrupt handler may be in external memory which cannot be
  reached by a branch instruction, so generate a local memory trampoline
  to transfer control.  The single parameter identifies the section where
  the trampoline is placed.

.. index:: interrupt function attribute, Epiphany

.. epiphany-fn-attr:: interrupt

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.  It may also generate
  a special section with code to initialize the interrupt vector table.

  On Epiphany targets one or more optional parameters can be added like this:

  .. code-block:: c++

    void __attribute__ ((interrupt ("dma0, dma1"))) universal_dma_handler ();

  Permissible values for these parameters are: :epiphany-fn-attr:`reset`,
  ``software_exception``, ``page_miss``,
  ``timer0``, ``timer1``, ``message``,
  ``dma0``, ``dma1``, ``wand`` and ``swi``.
  Multiple parameters indicate that multiple entries in the interrupt
  vector table should be initialized for this function, i.e. for each
  parameter :samp:`{name}`, a jump to the function is emitted in
  the section ivt_entry\_ :samp:`{name}`.  The parameter(s) may be omitted
  entirely, in which case no interrupt vector table entry is provided.

  Note that interrupts are enabled inside the function
  unless the :epiphany-fn-attr:`disinterrupt` attribute is also specified.

  The following examples are all valid uses of these attributes on
  Epiphany targets:

  .. code-block:: c++

    void __attribute__ ((interrupt)) universal_handler ();
    void __attribute__ ((interrupt ("dma1"))) dma1_handler ();
    void __attribute__ ((interrupt ("dma0, dma1")))
      universal_dma_handler ();
    void __attribute__ ((interrupt ("timer0"), disinterrupt))
      fast_timer_handler ();
    void __attribute__ ((interrupt ("dma0, dma1"),
                         forwarder_section ("tramp")))
      external_dma_handler ();

.. index:: long_call function attribute, Epiphany, short_call function attribute, Epiphany, indirect calls, Epiphany

.. epiphany-fn-attr:: long_call, short_call

  These attributes specify how a particular function is called.
  These attributes override the
  :option:`-mlong-calls` (see :ref:`adapteva-epiphany-options`)
  command-line switch and ``#pragma long_calls`` settings.