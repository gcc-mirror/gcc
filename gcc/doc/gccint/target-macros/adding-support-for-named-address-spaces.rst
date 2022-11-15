..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: named address spaces

.. _named-address-spaces:

Adding support for named address spaces
***************************************

The draft technical report of the ISO/IEC JTC1 S22 WG14 N1275
standards committee, Programming Languages - C - Extensions to
support embedded processors, specifies a syntax for embedded
processors to specify alternate address spaces.  You can configure a
GCC port to support section 5.1 of the draft report to add support for
address spaces other than the default address space.  These address
spaces are new keywords that are similar to the ``volatile`` and
``const`` type attributes.

Pointers to named address spaces can have a different size than
pointers to the generic address space.

For example, the SPU port uses the ``__ea`` address space to refer
to memory in the host processor, rather than memory local to the SPU
processor.  Access to memory in the ``__ea`` address space involves
issuing DMA operations to move data between the host processor and the
local processor memory address space.  Pointers in the ``__ea``
address space are either 32 bits or 64 bits based on the
:option:`-mea32` or :option:`-mea64` switches (native SPU pointers are
always 32 bits).

Internally, address spaces are represented as a small integer in the
range 0 to 15 with address space 0 being reserved for the generic
address space.

To register a named address space qualifier keyword with the C front end,
the target may call the ``c_register_addr_space`` routine.  For example,
the SPU port uses the following to declare ``__ea`` as the keyword for
named address space #1:

.. code-block:: c++

  #define ADDR_SPACE_EA 1
  c_register_addr_space ("__ea", ADDR_SPACE_EA);

.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_POINTER_MODE]
  :end-before: [TARGET_ADDR_SPACE_POINTER_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_ADDRESS_MODE]
  :end-before: [TARGET_ADDR_SPACE_ADDRESS_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_VALID_POINTER_MODE]
  :end-before: [TARGET_ADDR_SPACE_VALID_POINTER_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P]
  :end-before: [TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS]
  :end-before: [TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_SUBSET_P]
  :end-before: [TARGET_ADDR_SPACE_SUBSET_P]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID]
  :end-before: [TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_CONVERT]
  :end-before: [TARGET_ADDR_SPACE_CONVERT]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_DEBUG]
  :end-before: [TARGET_ADDR_SPACE_DEBUG]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDR_SPACE_DIAGNOSE_USAGE]
  :end-before: [TARGET_ADDR_SPACE_DIAGNOSE_USAGE]
