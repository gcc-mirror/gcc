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

.. function:: scalar_int_mode TARGET_ADDR_SPACE_POINTER_MODE (addr_space_t address_space)

  .. hook-start:TARGET_ADDR_SPACE_POINTER_MODE

  Define this to return the machine mode to use for pointers to
  :samp:`{address_space}` if the target supports named address spaces.
  The default version of this hook returns ``ptr_mode``.

.. hook-end

.. function:: scalar_int_mode TARGET_ADDR_SPACE_ADDRESS_MODE (addr_space_t address_space)

  .. hook-start:TARGET_ADDR_SPACE_ADDRESS_MODE

  Define this to return the machine mode to use for addresses in
  :samp:`{address_space}` if the target supports named address spaces.
  The default version of this hook returns ``Pmode``.

.. hook-end

.. function:: bool TARGET_ADDR_SPACE_VALID_POINTER_MODE (scalar_int_mode mode, addr_space_t as)

  .. hook-start:TARGET_ADDR_SPACE_VALID_POINTER_MODE

  Define this to return nonzero if the port can handle pointers
  with machine mode :samp:`{mode}` to address space :samp:`{as}`.  This target
  hook is the same as the ``TARGET_VALID_POINTER_MODE`` target hook,
  except that it includes explicit named address space support.  The default
  version of this hook returns true for the modes returned by either the
  ``TARGET_ADDR_SPACE_POINTER_MODE`` or ``TARGET_ADDR_SPACE_ADDRESS_MODE``
  target hooks for the given address space.

.. hook-end

.. function:: bool TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P (machine_mode mode, rtx exp, bool strict, addr_space_t as)

  .. hook-start:TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P

  Define this to return true if :samp:`{exp}` is a valid address for mode
  :samp:`{mode}` in the named address space :samp:`{as}`.  The :samp:`{strict}`
  parameter says whether strict addressing is in effect after reload has
  finished.  This target hook is the same as the
  ``TARGET_LEGITIMATE_ADDRESS_P`` target hook, except that it includes
  explicit named address space support.

.. hook-end

.. function:: rtx TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS (rtx x, rtx oldx, machine_mode mode, addr_space_t as)

  .. hook-start:TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS

  Define this to modify an invalid address :samp:`{x}` to be a valid address
  with mode :samp:`{mode}` in the named address space :samp:`{as}`.  This target
  hook is the same as the ``TARGET_LEGITIMIZE_ADDRESS`` target hook,
  except that it includes explicit named address space support.

.. hook-end

.. function:: bool TARGET_ADDR_SPACE_SUBSET_P (addr_space_t subset, addr_space_t superset)

  .. hook-start:TARGET_ADDR_SPACE_SUBSET_P

  Define this to return whether the :samp:`{subset}` named address space is
  contained within the :samp:`{superset}` named address space.  Pointers to
  a named address space that is a subset of another named address space
  will be converted automatically without a cast if used together in
  arithmetic operations.  Pointers to a superset address space can be
  converted to pointers to a subset address space via explicit casts.

.. hook-end

.. function:: bool TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID (addr_space_t as)

  .. hook-start:TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID

  Define this to modify the default handling of address 0 for the
  address space.  Return true if 0 should be considered a valid address.

.. hook-end

.. function:: rtx TARGET_ADDR_SPACE_CONVERT (rtx op, tree from_type, tree to_type)

  .. hook-start:TARGET_ADDR_SPACE_CONVERT

  Define this to convert the pointer expression represented by the RTL
  :samp:`{op}` with type :samp:`{from_type}` that points to a named address
  space to a new pointer expression with type :samp:`{to_type}` that points
  to a different named address space.  When this hook it called, it is
  guaranteed that one of the two address spaces is a subset of the other,
  as determined by the ``TARGET_ADDR_SPACE_SUBSET_P`` target hook.

.. hook-end

.. function:: int TARGET_ADDR_SPACE_DEBUG (addr_space_t as)

  .. hook-start:TARGET_ADDR_SPACE_DEBUG

  Define this to define how the address space is encoded in dwarf.
  The result is the value to be used with ``DW_AT_address_class``.

.. hook-end

.. function:: void TARGET_ADDR_SPACE_DIAGNOSE_USAGE (addr_space_t as, location_t loc)

  .. hook-start:TARGET_ADDR_SPACE_DIAGNOSE_USAGE

  Define this hook if the availability of an address space depends on
  command line options and some diagnostics should be printed when the
  address space is used.  This hook is called during parsing and allows
  to emit a better diagnostic compared to the case where the address space
  was not registered with ``c_register_addr_space``.  :samp:`{as}` is
  the address space as registered with ``c_register_addr_space``.
  :samp:`{loc}` is the location of the address space qualifier token.
  The default implementation does nothing.

.. hook-end