..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: position independent code, PIC

.. _pic:

Position Independent Code
*************************

This section describes macros that help implement generation of position
independent code.  Simply defining these macros is not enough to
generate valid PIC; you must also add support to the hook
``TARGET_LEGITIMATE_ADDRESS_P`` and to the macro
``PRINT_OPERAND_ADDRESS``, as well as ``LEGITIMIZE_ADDRESS``.  You
must modify the definition of :samp:`movsi` to do something appropriate
when the source operand contains a symbolic address.  You may also
need to alter the handling of switch statements so that they use
relative addresses.

.. i rearranged the order of the macros above to try to force one of

.. them to the next line, to eliminate an overfull hbox. -mew 10feb93

.. c:macro:: PIC_OFFSET_TABLE_REGNUM

  The register number of the register used to address a table of static
  data addresses in memory.  In some cases this register is defined by a
  processor's 'application binary interface' (ABI).  When this macro
  is defined, RTL is generated for this register once, as with the stack
  pointer and frame pointer registers.  If this macro is not defined, it
  is up to the machine-dependent files to allocate such a register (if
  necessary).  Note that this register must be fixed when in use (e.g.
  when ``flag_pic`` is true).

.. c:macro:: PIC_OFFSET_TABLE_REG_CALL_CLOBBERED

  A C expression that is nonzero if the register defined by
  ``PIC_OFFSET_TABLE_REGNUM`` is clobbered by calls.  If not defined,
  the default is zero.  Do not define
  this macro if ``PIC_OFFSET_TABLE_REGNUM`` is not defined.

.. c:macro:: LEGITIMATE_PIC_OPERAND_P (x)

  A C expression that is nonzero if :samp:`{x}` is a legitimate immediate
  operand on the target machine when generating position independent code.
  You can assume that :samp:`{x}` satisfies ``CONSTANT_P``, so you need not
  check this.  You can also assume :samp:`{flag_pic}` is true, so you need not
  check it either.  You need not define this macro if all constants
  (including ``SYMBOL_REF``) can be immediate operands when generating
  position independent code.
