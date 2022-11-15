..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: V850

.. index:: V850 Options

.. _v850-options:

V850 Options
^^^^^^^^^^^^

These :samp:`-m` options are defined for V850 implementations:

.. option:: -mlong-calls, -mno-long-calls

  Treat all calls as being far away (near).  If calls are assumed to be
  far away, the compiler always loads the function's address into a
  register, and calls indirect through the pointer.

.. option:: -mno-ep, -mep

  Do not optimize (do optimize) basic blocks that use the same index
  pointer 4 or more times to copy pointer into the ``ep`` register, and
  use the shorter ``sld`` and ``sst`` instructions.  The :option:`-mep`
  option is on by default if you optimize.

.. option:: -mno-prolog-function, -mprolog-function

  Do not use (do use) external functions to save and restore registers
  at the prologue and epilogue of a function.  The external functions
  are slower, but use less code space if more than one function saves
  the same number of registers.  The :option:`-mprolog-function` option
  is on by default if you optimize.

.. option:: -mspace

  Try to make the code as small as possible.  At present, this just turns
  on the :option:`-mep` and :option:`-mprolog-function` options.

.. option:: -mtda={n}

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the tiny data area that register ``ep`` points to.  The tiny data
  area can hold up to 256 bytes in total (128 bytes for byte references).

.. option:: -msda={n}

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the small data area that register ``gp`` points to.  The small data
  area can hold up to 64 kilobytes.

.. option:: -mzda={n}

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the first 32 kilobytes of memory.

.. option:: -mv850

  Specify that the target processor is the V850.

.. option:: -mv850e3v5

  Specify that the target processor is the V850E3V5.  The preprocessor
  constant ``__v850e3v5__`` is defined if this option is used.

.. option:: -mv850e2v4

  Specify that the target processor is the V850E3V5.  This is an alias for
  the :option:`-mv850e3v5` option.

.. option:: -mv850e2v3

  Specify that the target processor is the V850E2V3.  The preprocessor
  constant ``__v850e2v3__`` is defined if this option is used.

.. option:: -mv850e2

  Specify that the target processor is the V850E2.  The preprocessor
  constant ``__v850e2__`` is defined if this option is used.

.. option:: -mv850e1

  Specify that the target processor is the V850E1.  The preprocessor
  constants ``__v850e1__`` and ``__v850e__`` are defined if
  this option is used.

.. option:: -mv850es

  Specify that the target processor is the V850ES.  This is an alias for
  the :option:`-mv850e1` option.

.. option:: -mv850e

  Specify that the target processor is the V850E.  The preprocessor
  constant ``__v850e__`` is defined if this option is used.

  If neither :option:`-mv850` nor :option:`-mv850e` nor :option:`-mv850e1`
  nor :option:`-mv850e2` nor :option:`-mv850e2v3` nor :option:`-mv850e3v5`
  are defined then a default target processor is chosen and the
  relevant :samp:`__v850*__` preprocessor constant is defined.

  The preprocessor constants ``__v850`` and ``__v851__`` are always
  defined, regardless of which processor variant is the target.

.. option:: -mdisable-callt, -mno-disable-callt

  This option suppresses generation of the ``CALLT`` instruction for the
  v850e, v850e1, v850e2, v850e2v3 and v850e3v5 flavors of the v850
  architecture.

  This option is enabled by default when the RH850 ABI is
  in use (see :option:`-mrh850-abi`), and disabled by default when the
  GCC ABI is in use.  If ``CALLT`` instructions are being generated
  then the C preprocessor symbol ``__V850_CALLT__`` is defined.

.. option:: -mrelax, -mno-relax

  Pass on (or do not pass on) the :option:`-mrelax` command-line option
  to the assembler.

.. option:: -mlong-jumps, -mno-long-jumps

  Disable (or re-enable) the generation of PC-relative jump instructions.

.. option:: -msoft-float, -mhard-float

  Disable (or re-enable) the generation of hardware floating point
  instructions.  This option is only significant when the target
  architecture is :samp:`V850E2V3` or higher.  If hardware floating point
  instructions are being generated then the C preprocessor symbol
  ``__FPU_OK__`` is defined, otherwise the symbol
  ``__NO_FPU__`` is defined.

.. option:: -mloop

  Enables the use of the e3v5 LOOP instruction.  The use of this
  instruction is not enabled by default when the e3v5 architecture is
  selected because its use is still experimental.

.. option:: -mrh850-abi, -mghs

  Enables support for the RH850 version of the V850 ABI.  This is the
  default.  With this version of the ABI the following rules apply:

  * Integer sized structures and unions are returned via a memory pointer
    rather than a register.

  * Large structures and unions (more than 8 bytes in size) are passed by
    value.

  * Functions are aligned to 16-bit boundaries.

  * The :option:`-m8byte-align` command-line option is supported.

  * The :option:`-mdisable-callt` command-line option is enabled by
    default.  The :option:`-mno-disable-callt` command-line option is not
    supported.

  When this version of the ABI is enabled the C preprocessor symbol
  ``__V850_RH850_ABI__`` is defined.

.. option:: -mgcc-abi

  Enables support for the old GCC version of the V850 ABI.  With this
  version of the ABI the following rules apply:

  * Integer sized structures and unions are returned in register ``r10``.

  * Large structures and unions (more than 8 bytes in size) are passed by
    reference.

  * Functions are aligned to 32-bit boundaries, unless optimizing for
    size.

  * The :option:`-m8byte-align` command-line option is not supported.

  * The :option:`-mdisable-callt` command-line option is supported but not
    enabled by default.

  When this version of the ABI is enabled the C preprocessor symbol
  ``__V850_GCC_ABI__`` is defined.

.. option:: -m8byte-align, -mno-8byte-align

  Enables support for ``double`` and ``long long`` types to be
  aligned on 8-byte boundaries.  The default is to restrict the
  alignment of all objects to at most 4-bytes.  When
  :option:`-m8byte-align` is in effect the C preprocessor symbol
  ``__V850_8BYTE_ALIGN__`` is defined.

.. option:: -mbig-switch

  Generate code suitable for big switch tables.  Use this option only if
  the assembler/linker complain about out of range branches within a switch
  table.

.. option:: -mapp-regs

  This option causes r2 and r5 to be used in the code generated by
  the compiler.  This setting is the default.

.. option:: -mno-app-regs

  This option causes r2 and r5 to be treated as fixed registers.
