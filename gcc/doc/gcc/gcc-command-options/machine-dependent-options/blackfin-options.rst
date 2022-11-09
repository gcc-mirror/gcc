..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Blackfin

.. index:: Blackfin Options

.. _blackfin-options:

Blackfin Options
^^^^^^^^^^^^^^^^

.. option:: -mcpu={cpu}[-{sirevision}]

  Specifies the name of the target Blackfin processor.  Currently, :samp:`{cpu}`
  can be one of :samp:`bf512`, :samp:`bf514`, :samp:`bf516`, :samp:`bf518`,
  :samp:`bf522`, :samp:`bf523`, :samp:`bf524`, :samp:`bf525`, :samp:`bf526`,
  :samp:`bf527`, :samp:`bf531`, :samp:`bf532`, :samp:`bf533`,
  :samp:`bf534`, :samp:`bf536`, :samp:`bf537`, :samp:`bf538`, :samp:`bf539`,
  :samp:`bf542`, :samp:`bf544`, :samp:`bf547`, :samp:`bf548`, :samp:`bf549`,
  :samp:`bf542m`, :samp:`bf544m`, :samp:`bf547m`, :samp:`bf548m`, :samp:`bf549m`,
  :samp:`bf561`, :samp:`bf592`.

  The optional :samp:`{sirevision}` specifies the silicon revision of the target
  Blackfin processor.  Any workarounds available for the targeted silicon revision
  are enabled.  If :samp:`{sirevision}` is :samp:`none`, no workarounds are enabled.
  If :samp:`{sirevision}` is :samp:`any`, all workarounds for the targeted processor
  are enabled.  The ``__SILICON_REVISION__`` macro is defined to two
  hexadecimal digits representing the major and minor numbers in the silicon
  revision.  If :samp:`{sirevision}` is :samp:`none`, the ``__SILICON_REVISION__``
  is not defined.  If :samp:`{sirevision}` is :samp:`any`, the
  ``__SILICON_REVISION__`` is defined to be ``0xffff``.
  If this optional :samp:`{sirevision}` is not used, GCC assumes the latest known
  silicon revision of the targeted Blackfin processor.

  GCC defines a preprocessor macro for the specified :samp:`{cpu}`.
  For the :samp:`bfin-elf` toolchain, this option causes the hardware BSP
  provided by libgloss to be linked in if :option:`-msim` is not given.

  Without this option, :samp:`bf532` is used as the processor by default.

  Note that support for :samp:`bf561` is incomplete.  For :samp:`bf561`,
  only the preprocessor macro is defined.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  the simulator BSP provided by libgloss to be linked in.  This option
  has effect only for :samp:`bfin-elf` toolchain.
  Certain other options, such as :option:`-mid-shared-library` and
  :option:`-mfdpic`, imply :option:`-msim`.

.. option:: -momit-leaf-frame-pointer

  Don't keep the frame pointer in a register for leaf functions.  This
  avoids the instructions to save, set up and restore frame pointers and
  makes an extra register available in leaf functions.

.. option:: -mspecld-anomaly

  When enabled, the compiler ensures that the generated code does not
  contain speculative loads after jump instructions. If this option is used,
  ``__WORKAROUND_SPECULATIVE_LOADS`` is defined.

.. option:: -mno-specld-anomaly

  Don't generate extra code to prevent speculative loads from occurring.

.. option:: -mspecld-anomaly

  Default setting; overrides :option:`-mno-specld-anomaly`.

.. option:: -mcsync-anomaly

  When enabled, the compiler ensures that the generated code does not
  contain CSYNC or SSYNC instructions too soon after conditional branches.
  If this option is used, ``__WORKAROUND_SPECULATIVE_SYNCS`` is defined.

.. option:: -mno-csync-anomaly

  Don't generate extra code to prevent CSYNC or SSYNC instructions from
  occurring too soon after a conditional branch.

.. option:: -mcsync-anomaly

  Default setting; overrides :option:`-mno-csync-anomaly`.

.. option:: -mlow64k

  When enabled, the compiler is free to take advantage of the knowledge that
  the entire program fits into the low 64k of memory.

.. option:: -mno-low64k

  Assume that the program is arbitrarily large.  This is the default.

.. option:: -mstack-check-l1

  Do stack checking using information placed into L1 scratchpad memory by the
  uClinux kernel.

.. option:: -mid-shared-library

  Generate code that supports shared libraries via the library ID method.
  This allows for execute in place and shared libraries in an environment
  without virtual memory management.  This option implies :option:`-fPIC`.
  With a :samp:`bfin-elf` target, this option implies :option:`-msim`.

.. option:: -mno-id-shared-library

  Generate code that doesn't assume ID-based shared libraries are being used.
  This is the default.

.. option:: -mid-shared-library

  Default setting; overrides :option:`-mno-id-shared-library`.

.. option:: -mleaf-id-shared-library

  Generate code that supports shared libraries via the library ID method,
  but assumes that this library or executable won't link against any other
  ID shared libraries.  That allows the compiler to use faster code for jumps
  and calls.

.. option:: -mno-leaf-id-shared-library

  Do not assume that the code being compiled won't link against any ID shared
  libraries.  Slower code is generated for jump and call insns.

.. option:: -mleaf-id-shared-library

  Default setting; overrides :option:`-mno-leaf-id-shared-library`.

.. option:: -mshared-library-id=n

  Specifies the identification number of the ID-based shared library being
  compiled.  Specifying a value of 0 generates more compact code; specifying
  other values forces the allocation of that number to the current
  library but is no more space- or time-efficient than omitting this option.

.. option:: -msep-data

  Generate code that allows the data segment to be located in a different
  area of memory from the text segment.  This allows for execute in place in
  an environment without virtual memory management by eliminating relocations
  against the text section.

.. option:: -mno-sep-data

  Generate code that assumes that the data segment follows the text segment.
  This is the default.

.. option:: -msep-data

  Default setting; overrides :option:`-mno-sep-data`.

.. option:: -mlong-calls, -mno-long-calls

  Tells the compiler to perform function calls by first loading the
  address of the function into a register and then performing a subroutine
  call on this register.  This switch is needed if the target function
  lies outside of the 24-bit addressing range of the offset-based
  version of subroutine call instruction.

  This feature is not enabled by default.  Specifying
  :option:`-mno-long-calls` restores the default behavior.  Note these
  switches have no effect on how the compiler generates code to handle
  function calls via function pointers.

.. option:: -mfast-fp

  Link with the fast floating-point library. This library relaxes some of
  the IEEE floating-point standard's rules for checking inputs against
  Not-a-Number (NAN), in the interest of performance.

.. option:: -minline-plt

  Enable inlining of PLT entries in function calls to functions that are
  not known to bind locally.  It has no effect without :option:`-mfdpic`.

.. option:: -mmulticore

  Build a standalone application for multicore Blackfin processors.
  This option causes proper start files and link scripts supporting
  multicore to be used, and defines the macro ``__BFIN_MULTICORE``.
  It can only be used with :option:`-mcpu=bf561[-sirevision]`.

  This option can be used with :option:`-mcorea` or :option:`-mcoreb`, which
  selects the one-application-per-core programming model.  Without
  :option:`-mcorea` or :option:`-mcoreb`, the single-application/dual-core
  programming model is used. In this model, the main function of Core B
  should be named as ``coreb_main``.

  If this option is not used, the single-core application programming
  model is used.

.. option:: -mcorea

  Build a standalone application for Core A of BF561 when using
  the one-application-per-core programming model. Proper start files
  and link scripts are used to support Core A, and the macro
  ``__BFIN_COREA`` is defined.
  This option can only be used in conjunction with :option:`-mmulticore`.

.. option:: -mcoreb

  Build a standalone application for Core B of BF561 when using
  the one-application-per-core programming model. Proper start files
  and link scripts are used to support Core B, and the macro
  ``__BFIN_COREB`` is defined. When this option is used, ``coreb_main``
  should be used instead of ``main``.
  This option can only be used in conjunction with :option:`-mmulticore`.

.. option:: -msdram

  Build a standalone application for SDRAM. Proper start files and
  link scripts are used to put the application into SDRAM, and the macro
  ``__BFIN_SDRAM`` is defined.
  The loader should initialize SDRAM before loading the application.

.. option:: -micplb

  Assume that ICPLBs are enabled at run time.  This has an effect on certain
  anomaly workarounds.  For Linux targets, the default is to assume ICPLBs
  are enabled; for standalone applications the default is off.
