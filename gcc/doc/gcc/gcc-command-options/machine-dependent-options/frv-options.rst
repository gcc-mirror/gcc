..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: FRV

.. index:: FRV Options

.. _frv-options:

FRV Options
^^^^^^^^^^^

.. option:: -mgpr-32

  Only use the first 32 general-purpose registers.

.. option:: -mgpr-64

  Use all 64 general-purpose registers.

.. option:: -mfpr-32

  Use only the first 32 floating-point registers.

.. option:: -mfpr-64

  Use all 64 floating-point registers.

.. option:: -mhard-float

  Use hardware instructions for floating-point operations.

.. option:: -msoft-float

  Use library routines for floating-point operations.

.. option:: -malloc-cc

  Dynamically allocate condition code registers.

.. option:: -mfixed-cc

  Do not try to dynamically allocate condition code registers, only
  use ``icc0`` and ``fcc0``.

.. option:: -mdword

  Change ABI to use double word insns.

.. option:: -mno-dword

  Do not use double word instructions.

.. option:: -mdword

  Default setting; overrides :option:`-mno-dword`.

.. option:: -mdouble

  Use floating-point double instructions.

.. option:: -mno-double

  Do not use floating-point double instructions.

.. option:: -mmedia

  Use media instructions.

.. option:: -mno-media

  Do not use media instructions.

.. option:: -mmuladd

  Use multiply and add/subtract instructions.

.. option:: -mno-muladd

  Do not use multiply and add/subtract instructions.

.. option:: -mfdpic

  Select the FDPIC ABI, which uses function descriptors to represent
  pointers to functions.  Without any PIC/PIE-related options, it
  implies :option:`-fPIE`.  With :option:`-fpic` or :option:`-fpie`, it
  assumes GOT entries and small data are within a 12-bit range from the
  GOT base address; with :option:`-fPIC` or :option:`-fPIE`, GOT offsets
  are computed with 32 bits.
  With a :samp:`bfin-elf` target, this option implies :option:`-msim`.

.. option:: -minline-plt

  Enable inlining of PLT entries in function calls to functions that are
  not known to bind locally.  It has no effect without :option:`-mfdpic`.
  It's enabled by default if optimizing for speed and compiling for
  shared libraries (i.e., :option:`-fPIC` or :option:`-fpic`), or when an
  optimization option such as :option:`-O3` or above is present in the
  command line.

.. option:: -mTLS

  Assume a large TLS segment when generating thread-local code.

.. option:: -mtls

  Do not assume a large TLS segment when generating thread-local code.

.. option:: -mgprel-ro

  Enable the use of ``GPREL`` relocations in the FDPIC ABI for data
  that is known to be in read-only sections.  It's enabled by default,
  except for :option:`-fpic` or :option:`-fpie` : even though it may help
  make the global offset table smaller, it trades 1 instruction for 4.
  With :option:`-fPIC` or :option:`-fPIE`, it trades 3 instructions for 4,
  one of which may be shared by multiple symbols, and it avoids the need
  for a GOT entry for the referenced symbol, so it's more likely to be a
  win.  If it is not, :option:`-mno-gprel-ro` can be used to disable it.

.. option:: -multilib-library-pic

  Link with the (library, not FD) pic libraries.  It's implied by
  :option:`-mlibrary-pic`, as well as by :option:`-fPIC` and
  :option:`-fpic` without :option:`-mfdpic`.  You should never have to use
  it explicitly.

.. option:: -mlinked-fp

  Follow the EABI requirement of always creating a frame pointer whenever
  a stack frame is allocated.  This option is enabled by default and can
  be disabled with :option:`-mno-linked-fp`.

.. option:: -mlong-calls

  Use indirect addressing to call functions outside the current
  compilation unit.  This allows the functions to be placed anywhere
  within the 32-bit address space.

.. option:: -malign-labels

  Try to align labels to an 8-byte boundary by inserting NOPs into the
  previous packet.  This option only has an effect when VLIW packing
  is enabled.  It doesn't create new packets; it merely adds NOPs to
  existing ones.

.. option:: -mlibrary-pic

  Generate position-independent EABI code.

.. option:: -macc-4

  Use only the first four media accumulator registers.

.. option:: -macc-8

  Use all eight media accumulator registers.

.. option:: -mpack

  Pack VLIW instructions.

.. option:: -mno-pack

  Do not pack VLIW instructions.

.. option:: -mno-eflags

  Do not mark ABI switches in e_flags.

.. option:: -mcond-move

  Enable the use of conditional-move instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-cond-move

  Disable the use of conditional-move instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mscc

  Enable the use of conditional set instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-scc

  Disable the use of conditional set instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mcond-exec

  Enable the use of conditional execution (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-cond-exec

  Disable the use of conditional execution.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mvliw-branch

  Run a pass to pack branches into VLIW instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-vliw-branch

  Do not run a pass to pack branches into VLIW instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mmulti-cond-exec

  Enable optimization of ``&&`` and ``||`` in conditional execution
  (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-multi-cond-exec

  Disable optimization of ``&&`` and ``||`` in conditional execution.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mnested-cond-exec

  Enable nested conditional execution optimizations (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-nested-cond-exec

  Disable nested conditional execution optimizations.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -moptimize-membar

  This switch removes redundant ``membar`` instructions from the
  compiler-generated code.  It is enabled by default.

.. option:: -mno-optimize-membar

  This switch disables the automatic removal of redundant ``membar``
  instructions from the generated code.

.. option:: -moptimize-membar

  Default setting; overrides :option:`-mno-optimize-membar`.

.. option:: -mtomcat-stats

  Cause gas to print out tomcat statistics.

.. option:: -mcpu={cpu}

  Select the processor type for which to generate code.  Possible values are
  :samp:`frv`, :samp:`fr550`, :samp:`tomcat`, :samp:`fr500`, :samp:`fr450`,
  :samp:`fr405`, :samp:`fr400`, :samp:`fr300` and :samp:`simple`.