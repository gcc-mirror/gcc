..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: S/390 and zSeries

.. index:: S/390 and zSeries Options

.. _s-390-and-zseries-options:

S/390 and zSeries Options
^^^^^^^^^^^^^^^^^^^^^^^^^

These are the :samp:`-m` options defined for the S/390 and zSeries architecture.

.. option:: -mhard-float, -msoft-float

  Use (do not use) the hardware floating-point instructions and registers
  for floating-point operations.  When :option:`-msoft-float` is specified,
  functions in :samp:`libgcc.a` are used to perform floating-point
  operations.  When :option:`-mhard-float` is specified, the compiler
  generates IEEE floating-point instructions.  This is the default.

.. option:: -mhard-dfp, -mno-hard-dfp

  Use (do not use) the hardware decimal-floating-point instructions for
  decimal-floating-point operations.  When :option:`-mno-hard-dfp` is
  specified, functions in :samp:`libgcc.a` are used to perform
  decimal-floating-point operations.  When :option:`-mhard-dfp` is
  specified, the compiler generates decimal-floating-point hardware
  instructions.  This is the default for :option:`-march=z9-ec` or higher.

.. option:: -mlong-double-64, -mlong-double-128

  These switches control the size of ``long double`` type. A size
  of 64 bits makes the ``long double`` type equivalent to the ``double``
  type. This is the default.

.. option:: -mbackchain, -mno-backchain

  Store (do not store) the address of the caller's frame as backchain pointer
  into the callee's stack frame.
  A backchain may be needed to allow debugging using tools that do not understand
  DWARF call frame information.
  When :option:`-mno-packed-stack` is in effect, the backchain pointer is stored
  at the bottom of the stack frame; when :option:`-mpacked-stack` is in effect,
  the backchain is placed into the topmost word of the 96/160 byte register
  save area.

  In general, code compiled with :option:`-mbackchain` is call-compatible with
  code compiled with :option:`-mno-backchain` ; however, use of the backchain
  for debugging purposes usually requires that the whole binary is built with
  :option:`-mbackchain`.  Note that the combination of :option:`-mbackchain`,
  :option:`-mpacked-stack` and :option:`-mhard-float` is not supported.  In order
  to build a linux kernel use :option:`-msoft-float`.

  The default is to not maintain the backchain.

.. option:: -mpacked-stack, -mno-packed-stack

  Use (do not use) the packed stack layout.  When :option:`-mno-packed-stack` is
  specified, the compiler uses the all fields of the 96/160 byte register save
  area only for their default purpose; unused fields still take up stack space.
  When :option:`-mpacked-stack` is specified, register save slots are densely
  packed at the top of the register save area; unused space is reused for other
  purposes, allowing for more efficient use of the available stack space.
  However, when :option:`-mbackchain` is also in effect, the topmost word of
  the save area is always used to store the backchain, and the return address
  register is always saved two words below the backchain.

  As long as the stack frame backchain is not used, code generated with
  :option:`-mpacked-stack` is call-compatible with code generated with
  :option:`-mno-packed-stack`.  Note that some non-FSF releases of GCC 2.95 for
  S/390 or zSeries generated code that uses the stack frame backchain at run
  time, not just for debugging purposes.  Such code is not call-compatible
  with code compiled with :option:`-mpacked-stack`.  Also, note that the
  combination of :option:`-mbackchain`,
  :option:`-mpacked-stack` and :option:`-mhard-float` is not supported.  In order
  to build a linux kernel use :option:`-msoft-float`.

  The default is to not use the packed stack layout.

.. option:: -msmall-exec, -mno-small-exec

  Generate (or do not generate) code using the ``bras`` instruction
  to do subroutine calls.
  This only works reliably if the total executable size does not
  exceed 64k.  The default is to use the ``basr`` instruction instead,
  which does not have this limitation.

.. option:: -m64, -m31

  When :option:`-m31` is specified, generate code compliant to the
  GNU/Linux for S/390 ABI.  When :option:`-m64` is specified, generate
  code compliant to the GNU/Linux for zSeries ABI.  This allows GCC in
  particular to generate 64-bit instructions.  For the :samp:`s390`
  targets, the default is :option:`-m31`, while the :samp:`s390x`
  targets default to :option:`-m64`.

.. option:: -mzarch, -mesa

  When :option:`-mzarch` is specified, generate code using the
  instructions available on z/Architecture.
  When :option:`-mesa` is specified, generate code using the
  instructions available on ESA/390.  Note that :option:`-mesa` is
  not possible with :option:`-m64`.
  When generating code compliant to the GNU/Linux for S/390 ABI,
  the default is :option:`-mesa`.  When generating code compliant
  to the GNU/Linux for zSeries ABI, the default is :option:`-mzarch`.

.. option:: -mhtm, -mno-htm

  The :option:`-mhtm` option enables a set of builtins making use of
  instructions available with the transactional execution facility
  introduced with the IBM zEnterprise EC12 machine generation
  :ref:`s-390-system-z-built-in-functions`.
  :option:`-mhtm` is enabled by default when using :option:`-march=zEC12`.

.. option:: -mvx, -mno-vx

  When :option:`-mvx` is specified, generate code using the instructions
  available with the vector extension facility introduced with the IBM
  z13 machine generation.
  This option changes the ABI for some vector type values with regard to
  alignment and calling conventions.  In case vector type values are
  being used in an ABI-relevant context a GAS :samp:`.gnu_attribute`
  command will be added to mark the resulting binary with the ABI used.
  :option:`-mvx` is enabled by default when using :option:`-march=z13`.

.. option:: -mzvector, -mno-zvector

  The :option:`-mzvector` option enables vector language extensions and
  builtins using instructions available with the vector extension
  facility introduced with the IBM z13 machine generation.
  This option adds support for :samp:`vector` to be used as a keyword to
  define vector type variables and arguments.  :samp:`vector` is only
  available when GNU extensions are enabled.  It will not be expanded
  when requesting strict standard compliance e.g. with :option:`-std=c99`.
  In addition to the GCC low-level builtins :option:`-mzvector` enables
  a set of builtins added for compatibility with AltiVec-style
  implementations like Power and Cell.  In order to make use of these
  builtins the header file :samp:`vecintrin.h` needs to be included.
  :option:`-mzvector` is disabled by default.

.. option:: -mmvcle, -mno-mvcle

  Generate (or do not generate) code using the ``mvcle`` instruction
  to perform block moves.  When :option:`-mno-mvcle` is specified,
  use a ``mvc`` loop instead.  This is the default unless optimizing for
  size.

.. option:: -mdebug, -mno-debug

  Print (or do not print) additional debug information when compiling.
  The default is to not print debug information.

.. option:: -march={cpu-type}

  Generate code that runs on :samp:`{cpu-type}`, which is the name of a
  system representing a certain processor type.  Possible values for
  :samp:`{cpu-type}` are :samp:`z900`/:samp:`arch5`, :samp:`z990`/:samp:`arch6`,
  :samp:`z9-109`, :samp:`z9-ec`/:samp:`arch7`, :samp:`z10`/:samp:`arch8`,
  :samp:`z196`/:samp:`arch9`, :samp:`zEC12`, :samp:`z13`/:samp:`arch11`,
  :samp:`z14`/:samp:`arch12`, :samp:`z15`/:samp:`arch13`,
  :samp:`z16`/:samp:`arch14`, and :samp:`native`.

  The default is :option:`-march=z900`.

  Specifying :samp:`native` as cpu type can be used to select the best
  architecture option for the host processor.
  :option:`-march=native` has no effect if GCC does not recognize the
  processor.

.. option:: -mtune={cpu-type}

  Tune to :samp:`{cpu-type}` everything applicable about the generated code,
  except for the ABI and the set of available instructions.
  The list of :samp:`{cpu-type}` values is the same as for :option:`-march`.
  The default is the value used for :option:`-march`.

.. option:: -mtpf-trace, -mno-tpf-trace

  Generate code that adds (does not add) in TPF OS specific branches to trace
  routines in the operating system.  This option is off by default, even
  when compiling for the TPF OS.

.. option:: -mtpf-trace-skip, -mno-tpf-trace-skip

  Generate code that changes (does not change) the default branch
  targets enabled by :option:`-mtpf-trace` to point to specialized trace
  routines providing the ability of selectively skipping function trace
  entries for the TPF OS.  This option is off by default, even when
  compiling for the TPF OS and specifying :option:`-mtpf-trace`.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default if
  hardware floating point is used.

.. option:: -mwarn-framesize={framesize}

  Emit a warning if the current function exceeds the given frame size.  Because
  this is a compile-time check it doesn't need to be a real problem when the program
  runs.  It is intended to identify functions that most probably cause
  a stack overflow.  It is useful to be used in an environment with limited stack
  size e.g. the linux kernel.

.. option:: -mwarn-dynamicstack

  Emit a warning if the function calls ``alloca`` or uses dynamically-sized
  arrays.  This is generally a bad idea with a limited stack size.

.. option:: -mstack-guard={stack-guard}

  If these options are provided the S/390 back end emits additional instructions in
  the function prologue that trigger a trap if the stack size is :samp:`{stack-guard}`
  bytes above the :samp:`{stack-size}` (remember that the stack on S/390 grows downward).
  If the :samp:`{stack-guard}` option is omitted the smallest power of 2 larger than
  the frame size of the compiled function is chosen.
  These options are intended to be used to help debugging stack overflow problems.
  The additionally emitted code causes only little overhead and hence can also be
  used in production-like systems without greater performance degradation.  The given
  values have to be exact powers of 2 and :samp:`{stack-size}` has to be greater than
  :samp:`{stack-guard}` without exceeding 64k.
  In order to be efficient the extra code makes the assumption that the stack starts
  at an address aligned to the value given by :samp:`{stack-size}`.
  The :samp:`{stack-guard}` option can only be used in conjunction with :samp:`{stack-size}`.

.. option:: -mhotpatch={pre-halfwords},{post-halfwords}

  If the hotpatch option is enabled, a 'hot-patching' function
  prologue is generated for all functions in the compilation unit.
  The funtion label is prepended with the given number of two-byte
  NOP instructions (:samp:`{pre-halfwords}`, maximum 1000000).  After
  the label, 2 \* :samp:`{post-halfwords}` bytes are appended, using the
  largest NOP like instructions the architecture allows (maximum
  1000000).

  If both arguments are zero, hotpatching is disabled.

  This option can be overridden for individual functions with the
  ``hotpatch`` attribute.
