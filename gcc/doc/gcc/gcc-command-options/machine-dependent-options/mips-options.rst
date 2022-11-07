..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MIPS

.. index:: MIPS options

.. _mips-options:

MIPS Options
^^^^^^^^^^^^

.. option:: -EB

  Generate big-endian code.

.. option:: -EL

  Generate little-endian code.  This is the default for :samp:`mips*el-*-*`
  configurations.

.. option:: -march={arch}

  Generate code that runs on :samp:`{arch}`, which can be the name of a
  generic MIPS ISA, or the name of a particular processor.
  The ISA names are:
  :samp:`mips1`, :samp:`mips2`, :samp:`mips3`, :samp:`mips4`,
  :samp:`mips32`, :samp:`mips32r2`, :samp:`mips32r3`, :samp:`mips32r5`,
  :samp:`mips32r6`, :samp:`mips64`, :samp:`mips64r2`, :samp:`mips64r3`,
  :samp:`mips64r5` and :samp:`mips64r6`.
  The processor names are:
  :samp:`4kc`, :samp:`4km`, :samp:`4kp`, :samp:`4ksc`,
  :samp:`4kec`, :samp:`4kem`, :samp:`4kep`, :samp:`4ksd`,
  :samp:`5kc`, :samp:`5kf`,
  :samp:`20kc`,
  :samp:`24kc`, :samp:`24kf2_1`, :samp:`24kf1_1`,
  :samp:`24kec`, :samp:`24kef2_1`, :samp:`24kef1_1`,
  :samp:`34kc`, :samp:`34kf2_1`, :samp:`34kf1_1`, :samp:`34kn`,
  :samp:`74kc`, :samp:`74kf2_1`, :samp:`74kf1_1`, :samp:`74kf3_2`,
  :samp:`1004kc`, :samp:`1004kf2_1`, :samp:`1004kf1_1`,
  :samp:`i6400`, :samp:`i6500`,
  :samp:`interaptiv`,
  :samp:`loongson2e`, :samp:`loongson2f`, :samp:`loongson3a`, :samp:`gs464`,
  :samp:`gs464e`, :samp:`gs264e`,
  :samp:`m4k`,
  :samp:`m14k`, :samp:`m14kc`, :samp:`m14ke`, :samp:`m14kec`,
  :samp:`m5100`, :samp:`m5101`,
  :samp:`octeon`, :samp:`octeon+`, :samp:`octeon2`, :samp:`octeon3`,
  :samp:`orion`,
  :samp:`p5600`, :samp:`p6600`,
  :samp:`r2000`, :samp:`r3000`, :samp:`r3900`, :samp:`r4000`, :samp:`r4400`,
  :samp:`r4600`, :samp:`r4650`, :samp:`r4700`, :samp:`r5900`,
  :samp:`r6000`, :samp:`r8000`,
  :samp:`rm7000`, :samp:`rm9000`,
  :samp:`r10000`, :samp:`r12000`, :samp:`r14000`, :samp:`r16000`,
  :samp:`sb1`,
  :samp:`sr71000`,
  :samp:`vr4100`, :samp:`vr4111`, :samp:`vr4120`, :samp:`vr4130`, :samp:`vr4300`,
  :samp:`vr5000`, :samp:`vr5400`, :samp:`vr5500`,
  :samp:`xlr` and :samp:`xlp`.
  The special value :samp:`from-abi` selects the
  most compatible architecture for the selected ABI (that is,
  :samp:`mips1` for 32-bit ABIs and :samp:`mips3` for 64-bit ABIs).

  The native Linux/GNU toolchain also supports the value :samp:`native`,
  which selects the best architecture option for the host processor.
  :option:`-march=native` has no effect if GCC does not recognize
  the processor.

  In processor names, a final :samp:`000` can be abbreviated as :samp:`k`
  (for example, :option:`-march=r2k`).  Prefixes are optional, and
  :samp:`vr` may be written :samp:`r`.

  Names of the form :samp:`{n}f2_1` refer to processors with
  FPUs clocked at half the rate of the core, names of the form
  :samp:`{n}f1_1` refer to processors with FPUs clocked at the same
  rate as the core, and names of the form :samp:`{n}f3_2` refer to
  processors with FPUs clocked a ratio of 3:2 with respect to the core.
  For compatibility reasons, :samp:`{n}f` is accepted as a synonym
  for :samp:`{n}f2_1` while :samp:`{n}x` and :samp:`{b}fx` are
  accepted as synonyms for :samp:`{n}f1_1`.

  GCC defines two macros based on the value of this option.  The first
  is ``_MIPS_ARCH``, which gives the name of target architecture, as
  a string.  The second has the form ``_MIPS_ARCH_foo``,
  where :samp:`{foo}` is the capitalized value of ``_MIPS_ARCH``.
  For example, :option:`-march=r2000` sets ``_MIPS_ARCH``
  to ``"r2000"`` and defines the macro ``_MIPS_ARCH_R2000``.

  Note that the ``_MIPS_ARCH`` macro uses the processor names given
  above.  In other words, it has the full prefix and does not
  abbreviate :samp:`000` as :samp:`k`.  In the case of :samp:`from-abi`,
  the macro names the resolved architecture (either ``"mips1"`` or
  ``"mips3"``).  It names the default architecture when no
  :option:`-march` option is given.

.. option:: -mtune={arch}

  Optimize for :samp:`{arch}`.  Among other things, this option controls
  the way instructions are scheduled, and the perceived cost of arithmetic
  operations.  The list of :samp:`{arch}` values is the same as for
  :option:`-march`.

  When this option is not used, GCC optimizes for the processor
  specified by :option:`-march`.  By using :option:`-march` and
  :option:`-mtune` together, it is possible to generate code that
  runs on a family of processors, but optimize the code for one
  particular member of that family.

  :option:`-mtune` defines the macros ``_MIPS_TUNE`` and
  ``_MIPS_TUNE_foo``, which work in the same way as the
  :option:`-march` ones described above.

.. option:: -mips1

  Equivalent to :option:`-march=mips1`.

.. option:: -mips2

  Equivalent to :option:`-march=mips2`.

.. option:: -mips3

  Equivalent to :option:`-march=mips3`.

.. option:: -mips4

  Equivalent to :option:`-march=mips4`.

.. option:: -mips32

  Equivalent to :option:`-march=mips32`.

.. option:: -mips32r3

  Equivalent to :option:`-march=mips32r3`.

.. option:: -mips32r5

  Equivalent to :option:`-march=mips32r5`.

.. option:: -mips32r6

  Equivalent to :option:`-march=mips32r6`.

.. option:: -mips64

  Equivalent to :option:`-march=mips64`.

.. option:: -mips64r2

  Equivalent to :option:`-march=mips64r2`.

.. option:: -mips64r3

  Equivalent to :option:`-march=mips64r3`.

.. option:: -mips64r5

  Equivalent to :option:`-march=mips64r5`.

.. option:: -mips64r6

  Equivalent to :option:`-march=mips64r6`.

.. option:: -mips16, -mno-mips16

  Generate (do not generate) MIPS16 code.  If GCC is targeting a
  MIPS32 or MIPS64 architecture, it makes use of the MIPS16e ASE.

  MIPS16 code generation can also be controlled on a per-function basis
  by means of :mips-fn-attr:`mips16` and ``nomips16`` attributes.
  See :ref:`function-attributes`, for more information.

.. option:: -mflip-mips16

  Generate MIPS16 code on alternating functions.  This option is provided
  for regression testing of mixed MIPS16/non-MIPS16 code generation, and is
  not intended for ordinary use in compiling user code.

.. option:: -minterlink-compressed, -mno-interlink-compressed

  Require (do not require) that code using the standard (uncompressed) MIPS ISA
  be link-compatible with MIPS16 and microMIPS code, and vice versa.

  For example, code using the standard ISA encoding cannot jump directly
  to MIPS16 or microMIPS code; it must either use a call or an indirect jump.
  :option:`-minterlink-compressed` therefore disables direct jumps unless GCC
  knows that the target of the jump is not compressed.

.. option:: -minterlink-mips16, -mno-interlink-mips16

  Aliases of :option:`-minterlink-compressed` and
  :option:`-mno-interlink-compressed`.  These options predate the microMIPS ASE
  and are retained for backwards compatibility.

.. option:: -mabi=32

  Generate code for the given ABI.

  Note that the EABI has a 32-bit and a 64-bit variant.  GCC normally
  generates 64-bit code when you select a 64-bit architecture, but you
  can use :option:`-mgp32` to get 32-bit code instead.

  For information about the O64 ABI, see
  https://gcc.gnu.org/projects/mipso64-abi.html.

  GCC supports a variant of the o32 ABI in which floating-point registers
  are 64 rather than 32 bits wide.  You can select this combination with
  :option:`-mabi=32` :option:`-mfp64`.  This ABI relies on the ``mthc1``
  and ``mfhc1`` instructions and is therefore only supported for
  MIPS32R2, MIPS32R3 and MIPS32R5 processors.

  The register assignments for arguments and return values remain the
  same, but each scalar value is passed in a single 64-bit register
  rather than a pair of 32-bit registers.  For example, scalar
  floating-point values are returned in :samp:`$f0` only, not a
  :samp:`$f0`/:samp:`$f1` pair.  The set of call-saved registers also
  remains the same in that the even-numbered double-precision registers
  are saved.

  Two additional variants of the o32 ABI are supported to enable
  a transition from 32-bit to 64-bit registers.  These are FPXX
  (:option:`-mfpxx`) and FP64A (:option:`-mfp64` :option:`-mno-odd-spreg`).
  The FPXX extension mandates that all code must execute correctly
  when run using 32-bit or 64-bit registers.  The code can be interlinked
  with either FP32 or FP64, but not both.
  The FP64A extension is similar to the FP64 extension but forbids the
  use of odd-numbered single-precision registers.  This can be used
  in conjunction with the ``FRE`` mode of FPUs in MIPS32R5
  processors and allows both FP32 and FP64A code to interlink and
  run in the same process without changing FPU modes.

.. option:: -mabicalls, -mno-abicalls

  Generate (do not generate) code that is suitable for SVR4-style
  dynamic objects.  :option:`-mabicalls` is the default for SVR4-based
  systems.

.. option:: -mshared, -mno-shared

  Generate (do not generate) code that is fully position-independent,
  and that can therefore be linked into shared libraries.  This option
  only affects :option:`-mabicalls`.

  All :option:`-mabicalls` code has traditionally been position-independent,
  regardless of options like :option:`-fPIC` and :option:`-fpic`.  However,
  as an extension, the GNU toolchain allows executables to use absolute
  accesses for locally-binding symbols.  It can also use shorter GP
  initialization sequences and generate direct calls to locally-defined
  functions.  This mode is selected by :option:`-mno-shared`.

  :option:`-mno-shared` depends on binutils 2.16 or higher and generates
  objects that can only be linked by the GNU linker.  However, the option
  does not affect the ABI of the final executable; it only affects the ABI
  of relocatable objects.  Using :option:`-mno-shared` generally makes
  executables both smaller and quicker.

  :option:`-mshared` is the default.

.. option:: -mplt, -mno-plt

  Assume (do not assume) that the static and dynamic linkers
  support PLTs and copy relocations.  This option only affects
  :option:`-mno-shared -mabicalls`.  For the n64 ABI, this option
  has no effect without :option:`-msym32`.

  You can make :option:`-mplt` the default by configuring
  GCC with :option:`--with-mips-plt`.  The default is
  :option:`-mno-plt` otherwise.

.. option:: -mxgot, -mno-xgot

  Lift (do not lift) the usual restrictions on the size of the global
  offset table.

  GCC normally uses a single instruction to load values from the GOT.
  While this is relatively efficient, it only works if the GOT
  is smaller than about 64k.  Anything larger causes the linker
  to report an error such as:

  .. index:: relocation truncated to fit (MIPS)

  .. code-block:: c++

    relocation truncated to fit: R_MIPS_GOT16 foobar

  If this happens, you should recompile your code with :option:`-mxgot`.
  This works with very large GOTs, although the code is also
  less efficient, since it takes three instructions to fetch the
  value of a global symbol.

  Note that some linkers can create multiple GOTs.  If you have such a
  linker, you should only need to use :option:`-mxgot` when a single object
  file accesses more than 64k's worth of GOT entries.  Very few do.

  These options have no effect unless GCC is generating position
  independent code.

.. option:: -mgp32

  Assume that general-purpose registers are 32 bits wide.

.. option:: -mgp64

  Assume that general-purpose registers are 64 bits wide.

.. option:: -mfp32

  Assume that floating-point registers are 32 bits wide.

.. option:: -mfp64

  Assume that floating-point registers are 64 bits wide.

.. option:: -mfpxx

  Do not assume the width of floating-point registers.

.. option:: -mhard-float

  Use floating-point coprocessor instructions.

.. option:: -msoft-float

  Do not use floating-point coprocessor instructions.  Implement
  floating-point calculations using library calls instead.

.. option:: -mno-float

  Equivalent to :option:`-msoft-float`, but additionally asserts that the
  program being compiled does not perform any floating-point operations.
  This option is presently supported only by some bare-metal MIPS
  configurations, where it may select a special set of libraries
  that lack all floating-point support (including, for example, the
  floating-point ``printf`` formats).
  If code compiled with :option:`-mno-float` accidentally contains
  floating-point operations, it is likely to suffer a link-time
  or run-time failure.

.. option:: -msingle-float

  Assume that the floating-point coprocessor only supports single-precision
  operations.

.. option:: -mdouble-float

  Assume that the floating-point coprocessor supports double-precision
  operations.  This is the default.

.. option:: -modd-spreg, -mno-odd-spreg

  Enable the use of odd-numbered single-precision floating-point registers
  for the o32 ABI.  This is the default for processors that are known to
  support these registers.  When using the o32 FPXX ABI, :option:`-mno-odd-spreg`
  is set by default.

.. option:: -mabs=2008

  These options control the treatment of the special not-a-number (NaN)
  IEEE 754 floating-point data with the ``abs.fmt`` and
  ``neg.fmt`` machine instructions.

  By default or when :option:`-mabs=legacy` is used the legacy
  treatment is selected.  In this case these instructions are considered
  arithmetic and avoided where correct operation is required and the
  input operand might be a NaN.  A longer sequence of instructions that
  manipulate the sign bit of floating-point datum manually is used
  instead unless the :option:`-ffinite-math-only` option has also been
  specified.

  The :option:`-mabs=2008` option selects the IEEE 754-2008 treatment.  In
  this case these instructions are considered non-arithmetic and therefore
  operating correctly in all cases, including in particular where the
  input operand is a NaN.  These instructions are therefore always used
  for the respective operations.

.. option:: -mnan=2008

  These options control the encoding of the special not-a-number (NaN)
  IEEE 754 floating-point data.

  The :option:`-mnan=legacy` option selects the legacy encoding.  In this
  case quiet NaNs (qNaNs) are denoted by the first bit of their trailing
  significand field being 0, whereas signaling NaNs (sNaNs) are denoted
  by the first bit of their trailing significand field being 1.

  The :option:`-mnan=2008` option selects the IEEE 754-2008 encoding.  In
  this case qNaNs are denoted by the first bit of their trailing
  significand field being 1, whereas sNaNs are denoted by the first bit of
  their trailing significand field being 0.

  The default is :option:`-mnan=legacy` unless GCC has been configured with
  :option:`--with-nan=2008`.

.. option:: -mllsc, -mno-llsc

  Use (do not use) :samp:`ll`, :samp:`sc`, and :samp:`sync` instructions to
  implement atomic memory built-in functions.  When neither option is
  specified, GCC uses the instructions if the target architecture
  supports them.

  :option:`-mllsc` is useful if the runtime environment can emulate the
  instructions and :option:`-mno-llsc` can be useful when compiling for
  nonstandard ISAs.  You can make either option the default by
  configuring GCC with :option:`--with-llsc` and :option:`--without-llsc`
  respectively.  :option:`--with-llsc` is the default for some
  configurations; see the installation documentation for details.

.. option:: -mdsp, -mno-dsp

  Use (do not use) revision 1 of the MIPS DSP ASE.
  See :ref:`mips-dsp-built-in-functions`.  This option defines the
  preprocessor macro ``__mips_dsp``.  It also defines
  ``__mips_dsp_rev`` to 1.

.. option:: -mdspr2, -mno-dspr2

  Use (do not use) revision 2 of the MIPS DSP ASE.
  See :ref:`mips-dsp-built-in-functions`.  This option defines the
  preprocessor macros ``__mips_dsp`` and ``__mips_dspr2``.
  It also defines ``__mips_dsp_rev`` to 2.

.. option:: -msmartmips, -mno-smartmips

  Use (do not use) the MIPS SmartMIPS ASE.

.. option:: -mpaired-single, -mno-paired-single

  Use (do not use) paired-single floating-point instructions.
  See :ref:`mips-paired-single-support`.  This option requires
  hardware floating-point support to be enabled.

.. option:: -mdmx, -mno-mdmx

  Use (do not use) MIPS Digital Media Extension instructions.
  This option can only be used when generating 64-bit code and requires
  hardware floating-point support to be enabled.

.. option:: -mips3d, -mno-mips3d

  Use (do not use) the MIPS-3D ASE.  See :ref:`mips-3d-built-in-functions`.
  The option :option:`-mips3d` implies :option:`-mpaired-single`.

.. option:: -mmicromips, -mno-micromips

  Generate (do not generate) microMIPS code.

  MicroMIPS code generation can also be controlled on a per-function basis
  by means of ``micromips`` and ``nomicromips`` attributes.
  See :ref:`function-attributes`, for more information.

.. option:: -mmt, -mno-mt

  Use (do not use) MT Multithreading instructions.

.. option:: -mmcu, -mno-mcu

  Use (do not use) the MIPS MCU ASE instructions.

.. option:: -meva, -mno-eva

  Use (do not use) the MIPS Enhanced Virtual Addressing instructions.

.. option:: -mvirt, -mno-virt

  Use (do not use) the MIPS Virtualization (VZ) instructions.

.. option:: -mxpa, -mno-xpa

  Use (do not use) the MIPS eXtended Physical Address (XPA) instructions.

.. option:: -mcrc, -mno-crc

  Use (do not use) the MIPS Cyclic Redundancy Check (CRC) instructions.

.. option:: -mginv, -mno-ginv

  Use (do not use) the MIPS Global INValidate (GINV) instructions.

.. option:: -mloongson-mmi, -mno-loongson-mmi

  Use (do not use) the MIPS Loongson MultiMedia extensions Instructions (MMI).

.. option:: -mloongson-ext, -mno-loongson-ext

  Use (do not use) the MIPS Loongson EXTensions (EXT) instructions.

.. option:: -mloongson-ext2, -mno-loongson-ext2

  Use (do not use) the MIPS Loongson EXTensions r2 (EXT2) instructions.

.. option:: -mlong64

  Force ``long`` types to be 64 bits wide.  See :option:`-mlong32` for
  an explanation of the default and the way that the pointer size is
  determined.

.. option:: -mlong32

  Force ``long``, ``int``, and pointer types to be 32 bits wide.

  The default size of ``int`` s, ``long`` s and pointers depends on
  the ABI.  All the supported ABIs use 32-bit ``int`` s.  The n64 ABI
  uses 64-bit ``long`` s, as does the 64-bit EABI; the others use
  32-bit ``long`` s.  Pointers are the same size as ``long`` s,
  or the same size as integer registers, whichever is smaller.

.. option:: -msym32, -mno-sym32

  Assume (do not assume) that all symbols have 32-bit values, regardless
  of the selected ABI.  This option is useful in combination with
  :option:`-mabi=64` and :option:`-mno-abicalls` because it allows GCC
  to generate shorter and faster references to symbolic addresses.

.. option:: -G {num}

  Put definitions of externally-visible data in a small data section
  if that data is no bigger than :samp:`{num}` bytes.  GCC can then generate
  more efficient accesses to the data; see :option:`-mgpopt` for details.

  The default :option:`-G` option depends on the configuration.

.. option:: -mlocal-sdata, -mno-local-sdata

  Extend (do not extend) the :option:`-G` behavior to local data too,
  such as to static variables in C.  :option:`-mlocal-sdata` is the
  default for all configurations.

  If the linker complains that an application is using too much small data,
  you might want to try rebuilding the less performance-critical parts with
  :option:`-mno-local-sdata`.  You might also want to build large
  libraries with :option:`-mno-local-sdata`, so that the libraries leave
  more room for the main program.

.. option:: -mextern-sdata, -mno-extern-sdata

  Assume (do not assume) that externally-defined data is in
  a small data section if the size of that data is within the :option:`-G` limit.
  :option:`-mextern-sdata` is the default for all configurations.

  If you compile a module :samp:`{Mod}` with :option:`-mextern-sdata` :option:`-G  num`
  :option:`-mgpopt`, and :samp:`{Mod}` references a variable :samp:`{Var}`
  that is no bigger than :samp:`{num}` bytes, you must make sure that :samp:`{Var}`
  is placed in a small data section.  If :samp:`{Var}` is defined by another
  module, you must either compile that module with a high-enough
  :option:`-G` setting or attach a ``section`` attribute to :samp:`{Var}` 's
  definition.  If :samp:`{Var}` is common, you must link the application
  with a high-enough :option:`-G` setting.

  The easiest way of satisfying these restrictions is to compile
  and link every module with the same :option:`-G` option.  However,
  you may wish to build a library that supports several different
  small data limits.  You can do this by compiling the library with
  the highest supported :option:`-G` setting and additionally using
  :option:`-mno-extern-sdata` to stop the library from making assumptions
  about externally-defined data.

.. option:: -mgpopt, -mno-gpopt

  Use (do not use) GP-relative accesses for symbols that are known to be
  in a small data section; see :option:`-G`, :option:`-mlocal-sdata` and
  :option:`-mextern-sdata`.  :option:`-mgpopt` is the default for all
  configurations.

  :option:`-mno-gpopt` is useful for cases where the ``$gp`` register
  might not hold the value of ``_gp``.  For example, if the code is
  part of a library that might be used in a boot monitor, programs that
  call boot monitor routines pass an unknown value in ``$gp``.
  (In such situations, the boot monitor itself is usually compiled
  with :option:`-G0`.)

  :option:`-mno-gpopt` implies :option:`-mno-local-sdata` and
  :option:`-mno-extern-sdata`.

.. option:: -membedded-data, -mno-embedded-data

  Allocate variables to the read-only data section first if possible, then
  next in the small data section if possible, otherwise in data.  This gives
  slightly slower code than the default, but reduces the amount of RAM required
  when executing, and thus may be preferred for some embedded systems.

.. option:: -muninit-const-in-rodata, -mno-uninit-const-in-rodata

  Put uninitialized ``const`` variables in the read-only data section.
  This option is only meaningful in conjunction with :option:`-membedded-data`.

.. option:: -mcode-readable={setting}

  Specify whether GCC may generate code that reads from executable sections.
  There are three possible settings:

  ``-mcode-readable=yes``
    Instructions may freely access executable sections.  This is the
    default setting.

  ``-mcode-readable=pcrel``
    MIPS16 PC-relative load instructions can access executable sections,
    but other instructions must not do so.  This option is useful on 4KSc
    and 4KSd processors when the code TLBs have the Read Inhibit bit set.
    It is also useful on processors that can be configured to have a dual
    instruction/data SRAM interface and that, like the M4K, automatically
    redirect PC-relative loads to the instruction RAM.

  ``-mcode-readable=no``
    Instructions must not access executable sections.  This option can be
    useful on targets that are configured to have a dual instruction/data
    SRAM interface but that (unlike the M4K) do not automatically redirect
    PC-relative loads to the instruction RAM.

.. option:: -msplit-addresses, -mno-split-addresses

  Enable (disable) use of the ``%hi()`` and ``%lo()`` assembler
  relocation operators.  This option has been superseded by
  :option:`-mexplicit-relocs` but is retained for backwards compatibility.

.. option:: -mexplicit-relocs, -mno-explicit-relocs

  Use (do not use) assembler relocation operators when dealing with symbolic
  addresses.  The alternative, selected by :option:`-mno-explicit-relocs`,
  is to use assembler macros instead.

  :option:`-mexplicit-relocs` is the default if GCC was configured
  to use an assembler that supports relocation operators.

.. option:: -mcheck-zero-division, -mno-check-zero-division

  Trap (do not trap) on integer division by zero.

  The default is :option:`-mcheck-zero-division`.

.. option:: -mdivide-traps, -mdivide-breaks

  MIPS systems check for division by zero by generating either a
  conditional trap or a break instruction.  Using traps results in
  smaller code, but is only supported on MIPS II and later.  Also, some
  versions of the Linux kernel have a bug that prevents trap from
  generating the proper signal (``SIGFPE``).  Use :option:`-mdivide-traps` to
  allow conditional traps on architectures that support them and
  :option:`-mdivide-breaks` to force the use of breaks.

  The default is usually :option:`-mdivide-traps`, but this can be
  overridden at configure time using :option:`--with-divide=breaks`.
  Divide-by-zero checks can be completely disabled using
  :option:`-mno-check-zero-division`.

.. option:: -mload-store-pairs, -mno-load-store-pairs

  Enable (disable) an optimization that pairs consecutive load or store
  instructions to enable load/store bonding.  This option is enabled by
  default but only takes effect when the selected architecture is known
  to support bonding.

.. option:: -munaligned-access, -mno-unaligned-access

  Enable (disable) direct unaligned access for MIPS Release 6.
  MIPSr6 requires load/store unaligned-access support,
  by hardware or trap&emulate.
  So :option:`-mno-unaligned-access` may be needed by kernel.

.. option:: -mmemcpy, -mno-memcpy

  Force (do not force) the use of ``memcpy`` for non-trivial block
  moves.  The default is :option:`-mno-memcpy`, which allows GCC to inline
  most constant-sized copies.

.. option:: -mlong-calls, -mno-long-calls

  Disable (do not disable) use of the ``jal`` instruction.  Calling
  functions using ``jal`` is more efficient but requires the caller
  and callee to be in the same 256 megabyte segment.

  This option has no effect on abicalls code.  The default is
  :option:`-mno-long-calls`.

.. option:: -mmad, -mno-mad

  Enable (disable) use of the ``mad``, ``madu`` and ``mul``
  instructions, as provided by the R4650 ISA.

.. option:: -mimadd, -mno-imadd

  Enable (disable) use of the ``madd`` and ``msub`` integer
  instructions.  The default is :option:`-mimadd` on architectures
  that support ``madd`` and ``msub`` except for the 74k
  architecture where it was found to generate slower code.

.. option:: -mfused-madd, -mno-fused-madd

  Enable (disable) use of the floating-point multiply-accumulate
  instructions, when they are available.  The default is
  :option:`-mfused-madd`.

  On the R8000 CPU when multiply-accumulate instructions are used,
  the intermediate product is calculated to infinite precision
  and is not subject to the FCSR Flush to Zero bit.  This may be
  undesirable in some circumstances.  On other processors the result
  is numerically identical to the equivalent computation using
  separate multiply, add, subtract and negate instructions.

.. option:: -nocpp

  Tell the MIPS assembler to not run its preprocessor over user
  assembler files (with a :samp:`.s` suffix) when assembling them.

.. option:: -mfix-24k, -mno-fix-24k

  Work around the 24K E48 (lost data on stores during refill) errata.
  The workarounds are implemented by the assembler rather than by GCC.

.. option:: -mfix-r4000, -mno-fix-r4000

  Work around certain R4000 CPU errata:

  * A double-word or a variable shift may give an incorrect result if executed
    immediately after starting an integer division.

  * A double-word or a variable shift may give an incorrect result if executed
    while an integer multiplication is in progress.

  * An integer division may give an incorrect result if started in a delay slot
    of a taken branch or a jump.

.. option:: -mfix-r4400, -mno-fix-r4400

  Work around certain R4400 CPU errata:

  * A double-word or a variable shift may give an incorrect result if executed
    immediately after starting an integer division.

.. option:: -mfix-r10000, -mno-fix-r10000

  Work around certain R10000 errata:

  * ``ll`` / ``sc`` sequences may not behave atomically on revisions
    prior to 3.0.  They may deadlock on revisions 2.6 and earlier.

  This option can only be used if the target architecture supports
  branch-likely instructions.  :option:`-mfix-r10000` is the default when
  :option:`-march=r10000` is used; :option:`-mno-fix-r10000` is the default
  otherwise.

.. option:: -mfix-r5900, -mno-fix-r5900

  Do not attempt to schedule the preceding instruction into the delay slot
  of a branch instruction placed at the end of a short loop of six
  instructions or fewer and always schedule a ``nop`` instruction there
  instead.  The short loop bug under certain conditions causes loops to
  execute only once or twice, due to a hardware bug in the R5900 chip.  The
  workaround is implemented by the assembler rather than by GCC.

.. option:: -mfix-rm7000, -mno-fix-rm7000

  Work around the RM7000 ``dmult`` / ``dmultu`` errata.  The
  workarounds are implemented by the assembler rather than by GCC.

.. option:: -mfix-vr4120, -mno-fix-vr4120

  Work around certain VR4120 errata:

  * ``dmultu`` does not always produce the correct result.

  * ``div`` and ``ddiv`` do not always produce the correct result if one
    of the operands is negative.

  The workarounds for the division errata rely on special functions in
  :samp:`libgcc.a`.  At present, these functions are only provided by
  the ``mips64vr*-elf`` configurations.

  Other VR4120 errata require a NOP to be inserted between certain pairs of
  instructions.  These errata are handled by the assembler, not by GCC itself.

.. option:: -mfix-vr4130

  Work around the VR4130 ``mflo`` / ``mfhi`` errata.  The
  workarounds are implemented by the assembler rather than by GCC,
  although GCC avoids using ``mflo`` and ``mfhi`` if the
  VR4130 ``macc``, ``macchi``, ``dmacc`` and ``dmacchi``
  instructions are available instead.

.. option:: -mfix-sb1, -mno-fix-sb1

  Work around certain SB-1 CPU core errata.
  (This flag currently works around the SB-1 revision 2
  'F1' and 'F2' floating-point errata.)

.. option:: -mr10k-cache-barrier={setting}

  Specify whether GCC should insert cache barriers to avoid the
  side effects of speculation on R10K processors.

  In common with many processors, the R10K tries to predict the outcome
  of a conditional branch and speculatively executes instructions from
  the 'taken' branch.  It later aborts these instructions if the
  predicted outcome is wrong.  However, on the R10K, even aborted
  instructions can have side effects.

  This problem only affects kernel stores and, depending on the system,
  kernel loads.  As an example, a speculatively-executed store may load
  the target memory into cache and mark the cache line as dirty, even if
  the store itself is later aborted.  If a DMA operation writes to the
  same area of memory before the 'dirty' line is flushed, the cached
  data overwrites the DMA-ed data.  See the R10K processor manual
  for a full description, including other potential problems.

  One workaround is to insert cache barrier instructions before every memory
  access that might be speculatively executed and that might have side
  effects even if aborted.  :option:`-mr10k-cache-barrier=setting`
  controls GCC's implementation of this workaround.  It assumes that
  aborted accesses to any byte in the following regions does not have
  side effects:

  * the memory occupied by the current function's stack frame;

  * the memory occupied by an incoming stack argument;

  * the memory occupied by an object with a link-time-constant address.

  It is the kernel's responsibility to ensure that speculative
  accesses to these regions are indeed safe.

  If the input program contains a function declaration such as:

  .. code-block:: c++

    void foo (void);

  then the implementation of ``foo`` must allow ``j foo`` and
  ``jal foo`` to be executed speculatively.  GCC honors this
  restriction for functions it compiles itself.  It expects non-GCC
  functions (such as hand-written assembly code) to do the same.

  The option has three forms:

  ``-mr10k-cache-barrier=load-store``
    Insert a cache barrier before a load or store that might be
    speculatively executed and that might have side effects even
    if aborted.

  ``-mr10k-cache-barrier=store``
    Insert a cache barrier before a store that might be speculatively
    executed and that might have side effects even if aborted.

  ``-mr10k-cache-barrier=none``
    Disable the insertion of cache barriers.  This is the default setting.

.. option:: -mflush-func={func}

  Specifies the function to call to flush the I and D caches, or to not
  call any such function.  If called, the function must take the same
  arguments as the common ``_flush_func``, that is, the address of the
  memory range for which the cache is being flushed, the size of the
  memory range, and the number 3 (to flush both caches).  The default
  depends on the target GCC was configured for, but commonly is either
  ``_flush_func`` or ``__cpu_flush``.

.. option:: -mflush-func

  Default setting; overrides :option:`-mno-flush-func`.

.. option:: mbranch-cost=num

  Set the cost of branches to roughly :samp:`{num}` 'simple' instructions.
  This cost is only a heuristic and is not guaranteed to produce
  consistent results across releases.  A zero cost redundantly selects
  the default, which is based on the :option:`-mtune` setting.

.. option:: -mbranch-likely, -mno-branch-likely

  Enable or disable use of Branch Likely instructions, regardless of the
  default for the selected architecture.  By default, Branch Likely
  instructions may be generated if they are supported by the selected
  architecture.  An exception is for the MIPS32 and MIPS64 architectures
  and processors that implement those architectures; for those, Branch
  Likely instructions are not be generated by default because the MIPS32
  and MIPS64 architectures specifically deprecate their use.

.. option:: -mcompact-branches=never

  These options control which form of branches will be generated.  The
  default is :option:`-mcompact-branches=optimal`.

  The :option:`-mcompact-branches=never` option ensures that compact branch
  instructions will never be generated.

  The :option:`-mcompact-branches=always` option ensures that a compact
  branch instruction will be generated if available for MIPS Release 6 onwards.
  If a compact branch instruction is not available (or pre-R6),
  a delay slot form of the branch will be used instead.

  If it is used for MIPS16/microMIPS targets, it will be just ignored now.
  The behaviour for MIPS16/microMIPS may change in future,
  since they do have some compact branch instructions.

  The :option:`-mcompact-branches=optimal` option will cause a delay slot
  branch to be used if one is available in the current ISA and the delay
  slot is successfully filled.  If the delay slot is not filled, a compact
  branch will be chosen if one is available.

.. option:: -mfp-exceptions, -mno-fp-exceptions

  Specifies whether FP exceptions are enabled.  This affects how
  FP instructions are scheduled for some processors.
  The default is that FP exceptions are
  enabled.

  For instance, on the SB-1, if FP exceptions are disabled, and we are emitting
  64-bit code, then we can use both FP pipes.  Otherwise, we can only use one
  FP pipe.

.. option:: -mvr4130-align, -mno-vr4130-align

  The VR4130 pipeline is two-way superscalar, but can only issue two
  instructions together if the first one is 8-byte aligned.  When this
  option is enabled, GCC aligns pairs of instructions that it
  thinks should execute in parallel.

  This option only has an effect when optimizing for the VR4130.
  It normally makes code faster, but at the expense of making it bigger.
  It is enabled by default at optimization level :option:`-O3`.

.. option:: -msynci, -mno-synci

  Enable (disable) generation of ``synci`` instructions on
  architectures that support it.  The ``synci`` instructions (if
  enabled) are generated when ``__builtin___clear_cache`` is
  compiled.

  This option defaults to :option:`-mno-synci`, but the default can be
  overridden by configuring GCC with :option:`--with-synci`.

  When compiling code for single processor systems, it is generally safe
  to use ``synci``.  However, on many multi-core (SMP) systems, it
  does not invalidate the instruction caches on all cores and may lead
  to undefined behavior.

.. option:: -mrelax-pic-calls, -mno-relax-pic-calls

  Try to turn PIC calls that are normally dispatched via register
  ``$25`` into direct calls.  This is only possible if the linker can
  resolve the destination at link time and if the destination is within
  range for a direct call.

  :option:`-mrelax-pic-calls` is the default if GCC was configured to use
  an assembler and a linker that support the ``.reloc`` assembly
  directive and :option:`-mexplicit-relocs` is in effect.  With
  :option:`-mno-explicit-relocs`, this optimization can be performed by the
  assembler and the linker alone without help from the compiler.

.. option:: -mmcount-ra-address, -mno-mcount-ra-address

  Emit (do not emit) code that allows ``_mcount`` to modify the
  calling function's return address.  When enabled, this option extends
  the usual ``_mcount`` interface with a new :samp:`{ra-address}`
  parameter, which has type ``intptr_t *`` and is passed in register
  ``$12``.  ``_mcount`` can then modify the return address by
  doing both of the following:

  * Returning the new address in register ``$31``.

  * Storing the new address in ``*ra-address``,
    if :samp:`{ra-address}` is nonnull.

  The default is :option:`-mno-mcount-ra-address`.

.. option:: -mframe-header-opt, -mno-frame-header-opt

  Enable (disable) frame header optimization in the o32 ABI.  When using the
  o32 ABI, calling functions will allocate 16 bytes on the stack for the called
  function to write out register arguments.  When enabled, this optimization
  will suppress the allocation of the frame header if it can be determined that
  it is unused.

  This optimization is off by default at all optimization levels.

.. option:: -mlxc1-sxc1, -mno-lxc1-sxc1

  When applicable, enable (disable) the generation of ``lwxc1``,
  ``swxc1``, ``ldxc1``, ``sdxc1`` instructions.  Enabled by default.

.. option:: -mmadd4, -mno-madd4

  When applicable, enable (disable) the generation of 4-operand ``madd.s``,
  ``madd.d`` and related instructions.  Enabled by default.