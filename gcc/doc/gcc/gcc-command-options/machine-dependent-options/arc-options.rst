..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: ARC

.. index:: ARC options

.. _arc-options:

ARC Options
^^^^^^^^^^^

The following options control the architecture variant for which code
is being compiled:

.. architecture variants

.. option:: -mbarrel-shifter

  Generate instructions supported by barrel shifter.  This is the default
  unless :option:`-mcpu=ARC601` or :samp:`-mcpu=ARCEM` is in effect.

.. option:: -mjli-always

  Force to call a function using jli_s instruction.  This option is
  valid only for ARCv2 architecture.

.. option:: -mcpu={cpu}

  Set architecture type, register usage, and instruction scheduling
  parameters for :samp:`{cpu}`.  There are also shortcut alias options
  available for backward compatibility and convenience.  Supported
  values for :samp:`{cpu}` are

  .. index:: mA6, mARC600

  :samp:`arc600`
    Compile for ARC600.  Aliases: :option:`-mA6`, :option:`-mARC600`.

  :samp:`arc601`
    Compile for ARC601.  Alias: :option:`-mARC601`.

  :samp:`arc700`
    Compile for ARC700.  Aliases: :option:`-mA7`, :option:`-mARC700`.
    This is the default when configured with :option:`--with-cpu=arc700`.

  :samp:`arcem`
    Compile for ARC EM.

  :samp:`archs`
    Compile for ARC HS.

  :samp:`em`
    Compile for ARC EM CPU with no hardware extensions.

  :samp:`em4`
    Compile for ARC EM4 CPU.

  :samp:`em4_dmips`
    Compile for ARC EM4 DMIPS CPU.

  :samp:`em4_fpus`
    Compile for ARC EM4 DMIPS CPU with the single-precision floating-point
    extension.

  :samp:`em4_fpuda`
    Compile for ARC EM4 DMIPS CPU with single-precision floating-point and
    double assist instructions.

  :samp:`hs`
    Compile for ARC HS CPU with no hardware extensions except the atomic
    instructions.

  :samp:`hs34`
    Compile for ARC HS34 CPU.

  :samp:`hs38`
    Compile for ARC HS38 CPU.

  :samp:`hs38_linux`
    Compile for ARC HS38 CPU with all hardware extensions on.

  :samp:`hs4x`
    Compile for ARC HS4x CPU.

  :samp:`hs4xd`
    Compile for ARC HS4xD CPU.

  :samp:`hs4x_rel31`
    Compile for ARC HS4x CPU release 3.10a.

  :samp:`arc600_norm`
    Compile for ARC 600 CPU with ``norm`` instructions enabled.

  :samp:`arc600_mul32x16`
    Compile for ARC 600 CPU with ``norm`` and 32x16-bit multiply
    instructions enabled.

  :samp:`arc600_mul64`
    Compile for ARC 600 CPU with ``norm`` and ``mul64`` -family
    instructions enabled.

  :samp:`arc601_norm`
    Compile for ARC 601 CPU with ``norm`` instructions enabled.

  :samp:`arc601_mul32x16`
    Compile for ARC 601 CPU with ``norm`` and 32x16-bit multiply
    instructions enabled.

  :samp:`arc601_mul64`
    Compile for ARC 601 CPU with ``norm`` and ``mul64`` -family
    instructions enabled.

  :samp:`nps400`
    Compile for ARC 700 on NPS400 chip.

  :samp:`em_mini`
    Compile for ARC EM minimalist configuration featuring reduced register
    set.

.. option:: -mdpfp, -mdpfp-compact

  Generate double-precision FPX instructions, tuned for the compact
  implementation.

.. option:: -mdpfp-fast

  Generate double-precision FPX instructions, tuned for the fast
  implementation.

.. option:: -mno-dpfp-lrsr

  Disable ``lr`` and ``sr`` instructions from using FPX extension
  aux registers.

.. option:: -mea

  Generate extended arithmetic instructions.  Currently only
  ``divaw``, ``adds``, ``subs``, and ``sat16`` are
  supported.  Only valid for :option:`-mcpu=ARC700`.

.. option:: -mno-mpy

  Do not generate ``mpy`` -family instructions for ARC700.  This option is
  deprecated.

.. option:: -mmpy

  Default setting; overrides :option:`-mno-mpy`.

.. option:: -mmul32x16

  Generate 32x16-bit multiply and multiply-accumulate instructions.

.. option:: -mmul64

  Generate ``mul64`` and ``mulu64`` instructions.
  Only valid for :option:`-mcpu=ARC600`.

.. option:: -mnorm

  Generate ``norm`` instructions.  This is the default if :option:`-mcpu=ARC700`
  is in effect.

.. option:: -mspfp, -mspfp-compact

  Generate single-precision FPX instructions, tuned for the compact
  implementation.

.. option:: -mspfp-fast

  Generate single-precision FPX instructions, tuned for the fast
  implementation.

.. option:: -msimd

  Enable generation of ARC SIMD instructions via target-specific
  builtins.  Only valid for :option:`-mcpu=ARC700`.

.. option:: -msoft-float

  This option ignored; it is provided for compatibility purposes only.
  Software floating-point code is emitted by default, and this default
  can overridden by FPX options; :option:`-mspfp`, :option:`-mspfp-compact`, or
  :option:`-mspfp-fast` for single precision, and :option:`-mdpfp`,
  :option:`-mdpfp-compact`, or :option:`-mdpfp-fast` for double precision.

.. option:: -mswap

  Generate ``swap`` instructions.

.. option:: -matomic

  This enables use of the locked load/store conditional extension to implement
  atomic memory built-in functions.  Not available for ARC 6xx or ARC
  EM cores.

.. option:: -mdiv-rem

  Enable ``div`` and ``rem`` instructions for ARCv2 cores.

.. option:: -mcode-density

  Enable code density instructions for ARC EM.
  This option is on by default for ARC HS.

.. option:: -mll64

  Enable double load/store operations for ARC HS cores.

.. option:: -mtp-regno={regno}

  Specify thread pointer register number.

.. option:: -mmpy-option={multo}

  Compile ARCv2 code with a multiplier design option.  You can specify
  the option using either a string or numeric value for :samp:`{multo}`.
  :samp:`wlh1` is the default value.  The recognized values are:

  :samp:`0` :samp:`none`
    No multiplier available.

  :samp:`1` :samp:`w`
    16x16 multiplier, fully pipelined.
    The following instructions are enabled: ``mpyw`` and ``mpyuw``.

  :samp:`2` :samp:`wlh1`
    32x32 multiplier, fully
    pipelined (1 stage).  The following instructions are additionally
    enabled: ``mpy``, ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`3` :samp:`wlh2`
    32x32 multiplier, fully pipelined
    (2 stages).  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`4` :samp:`wlh3`
    Two 16x16 multipliers, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`5` :samp:`wlh4`
    One 16x16 multiplier, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`6` :samp:`wlh5`
    One 32x4 multiplier, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`7` :samp:`plus_dmpy`
    ARC HS SIMD support.

  :samp:`8` :samp:`plus_macd`
    ARC HS SIMD support.

  :samp:`9` :samp:`plus_qmacw`
    ARC HS SIMD support.

    This option is only available for ARCv2 cores.

.. option:: -mfpu={fpu}

  Enables support for specific floating-point hardware extensions for ARCv2
  cores.  Supported values for :samp:`{fpu}` are:

  :samp:`fpus`
    Enables support for single-precision floating-point hardware
    extensions.

  :samp:`fpud`
    Enables support for double-precision floating-point hardware
    extensions.  The single-precision floating-point extension is also
    enabled.  Not available for ARC EM.

  :samp:`fpuda`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.  The single-precision
    floating-point extension is also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_div`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    The single-precision floating-point, square-root, and divide
    extensions are also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_fma`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    The single-precision floating-point and fused multiply and add
    hardware extensions are also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_all`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    All single-precision floating-point hardware extensions are also
    enabled.  This option is only available for ARC EM.

  :samp:`fpus_div`
    Enables support for single-precision floating-point, square-root and divide
    hardware extensions.

  :samp:`fpud_div`
    Enables support for double-precision floating-point, square-root and divide
    hardware extensions.  This option
    includes option :samp:`fpus_div`. Not available for ARC EM.

  :samp:`fpus_fma`
    Enables support for single-precision floating-point and
    fused multiply and add hardware extensions.

  :samp:`fpud_fma`
    Enables support for double-precision floating-point and
    fused multiply and add hardware extensions.  This option
    includes option :samp:`fpus_fma`.  Not available for ARC EM.

  :samp:`fpus_all`
    Enables support for all single-precision floating-point hardware
    extensions.

  :samp:`fpud_all`
    Enables support for all single- and double-precision floating-point
    hardware extensions.  Not available for ARC EM.

.. option:: -mirq-ctrl-saved={register-range},{blink},{lp_count}

  Specifies general-purposes registers that the processor automatically
  saves/restores on interrupt entry and exit.  :samp:`{register-range}` is
  specified as two registers separated by a dash.  The register range
  always starts with ``r0``, the upper limit is ``fp`` register.
  :samp:`{blink}` and :samp:`{lp_count}` are optional.  This option is only
  valid for ARC EM and ARC HS cores.

.. option:: -mrgf-banked-regs={number}

  Specifies the number of registers replicated in second register bank
  on entry to fast interrupt.  Fast interrupts are interrupts with the
  highest priority level P0.  These interrupts save only PC and STATUS32
  registers to avoid memory transactions during interrupt entry and exit
  sequences.  Use this option when you are using fast interrupts in an
  ARC V2 family processor.  Permitted values are 4, 8, 16, and 32.

.. option:: -mlpc-width={width}

  Specify the width of the ``lp_count`` register.  Valid values for
  :samp:`{width}` are 8, 16, 20, 24, 28 and 32 bits.  The default width is
  fixed to 32 bits.  If the width is less than 32, the compiler does not
  attempt to transform loops in your program to use the zero-delay loop
  mechanism unless it is known that the ``lp_count`` register can
  hold the required loop-counter value.  Depending on the width
  specified, the compiler and run-time library might continue to use the
  loop mechanism for various needs.  This option defines macro
  ``__ARC_LPC_WIDTH__`` with the value of :samp:`{width}`.

.. option:: -mrf16

  This option instructs the compiler to generate code for a 16-entry
  register file.  This option defines the ``__ARC_RF16__``
  preprocessor macro.

.. option:: -mbranch-index

  Enable use of ``bi`` or ``bih`` instructions to implement jump
  tables.

The following options are passed through to the assembler, and also
define preprocessor macro symbols.

.. Flags used by the assembler, but for which we define preprocessor
   macro symbols as well.

.. option:: -mdsp-packa

  Passed down to the assembler to enable the DSP Pack A extensions.
  Also sets the preprocessor symbol ``__Xdsp_packa``.  This option is
  deprecated.

.. option:: -mdvbf

  Passed down to the assembler to enable the dual Viterbi butterfly
  extension.  Also sets the preprocessor symbol ``__Xdvbf``.  This
  option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mlock

  Passed down to the assembler to enable the locked load/store
  conditional extension.  Also sets the preprocessor symbol
  ``__Xlock``.

.. option:: -mmac-d16

  Passed down to the assembler.  Also sets the preprocessor symbol
  ``__Xxmac_d16``.  This option is deprecated.

.. option:: -mmac-24

  Passed down to the assembler.  Also sets the preprocessor symbol
  ``__Xxmac_24``.  This option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mrtsc

  Passed down to the assembler to enable the 64-bit time-stamp counter
  extension instruction.  Also sets the preprocessor symbol
  ``__Xrtsc``.  This option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mswape

  Passed down to the assembler to enable the swap byte ordering
  extension instruction.  Also sets the preprocessor symbol
  ``__Xswape``.

.. option:: -mtelephony

  Passed down to the assembler to enable dual- and single-operand
  instructions for telephony.  Also sets the preprocessor symbol
  ``__Xtelephony``.  This option is deprecated.

.. option:: -mxy

  Passed down to the assembler to enable the XY memory extension.  Also
  sets the preprocessor symbol ``__Xxy``.

The following options control how the assembly code is annotated:

.. Assembly annotation options

.. option:: -misize

  Annotate assembler instructions with estimated addresses.

.. option:: -mannotate-align

  Explain what alignment considerations lead to the decision to make an
  instruction short or long.

The following options are passed through to the linker:

.. options passed through to the linker

.. option:: -marclinux

  Passed through to the linker, to specify use of the ``arclinux`` emulation.
  This option is enabled by default in tool chains built for
  ``arc-linux-uclibc`` and ``arceb-linux-uclibc`` targets
  when profiling is not requested.

.. option:: -marclinux_prof

  Passed through to the linker, to specify use of the
  ``arclinux_prof`` emulation.  This option is enabled by default in
  tool chains built for ``arc-linux-uclibc`` and
  ``arceb-linux-uclibc`` targets when profiling is requested.

The following options control the semantics of generated code:

.. semantically relevant code generation options

.. option:: -mlong-calls

  Generate calls as register indirect calls, thus providing access
  to the full 32-bit address range.

.. option:: -mmedium-calls

  Don't use less than 25-bit addressing range for calls, which is the
  offset available for an unconditional branch-and-link
  instruction.  Conditional execution of function calls is suppressed, to
  allow use of the 25-bit range, rather than the 21-bit range with
  conditional branch-and-link.  This is the default for tool chains built
  for ``arc-linux-uclibc`` and ``arceb-linux-uclibc`` targets.

.. option:: -G {num}

  Put definitions of externally-visible data in a small data section if
  that data is no bigger than :samp:`{num}` bytes.  The default value of
  :samp:`{num}` is 4 for any ARC configuration, or 8 when we have double
  load/store operations.

.. option:: -mno-sdata

  Do not generate sdata references.  This is the default for tool chains
  built for ``arc-linux-uclibc`` and ``arceb-linux-uclibc``
  targets.

.. option:: -msdata

  Default setting; overrides :option:`-mno-sdata`.

.. option:: -mvolatile-cache

  Use ordinarily cached memory accesses for volatile references.  This is the
  default.

.. option:: -mno-volatile-cache

  Enable cache bypass for volatile references.

.. option:: -mvolatile-cache

  Default setting; overrides :option:`-mno-volatile-cache`.

The following options fine tune code generation:

.. code generation tuning options

.. option:: -malign-call

  Does nothing.  Preserved for backward compatibility.

.. option:: -mauto-modify-reg

  Enable the use of pre/post modify with register displacement.

.. option:: -mbbit-peephole

  Enable bbit peephole2.

.. option:: -mno-brcc

  This option disables a target-specific pass in :samp:`arc_reorg` to
  generate compare-and-branch (``brcc``) instructions.
  It has no effect on
  generation of these instructions driven by the combiner pass.

.. option:: -mcase-vector-pcrel

  Use PC-relative switch case tables to enable case table shortening.
  This is the default for :option:`-Os`.

.. option:: -mcompact-casesi

  Enable compact ``casesi`` pattern.  This is the default for :option:`-Os`,
  and only available for ARCv1 cores.  This option is deprecated.

.. option:: -mno-cond-exec

  Disable the ARCompact-specific pass to generate conditional
  execution instructions.

  Due to delay slot scheduling and interactions between operand numbers,
  literal sizes, instruction lengths, and the support for conditional execution,
  the target-independent pass to generate conditional execution is often lacking,
  so the ARC port has kept a special pass around that tries to find more
  conditional execution generation opportunities after register allocation,
  branch shortening, and delay slot scheduling have been done.  This pass
  generally, but not always, improves performance and code size, at the cost of
  extra compilation time, which is why there is an option to switch it off.
  If you have a problem with call instructions exceeding their allowable
  offset range because they are conditionalized, you should consider using
  :option:`-mmedium-calls` instead.

.. option:: -mearly-cbranchsi

  Enable pre-reload use of the ``cbranchsi`` pattern.

.. option:: -mexpand-adddi

  Expand ``adddi3`` and ``subdi3`` at RTL generation time into
  ``add.f``, ``adc`` etc.  This option is deprecated.

.. option:: -mindexed-loads

  Enable the use of indexed loads.  This can be problematic because some
  optimizers then assume that indexed stores exist, which is not
  the case.

.. option:: -mlra

  Enable Local Register Allocation.  This is still experimental for ARC,
  so by default the compiler uses standard reload
  (i.e. :option:`-mno-lra`).

.. option:: -mlra-priority-none

  Don't indicate any priority for target registers.

.. option:: -mlra-priority-compact

  Indicate target register priority for ``r0`` .. ``r3`` / ``r12`` .. ``r15``.

.. option:: -mlra-priority-noncompact

  Reduce target register priority for ``r0`` .. ``r3`` / ``r12`` .. ``r15``.

.. option:: -mmillicode

  When optimizing for size (using :option:`-Os`), prologues and epilogues
  that have to save or restore a large number of registers are often
  shortened by using call to a special function in libgcc; this is
  referred to as a *millicode* call.  As these calls can pose
  performance issues, and/or cause linking issues when linking in a
  nonstandard way, this option is provided to turn on or off millicode
  call generation.

.. option:: -mcode-density-frame

  This option enable the compiler to emit ``enter`` and ``leave``
  instructions.  These instructions are only valid for CPUs with
  code-density feature.

.. option:: -mmixed-code

  Does nothing.  Preserved for backward compatibility.

.. option:: -mq-class

  Ths option is deprecated.  Enable :samp:`q` instruction alternatives.
  This is the default for :option:`-Os`.

.. option:: -mRcq

  Does nothing.  Preserved for backward compatibility.

.. option:: -mRcw

  Does nothing.  Preserved for backward compatibility.

.. option:: -msize-level={level}

  Fine-tune size optimization with regards to instruction lengths and alignment.
  The recognized values for :samp:`{level}` are:

  :samp:`0`
    No size optimization.  This level is deprecated and treated like :samp:`1`.

  :samp:`1`
    Short instructions are used opportunistically.

  :samp:`2`
    In addition, alignment of loops and of code after barriers are dropped.

  :samp:`3`
    In addition, optional data alignment is dropped, and the option Os is enabled.

  This defaults to :samp:`3` when :option:`-Os` is in effect.  Otherwise,
  the behavior when this is not set is equivalent to level :samp:`1`.

.. option:: -mtune={cpu}

  Set instruction scheduling parameters for :samp:`{cpu}`, overriding any implied
  by :option:`-mcpu=`.

  Supported values for :samp:`{cpu}` are

  :samp:`ARC600`
    Tune for ARC600 CPU.

  :samp:`ARC601`
    Tune for ARC601 CPU.

  :samp:`ARC700`
    Tune for ARC700 CPU with standard multiplier block.

  :samp:`ARC700-xmac`
    Tune for ARC700 CPU with XMAC block.

  :samp:`ARC725D`
    Tune for ARC725D CPU.

  :samp:`ARC750D`
    Tune for ARC750D CPU.

  :samp:`core3`
    Tune for ARCv2 core3 type CPU.  This option enable usage of
    ``dbnz`` instruction.

  :samp:`release31a`
    Tune for ARC4x release 3.10a.

.. option:: -mmultcost={num}

  Cost to assume for a multiply instruction, with :samp:`4` being equal to a
  normal instruction.

.. option:: -munalign-prob-threshold={probability}

  Does nothing.  Preserved for backward compatibility.

The following options are maintained for backward compatibility, but
are now deprecated and will be removed in a future release:

.. Deprecated options

.. option:: -margonaut

  Obsolete FPX.

.. option:: -mbig-endian, -EB

  Compile code for big-endian targets.  Use of these options is now
  deprecated.  Big-endian code is supported by configuring GCC to build
  ``arceb-elf32`` and ``arceb-linux-uclibc`` targets,
  for which big endian is the default.

.. option:: -mlittle-endian, -EL

  Compile code for little-endian targets.  Use of these options is now
  deprecated.  Little-endian code is supported by configuring GCC to build
  ``arc-elf32`` and ``arc-linux-uclibc`` targets,
  for which little endian is the default.

.. option:: -mbarrel_shifter

  Replaced by :option:`-mbarrel-shifter`.

.. option:: -mdpfp_compact

  Replaced by :option:`-mdpfp-compact`.

.. option:: -mdpfp_fast

  Replaced by :option:`-mdpfp-fast`.

.. option:: -mdsp_packa

  Replaced by :option:`-mdsp-packa`.

.. option:: -mEA

  Replaced by :option:`-mea`.

.. option:: -mmac_24

  Replaced by :option:`-mmac-24`.

.. option:: -mmac_d16

  Replaced by :option:`-mmac-d16`.

.. option:: -mspfp_compact

  Replaced by :option:`-mspfp-compact`.

.. option:: -mspfp_fast

  Replaced by :option:`-mspfp-fast`.

.. option:: -mtune={cpu}

  Values :samp:`arc600`, :samp:`arc601`, :samp:`arc700` and
  :samp:`arc700-xmac` for :samp:`{cpu}` are replaced by :samp:`ARC600`,
  :samp:`ARC601`, :samp:`ARC700` and :samp:`ARC700-xmac` respectively.

.. option:: -multcost={num}

  Replaced by :option:`-mmultcost`.