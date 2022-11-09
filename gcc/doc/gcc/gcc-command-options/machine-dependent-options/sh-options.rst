..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: SH

.. _sh-options:

SH Options
^^^^^^^^^^

These :samp:`-m` options are defined for the SH implementations:

.. option:: -m1

  Generate code for the SH1.

.. option:: -m2

  Generate code for the SH2.

.. option:: -m2e

  Generate code for the SH2e.

.. option:: -m2a-nofpu

  Generate code for the SH2a without FPU, or for a SH2a-FPU in such a way
  that the floating-point unit is not used.

.. option:: -m2a-single-only

  Generate code for the SH2a-FPU, in such a way that no double-precision
  floating-point operations are used.

.. option:: -m2a-single

  Generate code for the SH2a-FPU assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m2a

  Generate code for the SH2a-FPU assuming the floating-point unit is in
  double-precision mode by default.

.. option:: -m3

  Generate code for the SH3.

.. option:: -m3e

  Generate code for the SH3e.

.. option:: -m4-nofpu

  Generate code for the SH4 without a floating-point unit.

.. option:: -m4-single-only

  Generate code for the SH4 with a floating-point unit that only
  supports single-precision arithmetic.

.. option:: -m4-single

  Generate code for the SH4 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4

  Generate code for the SH4.

.. option:: -m4-100

  Generate code for SH4-100.

.. option:: -m4-100-nofpu

  Generate code for SH4-100 in such a way that the
  floating-point unit is not used.

.. option:: -m4-100-single

  Generate code for SH4-100 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4-100-single-only

  Generate code for SH4-100 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-200

  Generate code for SH4-200.

.. option:: -m4-200-nofpu

  Generate code for SH4-200 without in such a way that the
  floating-point unit is not used.

.. option:: -m4-200-single

  Generate code for SH4-200 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4-200-single-only

  Generate code for SH4-200 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-300

  Generate code for SH4-300.

.. option:: -m4-300-nofpu

  Generate code for SH4-300 without in such a way that the
  floating-point unit is not used.

.. option:: -m4-300-single

  Generate code for SH4-300 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-300-single-only

  Generate code for SH4-300 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-340

  Generate code for SH4-340 (no MMU, no FPU).

.. option:: -m4-500

  Generate code for SH4-500 (no FPU).  Passes :option:`-isa=sh4-nofpu` to the
  assembler.

.. option:: -m4a-nofpu

  Generate code for the SH4al-dsp, or for a SH4a in such a way that the
  floating-point unit is not used.

.. option:: -m4a-single-only

  Generate code for the SH4a, in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4a-single

  Generate code for the SH4a assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4a

  Generate code for the SH4a.

.. option:: -m4al

  Same as :option:`-m4a-nofpu`, except that it implicitly passes
  :option:`-dsp` to the assembler.  GCC doesn't generate any DSP
  instructions at the moment.

.. option:: -mb

  Compile code for the processor in big-endian mode.

.. option:: -ml

  Compile code for the processor in little-endian mode.

.. option:: -mdalign

  Align doubles at 64-bit boundaries.  Note that this changes the calling
  conventions, and thus some functions from the standard C library do
  not work unless you recompile it first with :option:`-mdalign`.

.. option:: -mrelax

  Shorten some address references at link time, when possible; uses the
  linker option :option:`-relax`.

.. option:: -mbigtable

  Use 32-bit offsets in ``switch`` tables.  The default is to use
  16-bit offsets.

.. option:: -mbitops

  Enable the use of bit manipulation instructions on SH2A.

.. option:: -mfmovd

  Enable the use of the instruction ``fmovd``.  Check :option:`-mdalign` for
  alignment constraints.

.. option:: -mrenesas

  Comply with the calling conventions defined by Renesas.

.. option:: -mno-renesas

  Comply with the calling conventions defined for GCC before the Renesas
  conventions were available.  This option is the default for all
  targets of the SH toolchain.

.. option:: -mnomacsave

  Mark the ``MAC`` register as call-clobbered, even if
  :option:`-mrenesas` is given.

.. option:: -mieee, -mno-ieee

  Control the IEEE compliance of floating-point comparisons, which affects the
  handling of cases where the result of a comparison is unordered.  By default
  :option:`-mieee` is implicitly enabled.  If :option:`-ffinite-math-only` is
  enabled :option:`-mno-ieee` is implicitly set, which results in faster
  floating-point greater-equal and less-equal comparisons.  The implicit settings
  can be overridden by specifying either :option:`-mieee` or :option:`-mno-ieee`.

.. option:: -minline-ic_invalidate

  Inline code to invalidate instruction cache entries after setting up
  nested function trampolines.
  This option has no effect if :option:`-musermode` is in effect and the selected
  code generation option (e.g. :option:`-m4`) does not allow the use of the ``icbi``
  instruction.
  If the selected code generation option does not allow the use of the ``icbi``
  instruction, and :option:`-musermode` is not in effect, the inlined code
  manipulates the instruction cache address array directly with an associative
  write.  This not only requires privileged mode at run time, but it also
  fails if the cache line had been mapped via the TLB and has become unmapped.

.. option:: -misize

  Dump instruction size and location in the assembly code.

.. option:: -mpadstruct

  This option is deprecated.  It pads structures to multiple of 4 bytes,
  which is incompatible with the SH ABI.

.. index:: matomic-model=model

.. option:: -matomic-model={model}

  Sets the model of atomic operations and additional parameters as a comma
  separated list.  For details on the atomic built-in functions see
  :ref:`atomic-builtins`.  The following models and parameters are supported:

  :samp:`none`
    Disable compiler generated atomic sequences and emit library calls for atomic
    operations.  This is the default if the target is not ``sh*-*-linux*``.

  :samp:`soft-gusa`
    Generate GNU/Linux compatible gUSA software atomic sequences for the atomic
    built-in functions.  The generated atomic sequences require additional support
    from the interrupt/exception handling code of the system and are only suitable
    for SH3\* and SH4\* single-core systems.  This option is enabled by default when
    the target is ``sh*-*-linux*`` and SH3\* or SH4\*.  When the target is SH4A,
    this option also partially utilizes the hardware atomic instructions
    ``movli.l`` and ``movco.l`` to create more efficient code, unless
    :samp:`strict` is specified.

  :samp:`soft-tcb`
    Generate software atomic sequences that use a variable in the thread control
    block.  This is a variation of the gUSA sequences which can also be used on
    SH1\* and SH2\* targets.  The generated atomic sequences require additional
    support from the interrupt/exception handling code of the system and are only
    suitable for single-core systems.  When using this model, the :samp:`gbr-offset=`
    parameter has to be specified as well.

  :samp:`soft-imask`
    Generate software atomic sequences that temporarily disable interrupts by
    setting ``SR.IMASK = 1111``.  This model works only when the program runs
    in privileged mode and is only suitable for single-core systems.  Additional
    support from the interrupt/exception handling code of the system is not
    required.  This model is enabled by default when the target is
    ``sh*-*-linux*`` and SH1\* or SH2\*.

  :samp:`hard-llcs`
    Generate hardware atomic sequences using the ``movli.l`` and ``movco.l``
    instructions only.  This is only available on SH4A and is suitable for
    multi-core systems.  Since the hardware instructions support only 32 bit atomic
    variables access to 8 or 16 bit variables is emulated with 32 bit accesses.
    Code compiled with this option is also compatible with other software
    atomic model interrupt/exception handling systems if executed on an SH4A
    system.  Additional support from the interrupt/exception handling code of the
    system is not required for this model.

  :samp:`gbr-offset=`
    This parameter specifies the offset in bytes of the variable in the thread
    control block structure that should be used by the generated atomic sequences
    when the :samp:`soft-tcb` model has been selected.  For other models this
    parameter is ignored.  The specified value must be an integer multiple of four
    and in the range 0-1020.

  :samp:`strict`
    This parameter prevents mixed usage of multiple atomic models, even if they
    are compatible, and makes the compiler generate atomic sequences of the
    specified model only.

.. option:: -mtas

  Generate the ``tas.b`` opcode for ``__atomic_test_and_set``.
  Notice that depending on the particular hardware and software configuration
  this can degrade overall performance due to the operand cache line flushes
  that are implied by the ``tas.b`` instruction.  On multi-core SH4A
  processors the ``tas.b`` instruction must be used with caution since it
  can result in data corruption for certain cache configurations.

.. option:: -mprefergot

  When generating position-independent code, emit function calls using
  the Global Offset Table instead of the Procedure Linkage Table.

.. option:: -musermode, -mno-usermode

  Don't allow (allow) the compiler generating privileged mode code.  Specifying
  :option:`-musermode` also implies :option:`-mno-inline-ic_invalidate` if the
  inlined code would not work in user mode.  :option:`-musermode` is the default
  when the target is ``sh*-*-linux*``.  If the target is SH1\* or SH2\*
  :option:`-musermode` has no effect, since there is no user mode.

.. index:: multcost=number

.. option:: -multcost={number}

  Set the cost to assume for a multiply insn.

.. index:: mdiv=strategy

.. option:: -mdiv={strategy}

  Set the division strategy to be used for integer division operations.
  :samp:`{strategy}` can be one of:

  :samp:`call-div1`
    Calls a library function that uses the single-step division instruction
    ``div1`` to perform the operation.  Division by zero calculates an
    unspecified result and does not trap.  This is the default except for SH4,
    SH2A and SHcompact.

  :samp:`call-fp`
    Calls a library function that performs the operation in double precision
    floating point.  Division by zero causes a floating-point exception.  This is
    the default for SHcompact with FPU.  Specifying this for targets that do not
    have a double precision FPU defaults to ``call-div1``.

  :samp:`call-table`
    Calls a library function that uses a lookup table for small divisors and
    the ``div1`` instruction with case distinction for larger divisors.  Division
    by zero calculates an unspecified result and does not trap.  This is the default
    for SH4.  Specifying this for targets that do not have dynamic shift
    instructions defaults to ``call-div1``.

  When a division strategy has not been specified the default strategy is
  selected based on the current target.  For SH2A the default strategy is to
  use the ``divs`` and ``divu`` instructions instead of library function
  calls.

.. option:: -maccumulate-outgoing-args

  Reserve space once for outgoing arguments in the function prologue rather
  than around each call.  Generally beneficial for performance and size.  Also
  needed for unwinding to avoid changing the stack frame around conditional code.

.. index:: mdivsi3_libfunc=name

.. option:: -mdivsi3_libfunc={name}

  Set the name of the library function used for 32-bit signed division to
  :samp:`{name}`.
  This only affects the name used in the :samp:`call` division strategies, and
  the compiler still expects the same sets of input/output/clobbered registers as
  if this option were not present.

.. option:: -mfixed-range={register-range}

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. index:: mbranch-cost=num

.. option:: -mbranch-cost={num}

  Assume :samp:`{num}` to be the cost for a branch instruction.  Higher numbers
  make the compiler try to generate more branch-free code if possible.
  If not specified the value is selected depending on the processor type that
  is being compiled for.

.. option:: -mzdcbranch, -mno-zdcbranch

  Assume (do not assume) that zero displacement conditional branch instructions
  ``bt`` and ``bf`` are fast.  If :option:`-mzdcbranch` is specified, the
  compiler prefers zero displacement branch code sequences.  This is
  enabled by default when generating code for SH4 and SH4A.  It can be explicitly
  disabled by specifying :option:`-mno-zdcbranch`.

.. option:: -mcbranch-force-delay-slot

  Force the usage of delay slots for conditional branches, which stuffs the delay
  slot with a ``nop`` if a suitable instruction cannot be found.  By default
  this option is disabled.  It can be enabled to work around hardware bugs as
  found in the original SH7055.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default
  if hardware floating point is used.  The machine-dependent
  :option:`-mfused-madd` option is now mapped to the machine-independent
  :option:`-ffp-contract=fast` option, and :option:`-mno-fused-madd` is
  mapped to :option:`-ffp-contract=off`.

.. option:: -mfsca, -mno-fsca

  Allow or disallow the compiler to emit the ``fsca`` instruction for sine
  and cosine approximations.  The option :option:`-mfsca` must be used in
  combination with :option:`-funsafe-math-optimizations`.  It is enabled by default
  when generating code for SH4A.  Using :option:`-mno-fsca` disables sine and cosine
  approximations even if :option:`-funsafe-math-optimizations` is in effect.

.. option:: -mfsrra, -mno-fsrra

  Allow or disallow the compiler to emit the ``fsrra`` instruction for
  reciprocal square root approximations.  The option :option:`-mfsrra` must be used
  in combination with :option:`-funsafe-math-optimizations` and
  :option:`-ffinite-math-only`.  It is enabled by default when generating code for
  SH4A.  Using :option:`-mno-fsrra` disables reciprocal square root approximations
  even if :option:`-funsafe-math-optimizations` and :option:`-ffinite-math-only` are
  in effect.

.. option:: -mpretend-cmove

  Prefer zero-displacement conditional branches for conditional move instruction
  patterns.  This can result in faster code on the SH4 processor.

.. option:: -mfdpic

  Generate code using the FDPIC ABI.
