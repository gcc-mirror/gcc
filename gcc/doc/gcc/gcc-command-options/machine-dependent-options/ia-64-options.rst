..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: IA-64

.. index:: IA-64 Options

.. _ia-64-options:

IA-64 Options
^^^^^^^^^^^^^

These are the :samp:`-m` options defined for the Intel IA-64 architecture.

.. option:: -mbig-endian

  Generate code for a big-endian target.  This is the default for HP-UX.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default for AIX5
  and GNU/Linux.

.. option:: -mgnu-as, -mno-gnu-as

  Generate (or don't) code for the GNU assembler.  This is the default.

  .. Also, this is the default if the configure option @option{-with-gnu-as}

  .. is used.

.. option:: -mgnu-ld, -mno-gnu-ld

  Generate (or don't) code for the GNU linker.  This is the default.

  .. Also, this is the default if the configure option @option{-with-gnu-ld}

  .. is used.

.. option:: -mno-pic

  Generate code that does not use a global pointer register.  The result
  is not position independent code, and violates the IA-64 ABI.

.. option:: -mvolatile-asm-stop, -mno-volatile-asm-stop

  Generate (or don't) a stop bit immediately before and after volatile asm
  statements.

.. option:: -mregister-names, -mno-register-names

  Generate (or don't) :samp:`in`, :samp:`loc`, and :samp:`out` register names for
  the stacked registers.  This may make assembler output more readable.

.. option:: -mno-sdata, -msdata

  Disable (or enable) optimizations that use the small data section.  This may
  be useful for working around optimizer bugs.

.. option:: -mconstant-gp

  Generate code that uses a single constant global pointer value.  This is
  useful when compiling kernel code.

.. option:: -mauto-pic

  Generate code that is self-relocatable.  This implies :option:`-mconstant-gp`.
  This is useful when compiling firmware code.

.. option:: -minline-float-divide-min-latency

  Generate code for inline divides of floating-point values
  using the minimum latency algorithm.

.. option:: -minline-float-divide-max-throughput

  Generate code for inline divides of floating-point values
  using the maximum throughput algorithm.

.. option:: -mno-inline-float-divide

  Do not generate inline code for divides of floating-point values.

.. option:: -minline-int-divide-min-latency

  Generate code for inline divides of integer values
  using the minimum latency algorithm.

.. option:: -minline-int-divide-max-throughput

  Generate code for inline divides of integer values
  using the maximum throughput algorithm.

.. option:: -mno-inline-int-divide

  Do not generate inline code for divides of integer values.

.. option:: -minline-int-divide

  Default setting; overrides :option:`-mno-inline-int-divide`.

.. option:: -minline-sqrt-min-latency

  Generate code for inline square roots
  using the minimum latency algorithm.

.. option:: -minline-sqrt-max-throughput

  Generate code for inline square roots
  using the maximum throughput algorithm.

.. option:: -mno-inline-sqrt

  Do not generate inline code for ``sqrt``.

.. option:: -mfused-madd, -mno-fused-madd

  Do (don't) generate code that uses the fused multiply/add or multiply/subtract
  instructions.  The default is to use these instructions.

.. option:: -mno-dwarf2-asm, -mdwarf2-asm

  Don't (or do) generate assembler code for the DWARF line number debugging
  info.  This may be useful when not using the GNU assembler.

.. option:: -mearly-stop-bits, -mno-early-stop-bits

  Allow stop bits to be placed earlier than immediately preceding the
  instruction that triggered the stop bit.  This can improve instruction
  scheduling, but does not always do so.

.. option:: -mfixed-range={register-range}

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. option:: -mtls-size={tls-size}

  Specify bit size of immediate TLS offsets.  Valid values are 14, 22, and
  64.

.. option:: -mtune={cpu-type}

  Tune the instruction scheduling for a particular CPU, Valid values are
  :samp:`itanium`, :samp:`itanium1`, :samp:`merced`, :samp:`itanium2`,
  and :samp:`mckinley`.

.. option:: -milp32, -mlp64

  Generate code for a 32-bit or 64-bit environment.
  The 32-bit environment sets int, long and pointer to 32 bits.
  The 64-bit environment sets int to 32 bits and long and pointer
  to 64 bits.  These are HP-UX specific flags.

.. option:: -mno-sched-br-data-spec, -msched-br-data-spec

  (Dis/En)able data speculative scheduling before reload.
  This results in generation of ``ld.a`` instructions and
  the corresponding check instructions (``ld.c`` / ``chk.a``).
  The default setting is disabled.

.. option:: -msched-ar-data-spec, -mno-sched-ar-data-spec

  (En/Dis)able data speculative scheduling after reload.
  This results in generation of ``ld.a`` instructions and
  the corresponding check instructions (``ld.c`` / ``chk.a``).
  The default setting is enabled.

.. option:: -mno-sched-control-spec, -msched-control-spec

  (Dis/En)able control speculative scheduling.  This feature is
  available only during region scheduling (i.e. before reload).
  This results in generation of the ``ld.s`` instructions and
  the corresponding check instructions ``chk.s``.
  The default setting is disabled.

.. option:: -msched-br-in-data-spec, -mno-sched-br-in-data-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the data speculative loads before reload.
  This is effective only with :option:`-msched-br-data-spec` enabled.
  The default setting is enabled.

.. option:: -msched-ar-in-data-spec, -mno-sched-ar-in-data-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the data speculative loads after reload.
  This is effective only with :option:`-msched-ar-data-spec` enabled.
  The default setting is enabled.

.. option:: -msched-in-control-spec, -mno-sched-in-control-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the control speculative loads.
  This is effective only with :option:`-msched-control-spec` enabled.
  The default setting is enabled.

.. option:: -mno-sched-prefer-non-data-spec-insns, -msched-prefer-non-data-spec-insns

  If enabled, data-speculative instructions are chosen for schedule
  only if there are no other choices at the moment.  This makes
  the use of the data speculation much more conservative.
  The default setting is disabled.

.. option:: -mno-sched-prefer-non-control-spec-insns, -msched-prefer-non-control-spec-insns

  If enabled, control-speculative instructions are chosen for schedule
  only if there are no other choices at the moment.  This makes
  the use of the control speculation much more conservative.
  The default setting is disabled.

.. option:: -mno-sched-count-spec-in-critical-path, -msched-count-spec-in-critical-path

  If enabled, speculative dependencies are considered during
  computation of the instructions priorities.  This makes the use of the
  speculation a bit more conservative.
  The default setting is disabled.

.. option:: -msched-spec-ldc

  Use a simple data speculation check.  This option is on by default.

.. option:: -msched-control-spec-ldc

  Use a simple check for control speculation.  This option is on by default.

.. option:: -msched-stop-bits-after-every-cycle

  Place a stop bit after every cycle when scheduling.  This option is on
  by default.

.. option:: -msched-fp-mem-deps-zero-cost

  Assume that floating-point stores and loads are not likely to cause a conflict
  when placed into the same instruction group.  This option is disabled by
  default.

.. option:: -msel-sched-dont-check-control-spec

  Generate checks for control speculation in selective scheduling.
  This flag is disabled by default.

.. option:: -msched-max-memory-insns={max-insns}

  Limit on the number of memory insns per instruction group, giving lower
  priority to subsequent memory insns attempting to schedule in the same
  instruction group. Frequently useful to prevent cache bank conflicts.
  The default value is 1.

.. option:: -msched-max-memory-insns-hard-limit

  Makes the limit specified by msched-max-memory-insns a hard limit,
  disallowing more than that number in an instruction group.
  Otherwise, the limit is 'soft', meaning that non-memory operations
  are preferred when the limit is reached, but memory operations may still
  be scheduled.