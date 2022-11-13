..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Adapteva Epiphany

.. _adapteva-epiphany-options:

Adapteva Epiphany Options
^^^^^^^^^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for Adapteva Epiphany:

.. option:: -mhalf-reg-file

  Don't allocate any register in the range ``r32``... ``r63``.
  That allows code to run on hardware variants that lack these registers.

.. option:: -mprefer-short-insn-regs

  Preferentially allocate registers that allow short instruction generation.
  This can result in increased instruction count, so this may either reduce or
  increase overall code size.

.. option:: -mbranch-cost={num}

  Set the cost of branches to roughly :samp:`{num}` 'simple' instructions.
  This cost is only a heuristic and is not guaranteed to produce
  consistent results across releases.

.. option:: -mcmove

  Enable the generation of conditional moves.

.. option:: -mnops={num}

  Emit :samp:`{num}` NOPs before every other generated instruction.

.. option:: -mno-soft-cmpsf

  For single-precision floating-point comparisons, emit an ``fsub`` instruction
  and test the flags.  This is faster than a software comparison, but can
  get incorrect results in the presence of NaNs, or when two different small
  numbers are compared such that their difference is calculated as zero.
  The default is :option:`-msoft-cmpsf`, which uses slower, but IEEE-compliant,
  software comparisons.

.. option:: -msoft-cmpsf

  Default setting; overrides :option:`-mno-soft-cmpsf`.

.. option:: -mstack-offset={num}

  Set the offset between the top of the stack and the stack pointer.
  E.g., a value of 8 means that the eight bytes in the range ``sp+0...sp+7``
  can be used by leaf functions without stack allocation.
  Values other than :samp:`8` or :samp:`16` are untested and unlikely to work.
  Note also that this option changes the ABI; compiling a program with a
  different stack offset than the libraries have been compiled with
  generally does not work.
  This option can be useful if you want to evaluate if a different stack
  offset would give you better code, but to actually use a different stack
  offset to build working programs, it is recommended to configure the
  toolchain with the appropriate :option:`--with-stack-offset=num` option.

.. option:: -mno-round-nearest

  Make the scheduler assume that the rounding mode has been set to
  truncating.  The default is :option:`-mround-nearest`.

.. option:: -mround-nearest

  Default setting; overrides :option:`-mno-round-nearest`.

.. option:: -mlong-calls

  If not otherwise specified by an attribute, assume all calls might be beyond
  the offset range of the ``b`` / ``bl`` instructions, and therefore load the
  function address into a register before performing a (otherwise direct) call.
  This is the default.

.. option:: -mshort-calls

  If not otherwise specified by an attribute, assume all direct calls are
  in the range of the ``b`` / ``bl`` instructions, so use these instructions
  for direct calls.  The default is :option:`-mlong-calls`.

.. option:: -msmall16

  Assume addresses can be loaded as 16-bit unsigned values.  This does not
  apply to function addresses for which :option:`-mlong-calls` semantics
  are in effect.

.. option:: -mfp-mode={mode}

  Set the prevailing mode of the floating-point unit.
  This determines the floating-point mode that is provided and expected
  at function call and return time.  Making this mode match the mode you
  predominantly need at function start can make your programs smaller and
  faster by avoiding unnecessary mode switches.

  :samp:`{mode}` can be set to one the following values:

  :samp:`caller`
    Any mode at function entry is valid, and retained or restored when
    the function returns, and when it calls other functions.
    This mode is useful for compiling libraries or other compilation units
    you might want to incorporate into different programs with different
    prevailing FPU modes, and the convenience of being able to use a single
    object file outweighs the size and speed overhead for any extra
    mode switching that might be needed, compared with what would be needed
    with a more specific choice of prevailing FPU mode.

  :samp:`truncate`
    This is the mode used for floating-point calculations with
    truncating (i.e. round towards zero) rounding mode.  That includes
    conversion from floating point to integer.

  :samp:`round-nearest`
    This is the mode used for floating-point calculations with
    round-to-nearest-or-even rounding mode.

  :samp:`int`
    This is the mode used to perform integer calculations in the FPU, e.g.
    integer multiply, or integer multiply-and-accumulate.

    The default is :option:`-mfp-mode=caller`

.. option:: -mno-split-lohi, -mno-postinc, -mno-postmodify

  Code generation tweaks that disable, respectively, splitting of 32-bit
  loads, generation of post-increment addresses, and generation of
  post-modify addresses.  The defaults are msplit-lohi,
  :option:`-mpost-inc`, and :option:`-mpost-modify`.

.. option:: -mnovect-double

  Change the preferred SIMD mode to SImode.  The default is
  :option:`-mvect-double`, which uses DImode as preferred SIMD mode.

.. option:: -max-vect-align={num}

  The maximum alignment for SIMD vector mode types.
  :samp:`{num}` may be 4 or 8.  The default is 8.
  Note that this is an ABI change, even though many library function
  interfaces are unaffected if they don't use SIMD vector modes
  in places that affect size and/or alignment of relevant types.

.. option:: -msplit-vecmove-early

  Split vector moves into single word moves before reload.  In theory this
  can give better register allocation, but so far the reverse seems to be
  generally the case.

.. option:: -m1reg-reg

  Specify a register to hold the constant -1, which makes loading small negative
  constants and certain bitmasks faster.
  Allowable values for :samp:`{reg}` are :samp:`r43` and :samp:`r63`,
  which specify use of that register as a fixed register,
  and :samp:`none`, which means that no register is used for this
  purpose.  The default is :option:`-m1reg-none`.