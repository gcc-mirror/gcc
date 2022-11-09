..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: looping instruction patterns, defining looping instruction patterns

.. _looping-patterns:

Defining Looping Instruction Patterns
*************************************

Some machines have special jump instructions that can be utilized to
make loops more efficient.  A common example is the 68000 :samp:`dbra`
instruction which performs a decrement of a register and a branch if the
result was greater than zero.  Other machines, in particular digital
signal processors (DSPs), have special block repeat instructions to
provide low-overhead loop support.  For example, the TI TMS320C3x/C4x
DSPs have a block repeat instruction that loads special registers to
mark the top and end of a loop and to count the number of loop
iterations.  This avoids the need for fetching and executing a
:samp:`dbra`-like instruction and avoids pipeline stalls associated with
the jump.

GCC has two special named patterns to support low overhead looping.
They are :samp:`doloop_begin` and :samp:`doloop_end`.  These are emitted
by the loop optimizer for certain well-behaved loops with a finite
number of loop iterations using information collected during strength
reduction.

The :samp:`doloop_end` pattern describes the actual looping instruction
(or the implicit looping operation) and the :samp:`doloop_begin` pattern
is an optional companion pattern that can be used for initialization
needed for some low-overhead looping instructions.

Note that some machines require the actual looping instruction to be
emitted at the top of the loop (e.g., the TMS320C3x/C4x DSPs).  Emitting
the true RTL for a looping instruction at the top of the loop can cause
problems with flow analysis.  So instead, a dummy ``doloop`` insn is
emitted at the end of the loop.  The machine dependent reorg pass checks
for the presence of this ``doloop`` insn and then searches back to
the top of the loop, where it inserts the true looping insn (provided
there are no instructions in the loop which would cause problems).  Any
additional labels can be emitted at this point.  In addition, if the
desired special iteration counter register was not allocated, this
machine dependent reorg pass could emit a traditional compare and jump
instruction pair.

For the :samp:`doloop_end` pattern, the loop optimizer allocates an
additional pseudo register as an iteration counter.  This pseudo
register cannot be used within the loop (i.e., general induction
variables cannot be derived from it), however, in many cases the loop
induction variable may become redundant and removed by the flow pass.

The :samp:`doloop_end` pattern must have a specific structure to be
handled correctly by GCC.  The example below is taken (slightly
simplified) from the PDP-11 target:

.. code-block::

  (define_expand "doloop_end"
    [(parallel [(set (pc)
                     (if_then_else
                      (ne (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
                (set (match_dup 0)
                     (plus:HI (match_dup 0)
                           (const_int -1)))])]
    ""
    "{
      if (GET_MODE (operands[0]) != HImode)
        FAIL;
    }")

  (define_insn "doloop_end_insn"
    [(set (pc)
          (if_then_else
           (ne (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
               (const_int 1))
           (label_ref (match_operand 1 "" ""))
           (pc)))
     (set (match_dup 0)
          (plus:HI (match_dup 0)
                (const_int -1)))]
    ""

    {
      if (which_alternative == 0)
        return "sob %0,%l1";

      /* emulate sob */
      output_asm_insn ("dec %0", operands);
      return "bne %l1";
    })

The first part of the pattern describes the branch condition.  GCC
supports three cases for the way the target machine handles the loop
counter:

* Loop terminates when the loop register decrements to zero.  This
  is represented by a ``ne`` comparison of the register (its old value)
  with constant 1 (as in the example above).

* Loop terminates when the loop register decrements to -1.
  This is represented by a ``ne`` comparison of the register with
  constant zero.

* Loop terminates when the loop register decrements to a negative
  value.  This is represented by a ``ge`` comparison of the register
  with constant zero.  For this case, GCC will attach a ``REG_NONNEG``
  note to the ``doloop_end`` insn if it can determine that the register
  will be non-negative.

Since the ``doloop_end`` insn is a jump insn that also has an output,
the reload pass does not handle the output operand.  Therefore, the
constraint must allow for that operand to be in memory rather than a
register.  In the example shown above, that is handled (in the
``doloop_end_insn`` pattern) by using a loop instruction sequence
that can handle memory operands when the memory alternative appears.

GCC does not check the mode of the loop register operand when generating
the ``doloop_end`` pattern.  If the pattern is only valid for some
modes but not others, the pattern should be a ``define_expand``
pattern that checks the operand mode in the preparation code, and issues
``FAIL`` if an unsupported mode is found.  The example above does
this, since the machine instruction to be used only exists for
``HImode``.

If the ``doloop_end`` pattern is a ``define_expand``, there must
also be a ``define_insn`` or ``define_insn_and_split`` matching
the generated pattern.  Otherwise, the compiler will fail during loop
optimization.
