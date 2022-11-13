..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: peephole optimizer definitions, defining peephole optimizers

.. _peephole-definitions:

Machine-Specific Peephole Optimizers
************************************

In addition to instruction patterns the :samp:`md` file may contain
definitions of machine-specific peephole optimizations.

The combiner does not notice certain peephole optimizations when the data
flow in the program does not suggest that it should try them.  For example,
sometimes two consecutive insns related in purpose can be combined even
though the second one does not appear to use a register computed in the
first one.  A machine-specific peephole optimizer can detect such
opportunities.

There are two forms of peephole definitions that may be used.  The
original ``define_peephole`` is run at assembly output time to
match insns and substitute assembly text.  Use of ``define_peephole``
is deprecated.

A newer ``define_peephole2`` matches insns and substitutes new
insns.  The ``peephole2`` pass is run after register allocation
but before scheduling, which may result in much better code for
targets that do scheduling.

.. toctree::
  :maxdepth: 2


.. index:: define_peephole

.. _define_peephole:

RTL to Text Peephole Optimizers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A definition looks like this:

.. code-block::

  (define_peephole
    [insn-pattern-1
     insn-pattern-2
     ...]
    "condition"
    "template"
    "optional-insn-attributes")

The last string operand may be omitted if you are not using any
machine-specific information in this machine description.  If present,
it must obey the same rules as in a ``define_insn``.

In this skeleton, :samp:`{insn-pattern-1}` and so on are patterns to match
consecutive insns.  The optimization applies to a sequence of insns when
:samp:`{insn-pattern-1}` matches the first one, :samp:`{insn-pattern-2}` matches
the next, and so on.

Each of the insns matched by a peephole must also match a
``define_insn``.  Peepholes are checked only at the last stage just
before code generation, and only optionally.  Therefore, any insn which
would match a peephole but no ``define_insn`` will cause a crash in code
generation in an unoptimized compilation, or at various optimization
stages.

The operands of the insns are matched with ``match_operands``,
``match_operator``, and ``match_dup``, as usual.  What is not
usual is that the operand numbers apply to all the insn patterns in the
definition.  So, you can check for identical operands in two insns by
using ``match_operand`` in one insn and ``match_dup`` in the
other.

The operand constraints used in ``match_operand`` patterns do not have
any direct effect on the applicability of the peephole, but they will
be validated afterward, so make sure your constraints are general enough
to apply whenever the peephole matches.  If the peephole matches
but the constraints are not satisfied, the compiler will crash.

It is safe to omit constraints in all the operands of the peephole; or
you can write constraints which serve as a double-check on the criteria
previously tested.

Once a sequence of insns matches the patterns, the :samp:`{condition}` is
checked.  This is a C expression which makes the final decision whether to
perform the optimization (we do so if the expression is nonzero).  If
:samp:`{condition}` is omitted (in other words, the string is empty) then the
optimization is applied to every sequence of insns that matches the
patterns.

The defined peephole optimizations are applied after register allocation
is complete.  Therefore, the peephole definition can check which
operands have ended up in which kinds of registers, just by looking at
the operands.

.. index:: prev_active_insn

The way to refer to the operands in :samp:`{condition}` is to write
``operands[i]`` for operand number :samp:`{i}` (as matched by
``(match_operand i ...)``).  Use the variable ``insn``
to refer to the last of the insns being matched; use
``prev_active_insn`` to find the preceding insns.

.. index:: dead_or_set_p

When optimizing computations with intermediate results, you can use
:samp:`{condition}` to match only when the intermediate results are not used
elsewhere.  Use the C expression ``dead_or_set_p (insn,
op)``, where :samp:`{insn}` is the insn in which you expect the value
to be used for the last time (from the value of ``insn``, together
with use of ``prev_nonnote_insn``), and :samp:`{op}` is the intermediate
value (from ``operands[i]``).

Applying the optimization means replacing the sequence of insns with one
new insn.  The :samp:`{template}` controls ultimate output of assembler code
for this combined insn.  It works exactly like the template of a
``define_insn``.  Operand numbers in this template are the same ones
used in matching the original sequence of insns.

The result of a defined peephole optimizer does not need to match any of
the insn patterns in the machine description; it does not even have an
opportunity to match them.  The peephole optimizer definition itself serves
as the insn pattern to control how the insn is output.

Defined peephole optimizers are run as assembler code is being output,
so the insns they produce are never combined or rearranged in any way.

Here is an example, taken from the 68000 machine description:

.. code-block::

  (define_peephole
    [(set (reg:SI 15) (plus:SI (reg:SI 15) (const_int 4)))
     (set (match_operand:DF 0 "register_operand" "=f")
          (match_operand:DF 1 "register_operand" "ad"))]
    "FP_REG_P (operands[0]) && ! FP_REG_P (operands[1])"
  {
    rtx xoperands[2];
    xoperands[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  #ifdef MOTOROLA
    output_asm_insn ("move.l %1,(sp)", xoperands);
    output_asm_insn ("move.l %1,-(sp)", operands);
    return "fmove.d (sp)+,%0";
  #else
    output_asm_insn ("movel %1,sp@", xoperands);
    output_asm_insn ("movel %1,sp@-", operands);
    return "fmoved sp@+,%0";
  #endif
  })

The effect of this optimization is to change

.. code-block::

  jbsr _foobar
  addql #4,sp
  movel d1,sp@-
  movel d0,sp@-
  fmoved sp@+,fp0

into

.. code-block::

  jbsr _foobar
  movel d1,sp@
  movel d0,sp@-
  fmoved sp@+,fp0

If a peephole matches a sequence including one or more jump insns, you must
take account of the flags such as ``CC_REVERSED`` which specify that the
condition codes are represented in an unusual manner.  The compiler
automatically alters any ordinary conditional jumps which occur in such
situations, but the compiler cannot alter jumps which have been replaced by
peephole optimizations.  So it is up to you to alter the assembler code
that the peephole produces.  Supply C code to write the assembler output,
and in this C code check the condition code status flags and change the
assembler code as appropriate.
:samp:`{insn-pattern-1}` and so on look *almost* like the second
operand of ``define_insn``.  There is one important difference: the
second operand of ``define_insn`` consists of one or more RTX's
enclosed in square brackets.  Usually, there is only one: then the same
action can be written as an element of a ``define_peephole``.  But
when there are multiple actions in a ``define_insn``, they are
implicitly enclosed in a ``parallel``.  Then you must explicitly
write the ``parallel``, and the square brackets within it, in the
``define_peephole``.  Thus, if an insn pattern looks like this,

.. code-block::

  (define_insn "divmodsi4"
    [(set (match_operand:SI 0 "general_operand" "=d")
          (div:SI (match_operand:SI 1 "general_operand" "0")
                  (match_operand:SI 2 "general_operand" "dmsK")))
     (set (match_operand:SI 3 "general_operand" "=d")
          (mod:SI (match_dup 1) (match_dup 2)))]
    "TARGET_68020"
    "divsl%.l %2,%3:%0")

then the way to mention this insn in a peephole is as follows:

.. code-block::

  (define_peephole
    [...
     (parallel
      [(set (match_operand:SI 0 "general_operand" "=d")
            (div:SI (match_operand:SI 1 "general_operand" "0")
                    (match_operand:SI 2 "general_operand" "dmsK")))
       (set (match_operand:SI 3 "general_operand" "=d")
            (mod:SI (match_dup 1) (match_dup 2)))])
     ...]
    ...)

.. index:: define_peephole2

.. _define_peephole2:

RTL to RTL Peephole Optimizers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``define_peephole2`` definition tells the compiler how to
substitute one sequence of instructions for another sequence,
what additional scratch registers may be needed and what their
lifetimes must be.

.. code-block::

  (define_peephole2
    [insn-pattern-1
     insn-pattern-2
     ...]
    "condition"
    [new-insn-pattern-1
     new-insn-pattern-2
     ...]
    "preparation-statements")

The definition is almost identical to ``define_split``
(see :ref:`insn-splitting`) except that the pattern to match is not a
single instruction, but a sequence of instructions.

It is possible to request additional scratch registers for use in the
output template.  If appropriate registers are not free, the pattern
will simply not match.

.. index:: match_scratch, match_dup

Scratch registers are requested with a ``match_scratch`` pattern at
the top level of the input pattern.  The allocated register (initially) will
be dead at the point requested within the original sequence.  If the scratch
is used at more than a single point, a ``match_dup`` pattern at the
top level of the input pattern marks the last position in the input sequence
at which the register must be available.

Here is an example from the IA-32 machine description:

.. code-block::

  (define_peephole2
    [(match_scratch:SI 2 "r")
     (parallel [(set (match_operand:SI 0 "register_operand" "")
                     (match_operator:SI 3 "arith_or_logical_operator"
                       [(match_dup 0)
                        (match_operand:SI 1 "memory_operand" "")]))
                (clobber (reg:CC 17))])]
    "! optimize_size && ! TARGET_READ_MODIFY"
    [(set (match_dup 2) (match_dup 1))
     (parallel [(set (match_dup 0)
                     (match_op_dup 3 [(match_dup 0) (match_dup 2)]))
                (clobber (reg:CC 17))])]
    "")

This pattern tries to split a load from its use in the hopes that we'll be
able to schedule around the memory load latency.  It allocates a single
``SImode`` register of class ``GENERAL_REGS`` (``"r"``) that needs
to be live only at the point just before the arithmetic.

A real example requiring extended scratch lifetimes is harder to come by,
so here's a silly made-up example:

.. code-block::

  (define_peephole2
    [(match_scratch:SI 4 "r")
     (set (match_operand:SI 0 "" "") (match_operand:SI 1 "" ""))
     (set (match_operand:SI 2 "" "") (match_dup 1))
     (match_dup 4)
     (set (match_operand:SI 3 "" "") (match_dup 1))]
    "/* determine 1 does not overlap 0 and 2 */"
    [(set (match_dup 4) (match_dup 1))
     (set (match_dup 0) (match_dup 4))
     (set (match_dup 2) (match_dup 4))
     (set (match_dup 3) (match_dup 4))]
    "")

There are two special macros defined for use in the preparation statements:
``DONE`` and ``FAIL``.  Use them with a following semicolon,
as a statement.

.. index:: DONE

.. envvar:: DONE

  Use the ``DONE`` macro to end RTL generation for the peephole.  The
  only RTL insns generated as replacement for the matched input insn will
  be those already emitted by explicit calls to ``emit_insn`` within
  the preparation statements; the replacement pattern is not used.

.. envvar:: FAIL

  Make the ``define_peephole2`` fail on this occasion.  When a ``define_peephole2``
  fails, it means that the replacement was not truly available for the
  particular inputs it was given.  In that case, GCC may still apply a
  later ``define_peephole2`` that also matches the given insn pattern.
  (Note that this is different from ``define_split``, where ``FAIL``
  prevents the input insn from being split at all.)

If the preparation falls through (invokes neither ``DONE`` nor
``FAIL``), then the ``define_peephole2`` uses the replacement
template.

If we had not added the ``(match_dup 4)`` in the middle of the input
sequence, it might have been the case that the register we chose at the
beginning of the sequence is killed by the first or second ``set``.