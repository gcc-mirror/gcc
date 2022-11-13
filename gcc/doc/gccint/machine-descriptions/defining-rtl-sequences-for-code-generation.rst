..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: expander definitions, code generation RTL sequences, defining RTL sequences for code generation

.. _expander-definitions:

Defining RTL Sequences for Code Generation
******************************************

On some target machines, some standard pattern names for RTL generation
cannot be handled with single insn, but a sequence of RTL insns can
represent them.  For these target machines, you can write a
``define_expand`` to specify how to generate the sequence of RTL.

.. index:: define_expand

A ``define_expand`` is an RTL expression that looks almost like a
``define_insn`` ; but, unlike the latter, a ``define_expand`` is used
only for RTL generation and it can produce more than one RTL insn.

A ``define_expand`` RTX has four operands:

* The name.  Each ``define_expand`` must have a name, since the only
  use for it is to refer to it by name.

* The RTL template.  This is a vector of RTL expressions representing
  a sequence of separate instructions.  Unlike ``define_insn``, there
  is no implicit surrounding ``PARALLEL``.

* The condition, a string containing a C expression.  This expression is
  used to express how the availability of this pattern depends on
  subclasses of target machine, selected by command-line options when GCC
  is run.  This is just like the condition of a ``define_insn`` that
  has a standard name.  Therefore, the condition (if present) may not
  depend on the data in the insn being matched, but only the
  target-machine-type flags.  The compiler needs to test these conditions
  during initialization in order to learn exactly which named instructions
  are available in a particular run.

* The preparation statements, a string containing zero or more C
  statements which are to be executed before RTL code is generated from
  the RTL template.

  Usually these statements prepare temporary registers for use as
  internal operands in the RTL template, but they can also generate RTL
  insns directly by calling routines such as ``emit_insn``, etc.
  Any such insns precede the ones that come from the RTL template.

* Optionally, a vector containing the values of attributes. See :ref:`insn-attributes`.

Every RTL insn emitted by a ``define_expand`` must match some
``define_insn`` in the machine description.  Otherwise, the compiler
will crash when trying to generate code for the insn or trying to optimize
it.

The RTL template, in addition to controlling generation of RTL insns,
also describes the operands that need to be specified when this pattern
is used.  In particular, it gives a predicate for each operand.

A true operand, which needs to be specified in order to generate RTL from
the pattern, should be described with a ``match_operand`` in its first
occurrence in the RTL template.  This enters information on the operand's
predicate into the tables that record such things.  GCC uses the
information to preload the operand into a register if that is required for
valid RTL code.  If the operand is referred to more than once, subsequent
references should use ``match_dup``.

The RTL template may also refer to internal 'operands' which are
temporary registers or labels used only within the sequence made by the
``define_expand``.  Internal operands are substituted into the RTL
template with ``match_dup``, never with ``match_operand``.  The
values of the internal operands are not passed in as arguments by the
compiler when it requests use of this pattern.  Instead, they are computed
within the pattern, in the preparation statements.  These statements
compute the values and store them into the appropriate elements of
``operands`` so that ``match_dup`` can find them.

There are two special macros defined for use in the preparation statements:
``DONE`` and ``FAIL``.  Use them with a following semicolon,
as a statement.

.. index:: DONE

.. envvar:: DONE

  Use the ``DONE`` macro to end RTL generation for the pattern.  The
  only RTL insns resulting from the pattern on this occasion will be
  those already emitted by explicit calls to ``emit_insn`` within the
  preparation statements; the RTL template will not be generated.

.. envvar:: FAIL

  Make the pattern fail on this occasion.  When a pattern fails, it means
  that the pattern was not truly available.  The calling routines in the
  compiler will try other strategies for code generation using other patterns.

  Failure is currently supported only for binary (addition, multiplication,
  shifting, etc.) and bit-field (``extv``, ``extzv``, and ``insv``)
  operations.

If the preparation falls through (invokes neither ``DONE`` nor
``FAIL``), then the ``define_expand`` acts like a
``define_insn`` in that the RTL template is used to generate the
insn.

The RTL template is not used for matching, only for generating the
initial insn list.  If the preparation statement always invokes
``DONE`` or ``FAIL``, the RTL template may be reduced to a simple
list of operands, such as this example:

.. code-block::

  (define_expand "addsi3"
    [(match_operand:SI 0 "register_operand" "")
     (match_operand:SI 1 "register_operand" "")
     (match_operand:SI 2 "register_operand" "")]
    ""
    "
  {
    handle_add (operands[0], operands[1], operands[2]);
    DONE;
  }")

Here is an example, the definition of left-shift for the SPUR chip:

.. code-block::

  (define_expand "ashlsi3"
    [(set (match_operand:SI 0 "register_operand" "")
          (ashift:SI
            (match_operand:SI 1 "register_operand" "")
            (match_operand:SI 2 "nonmemory_operand" "")))]
    ""
    "
  {
    if (GET_CODE (operands[2]) != CONST_INT
        || (unsigned) INTVAL (operands[2]) > 3)
      FAIL;
  }")

This example uses ``define_expand`` so that it can generate an RTL insn
for shifting when the shift-count is in the supported range of 0 to 3 but
fail in other cases where machine insns aren't available.  When it fails,
the compiler tries another strategy using different patterns (such as, a
library call).

If the compiler were able to handle nontrivial condition-strings in
patterns with names, then it would be possible to use a
``define_insn`` in that case.  Here is another case (zero-extension
on the 68000) which makes more use of the power of ``define_expand`` :

.. code-block::

  (define_expand "zero_extendhisi2"
    [(set (match_operand:SI 0 "general_operand" "")
          (const_int 0))
     (set (strict_low_part
            (subreg:HI
              (match_dup 0)
              0))
          (match_operand:HI 1 "general_operand" ""))]
    ""
    "operands[1] = make_safe_from (operands[1], operands[0]);")

.. index:: make_safe_from

Here two RTL insns are generated, one to clear the entire output operand
and the other to copy the input operand into its low half.  This sequence
is incorrect if the input operand refers to [the old value of] the output
operand, so the preparation statement makes sure this isn't so.  The
function ``make_safe_from`` copies the ``operands[1]`` into a
temporary register if it refers to ``operands[0]``.  It does this
by emitting another RTL insn.

Finally, a third example shows the use of an internal operand.
Zero-extension on the SPUR chip is done by ``and`` -ing the result
against a halfword mask.  But this mask cannot be represented by a
``const_int`` because the constant value is too large to be legitimate
on this machine.  So it must be copied into a register with
``force_reg`` and then the register used in the ``and``.

.. code-block::

  (define_expand "zero_extendhisi2"
    [(set (match_operand:SI 0 "register_operand" "")
          (and:SI (subreg:SI
                    (match_operand:HI 1 "register_operand" "")
                    0)
                  (match_dup 2)))]
    ""
    "operands[2]
       = force_reg (SImode, GEN_INT (65535)); ")

.. note::

  If the ``define_expand`` is used to serve a
  standard binary or unary arithmetic operation or a bit-field operation,
  then the last insn it generates must not be a ``code_label``,
  ``barrier`` or ``note``.  It must be an ``insn``,
  ``jump_insn`` or ``call_insn``.  If you don't need a real insn
  at the end, emit an insn to copy the result of the operation into
  itself.  Such an insn will generate no code, but it can avoid problems
  in the compiler.