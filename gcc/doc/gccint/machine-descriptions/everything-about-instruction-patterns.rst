..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: patterns, instruction patterns, define_insn

.. _patterns:

Everything about Instruction Patterns
*************************************

A ``define_insn`` expression is used to define instruction patterns
to which insns may be matched.  A ``define_insn`` expression contains
an incomplete RTL expression, with pieces to be filled in later, operand
constraints that restrict how the pieces can be filled in, and an output
template or C code to generate the assembler output.

A ``define_insn`` is an RTL expression containing four or five operands:

* An optional name :samp:`{n}`.  When a name is present, the compiler
  automically generates a C++ function :samp:`gen_{n}` that takes
  the operands of the instruction as arguments and returns the instruction's
  rtx pattern.  The compiler also assigns the instruction a unique code
  :samp:`CODE_FOR_{n}`, with all such codes belonging to an enum
  called ``insn_code``.

  These names serve one of two purposes.  The first is to indicate that the
  instruction performs a certain standard job for the RTL-generation
  pass of the compiler, such as a move, an addition, or a conditional
  jump.  The second is to help the target generate certain target-specific
  operations, such as when implementing target-specific intrinsic functions.

  It is better to prefix target-specific names with the name of the
  target, to avoid any clash with current or future standard names.

  The absence of a name is indicated by writing an empty string
  where the name should go.  Nameless instruction patterns are never
  used for generating RTL code, but they may permit several simpler insns
  to be combined later on.

  For the purpose of debugging the compiler, you may also specify a
  name beginning with the :samp:`*` character.  Such a name is used only
  for identifying the instruction in RTL dumps; it is equivalent to having
  a nameless pattern for all other purposes.  Names beginning with the
  :samp:`*` character are not required to be unique.

  The name may also have the form :samp:`@{n}`.  This has the same
  effect as a name :samp:`{n}`, but in addition tells the compiler to
  generate further helper functions; see :ref:`parameterized-names` for details.

* The :dfn:`RTL template`: This is a vector of incomplete RTL expressions
  which describe the semantics of the instruction (see :ref:`rtl-template`).
  It is incomplete because it may contain ``match_operand``,
  ``match_operator``, and ``match_dup`` expressions that stand for
  operands of the instruction.

  If the vector has multiple elements, the RTL template is treated as a
  ``parallel`` expression.

.. index:: pattern conditions, conditions, in patterns

* The condition: This is a string which contains a C expression.  When the
  compiler attempts to match RTL against a pattern, the condition is
  evaluated.  If the condition evaluates to ``true``, the match is
  permitted.  The condition may be an empty string, which is treated
  as always ``true``.

  .. index:: named patterns and conditions

  For a named pattern, the condition may not depend on the data in the
  insn being matched, but only the target-machine-type flags.  The compiler
  needs to test these conditions during initialization in order to learn
  exactly which named instructions are available in a particular run.

  .. index:: operands

  For nameless patterns, the condition is applied only when matching an
  individual insn, and only after the insn has matched the pattern's
  recognition template.  The insn's operands may be found in the vector
  ``operands``.

  An instruction condition cannot become more restrictive as compilation
  progresses.  If the condition accepts a particular RTL instruction at
  one stage of compilation, it must continue to accept that instruction
  until the final pass.  For example, :samp:`!reload_completed` and
  :samp:`can_create_pseudo_p ()` are both invalid instruction conditions,
  because they are true during the earlier RTL passes and false during
  the later ones.  For the same reason, if a condition accepts an
  instruction before register allocation, it cannot later try to control
  register allocation by excluding certain register or value combinations.

  Although a condition cannot become more restrictive as compilation
  progresses, the condition for a nameless pattern *can* become
  more permissive.  For example, a nameless instruction can require
  :samp:`reload_completed` to be true, in which case it only matches
  after register allocation.

* The :dfn:`output template` or :dfn:`output statement`: This is either
  a string, or a fragment of C code which returns a string.

  When simple substitution isn't general enough, you can specify a piece
  of C code to compute the output.  See :ref:`output-statement`.

* The :dfn:`insn attributes`: This is an optional vector containing the values of
  attributes for insns matching this pattern (see :ref:`insn-attributes`).
