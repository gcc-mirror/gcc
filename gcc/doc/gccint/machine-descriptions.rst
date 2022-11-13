..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: machine descriptions

.. _machine-desc:

Machine Descriptions
--------------------

A machine description has two parts: a file of instruction patterns
(:samp:`.md` file) and a C header file of macro definitions.

The :samp:`.md` file for a target machine contains a pattern for each
instruction that the target machine supports (or at least each instruction
that is worth telling the compiler about).  It may also contain comments.
A semicolon causes the rest of the line to be a comment, unless the semicolon
is inside a quoted string.

See the next chapter for information on the C header file.

.. toctree::
  :maxdepth: 2

  machine-descriptions/overview-of-how-the-machine-description-is-used
  machine-descriptions/everything-about-instruction-patterns
  machine-descriptions/example-of-defineinsn
  machine-descriptions/rtl-template
  machine-descriptions/output-templates-and-operand-substitution
  machine-descriptions/c-statements-for-assembler-output
  machine-descriptions/predicates
  machine-descriptions/operand-constraints
  machine-descriptions/standard-pattern-names-for-generation
  machine-descriptions/when-the-order-of-patterns-matters
  machine-descriptions/interdependence-of-patterns
  machine-descriptions/defining-jump-instruction-patterns
  machine-descriptions/defining-looping-instruction-patterns
  machine-descriptions/canonicalization-of-instructions
  machine-descriptions/defining-rtl-sequences-for-code-generation
  machine-descriptions/defining-how-to-split-instructions
  machine-descriptions/including-patterns-in-machine-descriptions
  machine-descriptions/machine-specific-peephole-optimizers
  machine-descriptions/instruction-attributes
  machine-descriptions/conditional-execution
  machine-descriptions/rtl-templates-transformations
  machine-descriptions/constant-definitions
  machine-descriptions/iterators