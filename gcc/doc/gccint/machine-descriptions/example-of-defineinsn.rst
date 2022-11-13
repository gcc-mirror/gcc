..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: define_insn example

.. _example:

Example of define_insn
**********************

Here is an example of an instruction pattern, taken from the machine
description for the 68000/68020.

.. code-block::

  (define_insn "tstsi"
    [(set (cc0)
          (match_operand:SI 0 "general_operand" "rm"))]
    ""
    "*
  {
    if (TARGET_68020 || ! ADDRESS_REG_P (operands[0]))
      return \"tstl %0\";
    return \"cmpl #0,%0\";
  }")

This can also be written using braced strings:

.. code-block::

  (define_insn "tstsi"
    [(set (cc0)
          (match_operand:SI 0 "general_operand" "rm"))]
    ""
  {
    if (TARGET_68020 || ! ADDRESS_REG_P (operands[0]))
      return "tstl %0";
    return "cmpl #0,%0";
  })

This describes an instruction which sets the condition codes based on the
value of a general operand.  It has no condition, so any insn with an RTL
description of the form shown may be matched to this pattern.  The name
:samp:`tstsi` means 'test a ``SImode`` value' and tells the RTL
generation pass that, when it is necessary to test such a value, an insn
to do so can be constructed using this pattern.

The output control string is a piece of C code which chooses which
output template to return based on the kind of operand and the specific
type of CPU for which code is being generated.

:samp:`"rm"` is an operand constraint.  Its meaning is explained below.