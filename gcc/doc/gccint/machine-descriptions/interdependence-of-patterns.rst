..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Dependent Patterns, Interdependence of Patterns

.. _dependent-patterns:

Interdependence of Patterns
***************************

In some cases machines support instructions identical except for the
machine mode of one or more operands.  For example, there may be
'sign-extend halfword' and 'sign-extend byte' instructions whose
patterns are

.. code-block:: c++

  (set (match_operand:SI 0 ...)
       (extend:SI (match_operand:HI 1 ...)))

  (set (match_operand:SI 0 ...)
       (extend:SI (match_operand:QI 1 ...)))

Constant integers do not specify a machine mode, so an instruction to
extend a constant value could match either pattern.  The pattern it
actually will match is the one that appears first in the file.  For correct
results, this must be the one for the widest possible mode (``HImode``,
here).  If the pattern matches the ``QImode`` instruction, the results
will be incorrect if the constant value does not actually fit that mode.

Such instructions to extend constants are rarely generated because they are
optimized away, but they do occasionally happen in nonoptimized
compilations.

If a constraint in a pattern allows a constant, the reload pass may
replace a register with a constant permitted by the constraint in some
cases.  Similarly for memory references.  Because of this substitution,
you should not provide separate patterns for increment and decrement
instructions.  Instead, they should be generated from the same pattern
that supports register-register add insns by examining the operands and
generating the appropriate machine instruction.
