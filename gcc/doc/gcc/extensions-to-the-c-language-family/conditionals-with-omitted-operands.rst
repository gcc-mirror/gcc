..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: conditional expressions, extensions, omitted middle-operands, middle-operands, omitted, extensions, ?:, ?: extensions

.. _conditionals:

Conditionals with Omitted Operands
**********************************

The middle operand in a conditional expression may be omitted.  Then
if the first operand is nonzero, its value is the value of the conditional
expression.

Therefore, the expression

.. code-block:: c++

  x ? : y

has the value of ``x`` if that is nonzero; otherwise, the value of
``y``.

This example is perfectly equivalent to

.. code-block:: c++

  x ? x : y

.. index:: side effect in ?:, ?: side effect

In this simple case, the ability to omit the middle operand is not
especially useful.  When it becomes useful is when the first operand does,
or may (if it is a macro argument), contain a side effect.  Then repeating
the operand in the middle would perform the side effect twice.  Omitting
the middle operand uses the value already computed without the undesirable
effects of recomputing it.
