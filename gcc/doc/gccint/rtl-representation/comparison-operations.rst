..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL comparison operations

.. _comparisons:

Comparison Operations
*********************

Comparison operators test a relation on two operands and are considered
to represent a machine-dependent nonzero value described by, but not
necessarily equal to, ``STORE_FLAG_VALUE`` (see :ref:`misc`)
if the relation holds, or zero if it does not, for comparison operators
whose results have a 'MODE_INT' mode,
``FLOAT_STORE_FLAG_VALUE`` (see :ref:`misc`) if the relation holds, or
zero if it does not, for comparison operators that return floating-point
values, and a vector of either ``VECTOR_STORE_FLAG_VALUE`` (see :ref:`misc`)
if the relation holds, or of zeros if it does not, for comparison operators
that return vector results.
The mode of the comparison operation is independent of the mode
of the data being compared.  If the comparison operation is being tested
(e.g., the first operand of an ``if_then_else``), the mode must be
``VOIDmode``.

.. index:: condition codes

A comparison operation compares two data
objects.  The mode of the comparison is determined by the operands; they
must both be valid for a common machine mode.  A comparison with both
operands constant would be invalid as the machine mode could not be
deduced from it, but such a comparison should never exist in RTL due to
constant folding.

Usually only one style
of comparisons is supported on a particular machine, but the combine
pass will try to merge operations to produce code like
``(eq xy)``,
in case it exists in the context of the particular insn involved.

Inequality comparisons come in two flavors, signed and unsigned.  Thus,
there are distinct expression codes ``gt`` and ``gtu`` for signed and
unsigned greater-than.  These can produce different results for the same
pair of integer values: for example, 1 is signed greater-than -1 but not
unsigned greater-than, because -1 when regarded as unsigned is actually
``0xffffffff`` which is greater than 1.

The signed comparisons are also used for floating point values.  Floating
point comparisons are distinguished by the machine modes of the operands.

.. index:: eq, equal

:samp:`(eq:{m} {x} {y})`
  ``STORE_FLAG_VALUE`` if the values represented by :samp:`{x}` and :samp:`{y}`
  are equal, otherwise 0.

  .. index:: ne, not equal

:samp:`(ne:{m} {x} {y})`
  ``STORE_FLAG_VALUE`` if the values represented by :samp:`{x}` and :samp:`{y}`
  are not equal, otherwise 0.

  .. index:: gt, greater than

:samp:`(gt:{m} {x} {y})`
  ``STORE_FLAG_VALUE`` if the :samp:`{x}` is greater than :samp:`{y}`.  If they
  are fixed-point, the comparison is done in a signed sense.

  .. index:: gtu, greater than, unsigned greater than

:samp:`(gtu:{m} {x} {y})`
  Like ``gt`` but does unsigned comparison, on fixed-point numbers only.

  .. index:: lt, less than, ltu, unsigned less than

:samp:`(lt:{m} {x} {y})` :samp:`(ltu:{m} {x} {y})`
  Like ``gt`` and ``gtu`` but test for 'less than'.

  .. index:: ge, greater than, geu, unsigned greater than

:samp:`(ge:{m} {x} {y})` :samp:`(geu:{m} {x} {y})`
  Like ``gt`` and ``gtu`` but test for 'greater than or equal'.

  .. index:: le, less than or equal, leu, unsigned less than

:samp:`(le:{m} {x} {y})` :samp:`(leu:{m} {x} {y})`
  Like ``gt`` and ``gtu`` but test for 'less than or equal'.

  .. index:: if_then_else

:samp:`(if_then_else {cond} {then} {else})`
  This is not a comparison operation but is listed here because it is
  always used in conjunction with a comparison operation.  To be
  precise, :samp:`{cond}` is a comparison expression.  This expression
  represents a choice, according to :samp:`{cond}`, between the value
  represented by :samp:`{then}` and the one represented by :samp:`{else}`.

  On most machines, ``if_then_else`` expressions are valid only
  to express conditional jumps.

  .. index:: cond

:samp:`(cond [{test1} {value1} {test2} {value2} ...] {default})`
  Similar to ``if_then_else``, but more general.  Each of :samp:`{test1}`,
  :samp:`{test2}`, ... is performed in turn.  The result of this expression is
  the :samp:`{value}` corresponding to the first nonzero test, or :samp:`{default}` if
  none of the tests are nonzero expressions.

  This is currently not valid for instruction patterns and is supported only
  for insn attributes.  See :ref:`insn-attributes`.
