..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: accessors, access to operands, operand access, XEXP, XINT, XWINT, XSTR

.. _accessors:

Access to Operands
******************

Operands of expressions are accessed using the macros ``XEXP``,
``XINT``, ``XWINT`` and ``XSTR``.  Each of these macros takes
two arguments: an expression-pointer (RTX) and an operand number
(counting from zero).  Thus,

.. code-block:: c++

  XEXP (x, 2)

accesses operand 2 of expression :samp:`{x}`, as an expression.

.. code-block:: c++

  XINT (x, 2)

accesses the same operand as an integer.  ``XSTR``, used in the same
fashion, would access it as a string.

Any operand can be accessed as an integer, as an expression or as a string.
You must choose the correct method of access for the kind of value actually
stored in the operand.  You would do this based on the expression code of
the containing expression.  That is also how you would know how many
operands there are.

For example, if :samp:`{x}` is an ``int_list`` expression, you know that it has
two operands which can be correctly accessed as ``XINT (x, 0)``
and ``XEXP (x, 1)``.  Incorrect accesses like
``XEXP (x, 0)`` and ``XINT (x, 1)`` would compile,
but would trigger an internal compiler error when rtl checking is enabled.
Nothing stops you from writing ``XEXP (x, 28)`` either, but
this will access memory past the end of the expression with
unpredictable results.

Access to operands which are vectors is more complicated.  You can use the
macro ``XVEC`` to get the vector-pointer itself, or the macros
``XVECEXP`` and ``XVECLEN`` to access the elements and length of a
vector.

.. index:: XVEC

:samp:`XVEC ({exp}, {idx})`
  Access the vector-pointer which is operand number :samp:`{idx}` in :samp:`{exp}`.

  .. index:: XVECLEN

:samp:`XVECLEN ({exp}, {idx})`
  Access the length (number of elements) in the vector which is
  in operand number :samp:`{idx}` in :samp:`{exp}`.  This value is an ``int``.

  .. index:: XVECEXP

:samp:`XVECEXP ({exp}, {idx}, {eltnum})`
  Access element number :samp:`{eltnum}` in the vector which is
  in operand number :samp:`{idx}` in :samp:`{exp}`.  This value is an RTX.

  It is up to you to make sure that :samp:`{eltnum}` is not negative
  and is less than ``XVECLEN (exp, idx)``.

All the macros defined in this section expand into lvalues and therefore
can be used to assign the operands, lengths and vector elements as well as
to access them.
