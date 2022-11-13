..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _evaluation-of-logical-expressions:

Evaluation of logical expressions
*********************************

The Fortran standard does not require the compiler to evaluate all parts of an
expression, if they do not contribute to the final result.  For logical
expressions with ``.AND.`` or ``.OR.`` operators, in particular, GNU
Fortran will optimize out function calls (even to impure functions) if the
result of the expression can be established without them.  However, since not
all compilers do that, and such an optimization can potentially modify the
program flow and subsequent results, GNU Fortran throws warnings for such
situations with the :option:`-Wfunction-elimination` flag.