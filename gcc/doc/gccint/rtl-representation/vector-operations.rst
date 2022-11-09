..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: vector operations

.. _vector-operations:

Vector Operations
*****************

All normal RTL expressions can be used with vector modes; they are
interpreted as operating on each part of the vector independently.
Additionally, there are a few new expressions to describe specific vector
operations.

.. index:: vec_merge

:samp:`(vec_merge:{m} {vec1} {vec2} {items})`
  This describes a merge operation between two vectors.  The result is a vector
  of mode :samp:`{m}` ; its elements are selected from either :samp:`{vec1}` or
  :samp:`{vec2}`.  Which elements are selected is described by :samp:`{items}`, which
  is a bit mask represented by a ``const_int`` ; a zero bit indicates the
  corresponding element in the result vector is taken from :samp:`{vec2}` while
  a set bit indicates it is taken from :samp:`{vec1}`.

  .. index:: vec_select

:samp:`(vec_select:{m} {vec1} {selection})`
  This describes an operation that selects parts of a vector.  :samp:`{vec1}` is
  the source vector, and :samp:`{selection}` is a ``parallel`` that contains a
  ``const_int`` (or another expression, if the selection can be made at
  runtime) for each of the subparts of the result vector, giving the number of
  the source subpart that should be stored into it.  The result mode :samp:`{m}` is
  either the submode for a single element of :samp:`{vec1}` (if only one subpart is
  selected), or another vector mode with that element submode (if multiple
  subparts are selected).

  .. index:: vec_concat

:samp:`(vec_concat:{m} {x1} {x2})`
  Describes a vector concat operation.  The result is a concatenation of the
  vectors or scalars :samp:`{x1}` and :samp:`{x2}` ; its length is the sum of the
  lengths of the two inputs.

  .. index:: vec_duplicate

:samp:`(vec_duplicate:{m} {x})`
  This operation converts a scalar into a vector or a small vector into a
  larger one by duplicating the input values.  The output vector mode must have
  the same submodes as the input vector mode or the scalar modes, and the
  number of output parts must be an integer multiple of the number of input
  parts.

  .. index:: vec_series

:samp:`(vec_series:{m} {base} {step})`
  This operation creates a vector in which element :samp:`{i}` is equal to
  :samp:`{base} + {i}*{step}`.  :samp:`{m}` must be a vector integer mode.
