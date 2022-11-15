..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MATMUL, matrix multiplication, product, matrix

.. _matmul:

MATMUL --- matrix multiplication
********************************

.. function:: MATMUL(MATRIX_A, MATRIX_B)

  Performs a matrix multiplication on numeric or logical arguments.

  :param MATRIX_A:
    An array of ``INTEGER``,
    ``REAL``, ``COMPLEX``, or ``LOGICAL`` type, with a rank of
    one or two.

  :param MATRIX_B:
    An array of ``INTEGER``,
    ``REAL``, or ``COMPLEX`` type if :samp:`{MATRIX_A}` is of a numeric
    type; otherwise, an array of ``LOGICAL`` type. The rank shall be one
    or two, and the first (or only) dimension of :samp:`{MATRIX_B}` shall be
    equal to the last (or only) dimension of :samp:`{MATRIX_A}`.
    :samp:`{MATRIX_A}` and :samp:`{MATRIX_B}` shall not both be rank one arrays.

  :return:
    The matrix product of :samp:`{MATRIX_A}` and :samp:`{MATRIX_B}`.  The type and
    kind of the result follow the usual type and kind promotion rules, as
    for the ``*`` or ``.AND.`` operators.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = MATMUL(MATRIX_A, MATRIX_B)
