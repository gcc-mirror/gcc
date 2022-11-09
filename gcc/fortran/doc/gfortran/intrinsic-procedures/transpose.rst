..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TRANSPOSE, array, transpose, matrix, transpose, transpose

.. _transpose:

TRANSPOSE --- Transpose an array of rank two
********************************************

.. function:: TRANSPOSE(MATRIX)

  Transpose an array of rank two. Element (i, j) of the result has the value
  ``MATRIX(j, i)``, for all i, j.

  :param MATRIX:
    Shall be an array of any type and have a rank of two.

  :return:
    The result has the same type as :samp:`{MATRIX}`, and has shape
    ``(/ m, n /)`` if :samp:`{MATRIX}` has shape ``(/ n, m /)``.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = TRANSPOSE(MATRIX)
