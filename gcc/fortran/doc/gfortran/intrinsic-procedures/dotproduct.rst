..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DOT_PRODUCT, dot product, vector product, product, vector

.. _dot_product:

DOT_PRODUCT --- Dot product function
************************************

.. function:: DOT_PRODUCT(VECTOR_A, VECTOR_B)

  ``DOT_PRODUCT(VECTOR_A, VECTOR_B)`` computes the dot product multiplication
  of two vectors :samp:`{VECTOR_A}` and :samp:`{VECTOR_B}`.  The two vectors may be
  either numeric or logical and must be arrays of rank one and of equal size. If
  the vectors are ``INTEGER`` or ``REAL``, the result is
  ``SUM(VECTOR_A*VECTOR_B)``. If the vectors are ``COMPLEX``, the result
  is ``SUM(CONJG(VECTOR_A)*VECTOR_B)``. If the vectors are ``LOGICAL``,
  the result is ``ANY(VECTOR_A .AND. VECTOR_B)``.

  :param VECTOR_A:
    The type shall be numeric or ``LOGICAL``, rank 1.

  :param VECTOR_B:
    The type shall be numeric if :samp:`{VECTOR_A}` is of numeric type or ``LOGICAL`` if :samp:`{VECTOR_A}` is of type ``LOGICAL``. :samp:`{VECTOR_B}` shall be a rank-one array.

  :return:
    If the arguments are numeric, the return value is a scalar of numeric type,
    ``INTEGER``, ``REAL``, or ``COMPLEX``.  If the arguments are
    ``LOGICAL``, the return value is ``.TRUE.`` or ``.FALSE.``.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = DOT_PRODUCT(VECTOR_A, VECTOR_B)

  Example:
    .. code-block:: fortran

      program test_dot_prod
          integer, dimension(3) :: a, b
          a = (/ 1, 2, 3 /)
          b = (/ 4, 5, 6 /)
          print '(3i3)', a
          print *
          print '(3i3)', b
          print *
          print *, dot_product(a,b)
      end program test_dot_prod
