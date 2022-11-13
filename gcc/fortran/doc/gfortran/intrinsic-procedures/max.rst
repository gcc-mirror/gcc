..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _max:

.. index:: MAX

.. index:: MAX0

.. index:: AMAX0

.. index:: MAX1

.. index:: AMAX1

.. index:: DMAX1

.. index:: maximum value

MAX --- Maximum value of an argument list
*****************************************

.. function:: MAX(A1, A2 , A3 , ...)

  Returns the argument with the largest (most positive) value.

  :param A1:
    The type shall be ``INTEGER`` or
    ``REAL``.

  :param A2}, {A3}, ...:
    An expression of the same type and kind
    as :samp:`{A1}`.  (As a GNU extension, arguments of different kinds are
    permitted.)

  :return:
    The return value corresponds to the maximum value among the arguments,
    and has the same type and kind as the first argument.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = MAX(A1, A2 [, A3 [, ...]])

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``MAX0(A1)``
         - ``INTEGER(4) A1``
         - ``INTEGER(4)``
         - Fortran 77 and later
       * - ``AMAX0(A1)``
         - ``INTEGER(4) A1``
         - ``REAL(MAX(X))``
         - Fortran 77 and later
       * - ``MAX1(A1)``
         - ``REAL A1``
         - ``INT(MAX(X))``
         - Fortran 77 and later
       * - ``AMAX1(A1)``
         - ``REAL(4) A1``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DMAX1(A1)``
         - ``REAL(8) A1``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    :ref:`MAXLOC`
    :ref:`MAXVAL`,
    :ref:`MIN`
