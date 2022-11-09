..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _iand:

.. index:: IAND

.. index:: BIAND

.. index:: IIAND

.. index:: JIAND

.. index:: KIAND

.. index:: bitwise logical and

.. index:: logical and, bitwise

IAND --- Bitwise logical and
****************************

.. function:: IAND(I, J)

  Bitwise logical ``AND``.

  :param I:
    The type shall be ``INTEGER`` or a boz-literal-constant.

  :param J:
    The type shall be ``INTEGER`` with the same
    kind type parameter as :samp:`{I}` or a boz-literal-constant.
    :samp:`{I}` and :samp:`{J}` shall not both be boz-literal-constants.

  :return:
    The return type is ``INTEGER`` with the kind type parameter of the
    arguments.
    A boz-literal-constant is converted to an ``INTEGER`` with the kind
    type parameter of the other argument as-if a call to :ref:`INT` occurred.

  Standard:
    Fortran 90 and later, with boz-literal-constant Fortran 2008 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = IAND(I, J)

  Example:
    .. code-block:: fortran

      PROGRAM test_iand
        INTEGER :: a, b
        DATA a / Z'F' /, b / Z'3' /
        WRITE (*,*) IAND(a, b)
      END PROGRAM

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``IAND(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 90 and later
       * - ``BIAND(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IIAND(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JIAND(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KIAND(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`IOR`,
    :ref:`IEOR`,
    :ref:`IBITS`,
    :ref:`IBSET`,
    :ref:`IBCLR`,
    :ref:`NOT`
