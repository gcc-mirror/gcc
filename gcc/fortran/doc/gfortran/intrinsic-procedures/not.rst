..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _not:

.. index:: NOT

.. index:: BNOT

.. index:: INOT

.. index:: JNOT

.. index:: KNOT

.. index:: bits, negate

.. index:: bitwise logical not

.. index:: logical not, bitwise

NOT --- Logical negation
************************

.. function:: NOT()

  ``NOT`` returns the bitwise Boolean inverse of :samp:`{I}`.

  :param I:
    The type shall be ``INTEGER``.

  :return:
    The return type is ``INTEGER``, of the same kind as the
    argument.

  Standard:
    Fortran 90 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = NOT(I)

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``NOT(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 95 and later
       * - ``BNOT(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``INOT(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JNOT(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KNOT(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`IAND`,
    :ref:`IEOR`,
    :ref:`IOR`,
    :ref:`IBITS`,
    :ref:`IBSET`,
    :ref:`IBCLR`