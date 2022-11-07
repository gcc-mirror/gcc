..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ibclr:

.. index:: IBCLR

.. index:: BBCLR

.. index:: IIBCLR

.. index:: JIBCLR

.. index:: KIBCLR

.. index:: bits, unset

.. index:: bits, clear

IBCLR --- Clear bit
*******************

.. function:: IBCLR()

  ``IBCLR`` returns the value of :samp:`{I}` with the bit at position
  :samp:`{POS}` set to zero.

  :param I:
    The type shall be ``INTEGER``.

  :param POS:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  Standard:
    Fortran 90 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = IBCLR(I, POS)

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``IBCLR(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 90 and later
       * - ``BBCLR(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IIBCLR(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JIBCLR(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KIBCLR(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`IBITS`,
    :ref:`IBSET`,
    :ref:`IAND`,
    :ref:`IOR`,
    :ref:`IEOR`,
    :ref:`MVBITS`