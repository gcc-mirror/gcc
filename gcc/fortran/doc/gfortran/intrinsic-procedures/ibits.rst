..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ibits:

.. index:: IBITS

.. index:: BBITS

.. index:: IIBITS

.. index:: JIBITS

.. index:: KIBITS

.. index:: bits, get

.. index:: bits, extract

IBITS --- Bit extraction
************************

.. function:: IBITS()

  ``IBITS`` extracts a field of length :samp:`{LEN}` from :samp:`{I}`,
  starting from bit position :samp:`{POS}` and extending left for :samp:`{LEN}`
  bits.  The result is right-justified and the remaining bits are
  zeroed.  The value of ``POS+LEN`` must be less than or equal to the
  value ``BIT_SIZE(I)``.

  :param I:
    The type shall be ``INTEGER``.

  :param POS:
    The type shall be ``INTEGER``.

  :param LEN:
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

      RESULT = IBITS(I, POS, LEN)

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``IBITS(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 90 and later
       * - ``BBITS(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IIBITS(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JIBITS(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KIBITS(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`BIT_SIZE`,
    :ref:`IBCLR`,
    :ref:`IBSET`,
    :ref:`IAND`,
    :ref:`IOR`,
    :ref:`IEOR`
