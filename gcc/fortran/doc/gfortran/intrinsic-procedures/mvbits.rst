..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MVBITS

.. index:: BMVBITS

.. index:: IMVBITS

.. index:: JMVBITS

.. index:: KMVBITS

.. index:: bits, move

.. _mvbits:

MVBITS --- Move bits from one integer to another
************************************************

.. function:: MVBITS(FROM, FROMPOS, LEN, TO, TOPOS)

  Moves :samp:`{LEN}` bits from positions :samp:`{FROMPOS}` through
  ``FROMPOS+LEN-1`` of :samp:`{FROM}` to positions :samp:`{TOPOS}` through
  ``TOPOS+LEN-1`` of :samp:`{TO}`. The portion of argument :samp:`{TO}` not
  affected by the movement of bits is unchanged. The values of
  ``FROMPOS+LEN-1`` and ``TOPOS+LEN-1`` must be less than
  ``BIT_SIZE(FROM)``.

  :param FROM:
    The type shall be ``INTEGER``.

  :param FROMPOS:
    The type shall be ``INTEGER``.

  :param LEN:
    The type shall be ``INTEGER``.

  :param TO:
    The type shall be ``INTEGER``, of the
    same kind as :samp:`{FROM}`.

  :param TOPOS:
    The type shall be ``INTEGER``.

  Standard:
    Fortran 90 and later, has overloads that are GNU extensions

  Class:
    Elemental subroutine

  Syntax:
    .. code-block:: fortran

      CALL MVBITS(FROM, FROMPOS, LEN, TO, TOPOS)

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``MVBITS(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 90 and later
       * - ``BMVBITS(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IMVBITS(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JMVBITS(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KMVBITS(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`IBCLR`,
    :ref:`IBSET`,
    :ref:`IBITS`,
    :ref:`IAND`,
    :ref:`IOR`,
    :ref:`IEOR`
