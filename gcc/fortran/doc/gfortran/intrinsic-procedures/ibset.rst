..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ibset:

.. index:: IBSET

.. index:: BBSET

.. index:: IIBSET

.. index:: JIBSET

.. index:: KIBSET

.. index:: bits, set

IBSET --- Set bit
*****************

.. function:: IBSET()

  ``IBSET`` returns the value of :samp:`{I}` with the bit at position
  :samp:`{POS}` set to one.

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

      RESULT = IBSET(I, POS)

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``IBSET(A)``
         - ``INTEGER A``
         - ``INTEGER``
         - Fortran 90 and later
       * - ``BBSET(A)``
         - ``INTEGER(1) A``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IIBSET(A)``
         - ``INTEGER(2) A``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JIBSET(A)``
         - ``INTEGER(4) A``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KIBSET(A)``
         - ``INTEGER(8) A``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`IBCLR`,
    :ref:`IBITS`,
    :ref:`IAND`,
    :ref:`IOR`,
    :ref:`IEOR`,
    :ref:`MVBITS`