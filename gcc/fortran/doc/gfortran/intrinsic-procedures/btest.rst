..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _btest:

.. index:: BTEST

.. index:: BBTEST

.. index:: BITEST

.. index:: BJTEST

.. index:: BKTEST

.. index:: bits, testing

BTEST --- Bit test function
***************************

.. function:: BTEST(I,POS)

  ``BTEST(I,POS)`` returns logical ``.TRUE.`` if the bit at :samp:`{POS}`
  in :samp:`{I}` is set.  The counting of the bits starts at 0.

  :param I:
    The type shall be ``INTEGER``.

  :param POS:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``LOGICAL``

  Standard:
    Fortran 90 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = BTEST(I, POS)

  Example:
    .. code-block:: fortran

      program test_btest
          integer :: i = 32768 + 1024 + 64
          integer :: pos
          logical :: bool
          do pos=0,16
              bool = btest(i, pos)
              print *, pos, bool
          end do
      end program test_btest

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``BTEST(I,POS)``
         - ``INTEGER I,POS``
         - ``LOGICAL``
         - Fortran 95 and later
       * - ``BBTEST(I,POS)``
         - ``INTEGER(1) I,POS``
         - ``LOGICAL(1)``
         - GNU extension
       * - ``BITEST(I,POS)``
         - ``INTEGER(2) I,POS``
         - ``LOGICAL(2)``
         - GNU extension
       * - ``BJTEST(I,POS)``
         - ``INTEGER(4) I,POS``
         - ``LOGICAL(4)``
         - GNU extension
       * - ``BKTEST(I,POS)``
         - ``INTEGER(8) I,POS``
         - ``LOGICAL(8)``
         - GNU extension
