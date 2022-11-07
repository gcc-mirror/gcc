..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _erf:

.. index:: ERF

.. index:: error function

ERF --- Error function
***********************

.. function:: ERF(X)

  ``ERF(X)`` computes the error function of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL``, of the same kind as
    :samp:`{X}` and lies in the range -1 \leq erf (x) \leq 1 .

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ERF(X)

  Example:
    .. code-block:: fortran

      program test_erf
        real(8) :: x = 0.17_8
        x = erf(x)
      end program test_erf

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DERF(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension