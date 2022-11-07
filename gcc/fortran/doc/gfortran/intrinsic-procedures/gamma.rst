..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gamma:

.. index:: GAMMA

.. index:: DGAMMA

.. index:: Gamma function

.. index:: Factorial function

GAMMA --- Gamma function
************************

.. function:: GAMMA(X)

  ``GAMMA(X)`` computes Gamma (\Gamma) of :samp:`{X}`. For positive,
  integer values of :samp:`{X}` the Gamma function simplifies to the factorial
  function \Gamma(x)=(x-1)!.

  :param X:
    Shall be of type ``REAL`` and neither zero
    nor a negative integer.

  :return:
    The return value is of type ``REAL`` of the same kind as :samp:`{X}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      X = GAMMA(X)

  Example:
    .. code-block:: fortran

      program test_gamma
        real :: x = 1.0
        x = gamma(x) ! returns 1.0
      end program test_gamma

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DGAMMA(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Logarithm of the Gamma function:
    :ref:`LOG_GAMMA`