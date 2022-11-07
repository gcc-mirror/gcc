..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _log_gamma:

.. index:: LOG_GAMMA

.. index:: LGAMMA

.. index:: ALGAMA

.. index:: DLGAMA

.. index:: Gamma function, logarithm of

LOG_GAMMA --- Logarithm of the Gamma function
*********************************************

.. function:: LOG_GAMMA(X)

  ``LOG_GAMMA(X)`` computes the natural logarithm of the absolute value
  of the Gamma (\Gamma) function.

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

      X = LOG_GAMMA(X)

  Example:
    .. code-block:: fortran

      program test_log_gamma
        real :: x = 1.0
        x = lgamma(x) ! returns 0.0
      end program test_log_gamma

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``LGAMMA(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``ALGAMA(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DLGAMA(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Gamma function:
    :ref:`GAMMA`