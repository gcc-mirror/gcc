..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _isnan:

ISNAN --- Test for a NaN
************************

.. index:: ISNAN, IEEE, ISNAN

.. function:: ISNAN(X)

  ``ISNAN`` tests whether a floating-point value is an IEEE
  Not-a-Number (NaN).

  :param X:
    Variable of the type ``REAL``.

  :return:
    Returns a default-kind ``LOGICAL``. The returned value is ``TRUE``
    if :samp:`{X}` is a NaN and ``FALSE`` otherwise.

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      ISNAN(X)

  Example:
    .. code-block:: fortran

      program test_nan
        implicit none
        real :: x
        x = -1.0
        x = sqrt(x)
        if (isnan(x)) stop '"x" is a NaN'
      end program test_nan
