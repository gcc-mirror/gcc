..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RADIX, model representation, base, model representation, radix

.. _radix:

RADIX --- Base of a model number
********************************

.. function:: RADIX(X)

  ``RADIX(X)`` returns the base of the model representing the entity :samp:`{X}`.

  :param X:
    Shall be of type ``INTEGER`` or ``REAL``

  :return:
    The return value is a scalar of type ``INTEGER`` and of the default
    integer kind.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = RADIX(X)

  Example:
    .. code-block:: fortran

      program test_radix
        print *, "The radix for the default integer kind is", radix(0)
        print *, "The radix for the default real kind is", radix(0.0)
      end program test_radix

  See also:
    :ref:`SELECTED_REAL_KIND`
