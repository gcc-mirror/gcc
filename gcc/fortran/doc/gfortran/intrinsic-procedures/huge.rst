..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: HUGE, limits, largest number, model representation, largest number

.. _huge:

HUGE --- Largest number of a kind
*********************************

.. function:: HUGE(X)

  ``HUGE(X)`` returns the largest number that is not an infinity in
  the model of the type of ``X``.

  :param X:
    Shall be of type ``REAL`` or ``INTEGER``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = HUGE(X)

  Example:
    .. code-block:: fortran

      program test_huge_tiny
        print *, huge(0), huge(0.0), huge(0.0d0)
        print *, tiny(0.0), tiny(0.0d0)
      end program test_huge_tiny
