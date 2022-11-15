..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TINY, limits, smallest number, model representation, smallest number

.. _tiny:

TINY --- Smallest positive number of a real kind
************************************************

.. function:: TINY(X)

  ``TINY(X)`` returns the smallest positive (non zero) number
  in the model of the type of ``X``.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = TINY(X)

  Example:
    See ``HUGE`` for an example.
