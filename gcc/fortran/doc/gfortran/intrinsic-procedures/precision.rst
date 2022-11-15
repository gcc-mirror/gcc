..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: PRECISION, model representation, precision

.. _precision:

PRECISION --- Decimal precision of a real kind
**********************************************

.. function:: PRECISION(X)

  ``PRECISION(X)`` returns the decimal precision in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``REAL`` or ``COMPLEX``. It may
    be scalar or valued.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = PRECISION(X)

  Example:
    .. code-block:: fortran

      program prec_and_range
        real(kind=4) :: x(2)
        complex(kind=8) :: y

        print *, precision(x), range(x)
        print *, precision(y), range(y)
      end program prec_and_range

  See also:
    :ref:`SELECTED_REAL_KIND`,
    :ref:`RANGE`
