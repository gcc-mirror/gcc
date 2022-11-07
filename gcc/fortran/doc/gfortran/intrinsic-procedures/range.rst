..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RANGE, model representation, range

.. _range:

RANGE --- Decimal exponent range
********************************

.. function:: RANGE(X)

  ``RANGE(X)`` returns the decimal exponent range in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``INTEGER``, ``REAL``
    or ``COMPLEX``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = RANGE(X)

  Example:
    See ``PRECISION`` for an example.

  See also:
    :ref:`SELECTED_REAL_KIND`,
    :ref:`PRECISION`