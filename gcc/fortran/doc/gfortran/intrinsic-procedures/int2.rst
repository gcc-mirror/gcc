..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: INT2, conversion, to integer

.. _int2:

INT2 --- Convert to 16-bit integer type
***************************************

.. function:: INT2(A)

  Convert to a ``KIND=2`` integer type. This is equivalent to the
  standard ``INT`` intrinsic with an optional argument of
  ``KIND=2``, and is only included for backwards compatibility.

  :param A:
    Shall be of type ``INTEGER``,
    ``REAL``, or ``COMPLEX``.

  :return:
    The return value is a ``INTEGER(2)`` variable.

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = INT2(A)

  See also:
    :ref:`INT`,
    :ref:`INT8`
