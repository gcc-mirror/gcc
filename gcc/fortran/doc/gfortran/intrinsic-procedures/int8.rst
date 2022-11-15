..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: INT8, conversion, to integer

.. _int8:

INT8 --- Convert to 64-bit integer type
***************************************

.. function:: INT8(A)

  Convert to a ``KIND=8`` integer type. This is equivalent to the
  standard ``INT`` intrinsic with an optional argument of
  ``KIND=8``, and is only included for backwards compatibility.

  :param A:
    Shall be of type ``INTEGER``,
    ``REAL``, or ``COMPLEX``.

  :return:
    The return value is a ``INTEGER(8)`` variable.

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = INT8(A)

  See also:
    :ref:`INT`,
    :ref:`INT2`
