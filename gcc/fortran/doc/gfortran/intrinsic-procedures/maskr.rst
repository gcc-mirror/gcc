..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MASKR, mask, right justified

.. _maskr:

MASKR --- Right justified mask
******************************

.. function:: MASKR(I, KIND)

  ``MASKL(I[, KIND])`` has its rightmost :samp:`{I}` bits set to 1, and the
  remaining bits set to 0.

  :param I:
    Shall be of type ``INTEGER``.

  :param KIND:
    Shall be a scalar constant expression of type
    ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``. If :samp:`{KIND}` is present, it
    specifies the kind value of the return type; otherwise, it is of the
    default integer kind.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = MASKR(I[, KIND])

  See also:
    :ref:`MASKL`