..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _shiftr:

SHIFTR --- Right shift
**********************

.. index:: SHIFTR, bits, shift right, shift, right

.. function:: SHIFTR(I, SHIFT)

  ``SHIFTR`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost, and bits shifted in from the left end are set to 0.

  :param I:
    The type shall be ``INTEGER``.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SHIFTR(I, SHIFT)

  See also:
    :ref:`SHIFTA`,
    :ref:`SHIFTL`