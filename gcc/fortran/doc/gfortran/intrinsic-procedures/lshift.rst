..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _lshift:

LSHIFT --- Left shift bits
**************************

.. index:: LSHIFT, bits, shift left

.. function:: LSHIFT(I, SHIFT)

  ``LSHIFT`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted left by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the left end are
  lost; zeros are shifted in from the opposite end.

  :param I:
    The type shall be ``INTEGER``.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = LSHIFT(I, SHIFT)

  See also:
    :ref:`ISHFT`,
    :ref:`ISHFTC`,
    :ref:`RSHIFT`,
    :ref:`SHIFTA`,
    :ref:`SHIFTL`,
    :ref:`SHIFTR`