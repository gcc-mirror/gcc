..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _rshift:

RSHIFT --- Right shift bits
***************************

.. index:: RSHIFT, bits, shift right

.. function:: RSHIFT(I, SHIFT)

  ``RSHIFT`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost. The fill is arithmetic: the bits shifted in from the left
  end are equal to the leftmost bit, which in two's complement
  representation is the sign bit.

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

      RESULT = RSHIFT(I, SHIFT)

  See also:
    :ref:`ISHFT`,
    :ref:`ISHFTC`,
    :ref:`LSHIFT`,
    :ref:`SHIFTA`,
    :ref:`SHIFTR`,
    :ref:`SHIFTL`