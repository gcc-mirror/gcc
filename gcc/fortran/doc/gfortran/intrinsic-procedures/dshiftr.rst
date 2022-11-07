..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DSHIFTR, right shift, combined, shift, right

.. _dshiftr:

DSHIFTR --- Combined right shift
********************************

.. function:: DSHIFTR(I, J, SHIFT)

  ``DSHIFTR(I, J, SHIFT)`` combines bits of :samp:`{I}` and :samp:`{J}`. The
  leftmost :samp:`{SHIFT}` bits of the result are the rightmost :samp:`{SHIFT}`
  bits of :samp:`{I}`, and the remaining bits are the leftmost bits of
  :samp:`{J}`.

  :param I:
    Shall be of type ``INTEGER`` or a BOZ constant.

  :param J:
    Shall be of type ``INTEGER`` or a BOZ constant.
    If both :samp:`{I}` and :samp:`{J}` have integer type, then they shall have
    the same kind type parameter. :samp:`{I}` and :samp:`{J}` shall not both be
    BOZ constants.

  :param SHIFT:
    Shall be of type ``INTEGER``. It shall
    be nonnegative.  If :samp:`{I}` is not a BOZ constant, then :samp:`{SHIFT}`
    shall be less than or equal to ``BIT_SIZE(I)`` ; otherwise,
    :samp:`{SHIFT}` shall be less than or equal to ``BIT_SIZE(J)``.

  :return:
    If either :samp:`{I}` or :samp:`{J}` is a BOZ constant, it is first converted
    as if by the intrinsic function ``INT`` to an integer type with the
    kind type parameter of the other.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = DSHIFTR(I, J, SHIFT)

  See also:
    :ref:`DSHIFTL`