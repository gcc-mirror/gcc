..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DSHIFTL, left shift, combined, shift, left

.. _dshiftl:

DSHIFTL --- Combined left shift
*******************************

.. function:: DSHIFTL(I, J, SHIFT)

  ``DSHIFTL(I, J, SHIFT)`` combines bits of :samp:`{I}` and :samp:`{J}`. The
  rightmost :samp:`{SHIFT}` bits of the result are the leftmost :samp:`{SHIFT}`
  bits of :samp:`{J}`, and the remaining bits are the rightmost bits of
  :samp:`{I}`.

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

      RESULT = DSHIFTL(I, J, SHIFT)

  See also:
    :ref:`DSHIFTR`