..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MERGE_BITS, bits, merge

.. _merge_bits:

MERGE_BITS --- Merge of bits under mask
***************************************

.. function:: MERGE_BITS(I, J, MASK)

  ``MERGE_BITS(I, J, MASK)`` merges the bits of :samp:`{I}` and :samp:`{J}`
  as determined by the mask.  The i-th bit of the result is equal to the
  i-th bit of :samp:`{I}` if the i-th bit of :samp:`{MASK}` is 1; it is equal to
  the i-th bit of :samp:`{J}` otherwise.

  :param I:
    Shall be of type ``INTEGER`` or a boz-literal-constant.

  :param J:
    Shall be of type ``INTEGER`` with the same
    kind type parameter as :samp:`{I}` or a boz-literal-constant.
    :samp:`{I}` and :samp:`{J}` shall not both be boz-literal-constants.

  :param MASK:
    Shall be of type ``INTEGER`` or a boz-literal-constant
    and of the same kind as :samp:`{I}`.

  :return:
    The result is of the same type and kind as :samp:`{I}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = MERGE_BITS(I, J, MASK)
