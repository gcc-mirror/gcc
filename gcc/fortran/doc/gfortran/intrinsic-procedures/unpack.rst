..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: UNPACK, array, unpacking, array, increase dimension, array, scatter elements

.. _unpack:

UNPACK --- Unpack an array of rank one into an array
****************************************************

.. function:: UNPACK(VECTOR, MASK, FIELD)

  Store the elements of :samp:`{VECTOR}` in an array of higher rank.

  :param VECTOR:
    Shall be an array of any type and rank one. It
    shall have at least as many elements as :samp:`{MASK}` has ``TRUE`` values.

  :param MASK:
    Shall be an array of type ``LOGICAL``.

  :param FIELD:
    Shall be of the same type as :samp:`{VECTOR}` and have
    the same shape as :samp:`{MASK}`.

  :return:
    The resulting array corresponds to :samp:`{FIELD}` with ``TRUE`` elements
    of :samp:`{MASK}` replaced by values from :samp:`{VECTOR}` in array element order.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = UNPACK(VECTOR, MASK, FIELD)

  Example:
    .. code-block:: fortran

      PROGRAM test_unpack
        integer :: vector(2)  = (/1,1/)
        logical :: mask(4)  = (/ .TRUE., .FALSE., .FALSE., .TRUE. /)
        integer :: field(2,2) = 0, unity(2,2)

        ! result: unity matrix
        unity = unpack(vector, reshape(mask, (/2,2/)), field)
      END PROGRAM

  See also:
    :ref:`PACK`,
    :ref:`SPREAD`
