..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _associated:

ASSOCIATED --- Status of a pointer or pointer/target pair
**********************************************************

.. index:: ASSOCIATED, pointer, status, association status

.. function:: ASSOCIATED(POINTER, TARGET)

  ``ASSOCIATED(POINTER [, TARGET])`` determines the status of the pointer
  :samp:`{POINTER}` or if :samp:`{POINTER}` is associated with the target :samp:`{TARGET}`.

  :param POINTER:
    :samp:`{POINTER}` shall have the ``POINTER`` attribute
    and it can be of any type.

  :param TARGET:
    (Optional) :samp:`{TARGET}` shall be a pointer or
    a target.  It must have the same type, kind type parameter, and
    array rank as :samp:`{POINTER}`.

  :return:
    ``ASSOCIATED(POINTER)`` returns a scalar value of type ``LOGICAL(4)``.
    There are several cases:

    - When the optional TARGET is not present then
      ASSOCIATED(POINTER) is true if POINTER is associated with a target; otherwise, it returns false.

    - If TARGET is present and a scalar target, the result is true if
      TARGET is not a zero-sized storage sequence and the target associated with POINTER occupies the same storage units. If POINTER is disassociated, the result is false.

    - If TARGET is present and an array target, the result is true if
      TARGET and POINTER have the same shape, are not zero-sized arrays, are arrays whose elements are not zero-sized storage sequences,
      and TARGET and POINTER occupy the same storage units in array element order. As in case(B), the result is false, if POINTER is disassociated.

    - If TARGET is present and an scalar pointer, the result is true
      if TARGET is associated with POINTER, the target associated with TARGET are not zero-sized storage sequences and occupy the same storage units.
      The result is false, if either TARGET or POINTER is disassociated.

    - If TARGET is present and an array pointer, the result is true if
      target associated with POINTER and the target associated with TARGET have the same shape, are not zero-sized arrays,
      are arrays whose elements are not zero-sized storage sequences, and TARGET and POINTER occupy the same storage units in array element order.
      The result is false, if either TARGET or POINTER is disassociated.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = ASSOCIATED(POINTER [, TARGET])

  Example:
    .. code-block:: fortran

      program test_associated
         implicit none
         real, target  :: tgt(2) = (/1., 2./)
         real, pointer :: ptr(:)
         ptr => tgt
         if (associated(ptr)     .eqv. .false.) call abort
         if (associated(ptr,tgt) .eqv. .false.) call abort
      end program test_associated

  See also:
    :ref:`NULL`