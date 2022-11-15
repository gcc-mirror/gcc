..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RANK, rank

.. _rank:

RANK --- Rank of a data object
******************************

.. function:: RANK(A)

  ``RANK(A)`` returns the rank of a scalar or array data object.

  :param A:
    can be of any type

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind. For arrays, their rank is returned; for scalars zero is returned.

  Standard:
    Technical Specification (TS) 29113

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = RANK(A)

  Example:
    .. code-block:: fortran

      program test_rank
        integer :: a
        real, allocatable :: b(:,:)

        print *, rank(a), rank(b) ! Prints:  0  2
      end program test_rank
