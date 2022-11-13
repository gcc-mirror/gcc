..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MOVE_ALLOC, moving allocation, allocation, moving

.. _move_alloc:

MOVE_ALLOC --- Move allocation from one object to another
*********************************************************

.. function:: MOVE_ALLOC(FROM, TO)

  ``MOVE_ALLOC(FROM, TO)`` moves the allocation from :samp:`{FROM}` to
  :samp:`{TO}`.  :samp:`{FROM}` will become deallocated in the process.

  :param FROM:
    ``ALLOCATABLE``, ``INTENT(INOUT)``, may be
    of any type and kind.

  :param TO:
    ``ALLOCATABLE``, ``INTENT(OUT)``, shall be
    of the same type, kind and rank as :samp:`{FROM}`.

  :return:
    None

  Standard:
    Fortran 2003 and later

  Class:
    Pure subroutine

  Syntax:
    .. code-block:: fortran

      CALL MOVE_ALLOC(FROM, TO)

  Example:
    .. code-block:: fortran

      program test_move_alloc
          integer, allocatable :: a(:), b(:)

          allocate(a(3))
          a = [ 1, 2, 3 ]
          call move_alloc(a, b)
          print *, allocated(a), allocated(b)
          print *, b
      end program test_move_alloc