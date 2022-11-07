..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ALLOCATED, allocation, status

.. _allocated:

ALLOCATED --- Status of an allocatable entity
*********************************************

.. function:: ALLOCATED(ARRAY)

  ``ALLOCATED(ARRAY)`` and ``ALLOCATED(SCALAR)`` check the allocation
  status of :samp:`{ARRAY}` and :samp:`{SCALAR}`, respectively.

  :param ARRAY:
    The argument shall be an ``ALLOCATABLE`` array.

  :param SCALAR:
    The argument shall be an ``ALLOCATABLE`` scalar.

  :return:
    The return value is a scalar ``LOGICAL`` with the default logical
    kind type parameter.  If the argument is allocated, then the result is
    ``.TRUE.`` ; otherwise, it returns ``.FALSE.``

  Standard:
    Fortran 90 and later.  Note, the ``SCALAR=`` keyword and allocatable
    scalar entities are available in Fortran 2003 and later.

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = ALLOCATED(ARRAY)
      RESULT = ALLOCATED(SCALAR)

  Example:
    .. code-block:: fortran

      program test_allocated
        integer :: i = 4
        real(4), allocatable :: x(:)
        if (.not. allocated(x)) allocate(x(i))
      end program test_allocated