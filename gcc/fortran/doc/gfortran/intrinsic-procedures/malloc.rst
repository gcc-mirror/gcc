..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MALLOC, pointer, cray

.. _malloc:

MALLOC --- Allocate dynamic memory
**********************************

.. function:: MALLOC(SIZE)

  ``MALLOC(SIZE)`` allocates :samp:`{SIZE}` bytes of dynamic memory and
  returns the address of the allocated memory. The ``MALLOC`` intrinsic
  is an extension intended to be used with Cray pointers, and is provided
  in GNU Fortran to allow the user to compile legacy code. For new code
  using Fortran 95 pointers, the memory allocation intrinsic is
  ``ALLOCATE``.

  :param SIZE:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER(K)``, with :samp:`{K}` such that
    variables of type ``INTEGER(K)`` have the same size as
    C pointers (``sizeof(void *)``).

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      PTR = MALLOC(SIZE)

  Example:
    The following example demonstrates the use of ``MALLOC`` and
    ``FREE`` with Cray pointers.

    .. code-block:: fortran

      program test_malloc
        implicit none
        integer i
        real*8 x(*), z
        pointer(ptr_x,x)

        ptr_x = malloc(20*8)
        do i = 1, 20
          x(i) = sqrt(1.0d0 / i)
        end do
        z = 0
        do i = 1, 20
          z = z + x(i)
          print *, z
        end do
        call free(ptr_x)
      end program test_malloc

  See also:
    :ref:`FREE`