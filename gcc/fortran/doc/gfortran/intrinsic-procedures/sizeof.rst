..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SIZEOF, expression size, size of an expression

.. _sizeof:

SIZEOF --- Size in bytes of an expression
*****************************************

.. function:: SIZEOF(X)

  ``SIZEOF(X)`` calculates the number of bytes of storage the
  expression ``X`` occupies.

  :param X:
    The argument shall be of any type, rank or shape.

  :return:
    The return value is of type integer and of the system-dependent kind
    :samp:`{C_SIZE_T}` (from the :samp:`{ISO_C_BINDING}` module). Its value is the
    number of bytes occupied by the argument.  If the argument has the
    ``POINTER`` attribute, the number of bytes of the storage area pointed
    to is returned.  If the argument is of a derived type with ``POINTER``
    or ``ALLOCATABLE`` components, the return value does not account for
    the sizes of the data pointed to by these components. If the argument is
    polymorphic, the size according to the dynamic type is returned. The argument
    may not be a procedure or procedure pointer. Note that the code assumes for
    arrays that those are contiguous; for contiguous arrays, it returns the
    storage or an array element multiplied by the size of the array.

  Standard:
    GNU extension

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      N = SIZEOF(X)

  Example:
    .. code-block:: fortran

         integer :: i
         real :: r, s(5)
         print *, (sizeof(s)/sizeof(r) == 5)
         end

    The example will print ``.TRUE.`` unless you are using a platform
    where default ``REAL`` variables are unusually padded.

  See also:
    :ref:`C_SIZEOF`,
    :ref:`STORAGE_SIZE`