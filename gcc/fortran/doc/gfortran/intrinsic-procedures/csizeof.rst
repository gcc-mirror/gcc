..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: C_SIZEOF, expression size, size of an expression

.. _c_sizeof:

C_SIZEOF --- Size in bytes of an expression
*******************************************

.. function:: C_SIZEOF(X)

  ``C_SIZEOF(X)`` calculates the number of bytes of storage the
  expression ``X`` occupies.

  :param X:
    The argument shall be an interoperable data entity.

  :return:
    The return value is of type integer and of the system-dependent kind
    ``C_SIZE_T`` (from the ``ISO_C_BINDING`` module). Its value is the
    number of bytes occupied by the argument.  If the argument has the
    ``POINTER`` attribute, the number of bytes of the storage area pointed
    to is returned.  If the argument is of a derived type with ``POINTER``
    or ``ALLOCATABLE`` components, the return value does not account for
    the sizes of the data pointed to by these components.

  Standard:
    Fortran 2008

  Class:
    Inquiry function of the module ``ISO_C_BINDING``

  Syntax:
    .. code-block:: fortran

      N = C_SIZEOF(X)

  Example:
    .. code-block:: fortran

         use iso_c_binding
         integer(c_int) :: i
         real(c_float) :: r, s(5)
         print *, (c_sizeof(s)/c_sizeof(r) == 5)
         end

    The example will print ``T`` unless you are using a platform
    where default ``REAL`` variables are unusually padded.

  See also:
    :ref:`SIZEOF`,
    :ref:`STORAGE_SIZE`