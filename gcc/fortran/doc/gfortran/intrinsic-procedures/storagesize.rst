..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: STORAGE_SIZE, storage size

.. _storage_size:

STORAGE_SIZE --- Storage size in bits
*************************************

.. function:: STORAGE_SIZE(A , KIND)

  Returns the storage size of argument :samp:`{A}` in bits.

  :param A:
    Shall be a scalar or array of any type.

  :param KIND:
    (Optional) shall be a scalar integer constant expression.

  Standard:
    Fortran 2008 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = STORAGE_SIZE(A [, KIND])

  Return Value:
    The result is a scalar integer with the kind type parameter specified by KIND
    (or default integer type if KIND is missing). The result value is the size
    expressed in bits for an element of an array that has the dynamic type and type
    parameters of A.

  See also:
    :ref:`C_SIZEOF`,
    :ref:`SIZEOF`
