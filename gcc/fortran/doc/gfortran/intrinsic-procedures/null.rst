..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _null:

NULL --- Function that returns an disassociated pointer
*******************************************************

.. index:: NULL, pointer, status, pointer, disassociated

.. function:: NULL(MOLD)

  Returns a disassociated pointer.

  :param MOLD:
    (Optional) shall be a pointer of any association
    status and of any type.

  :return:
    A disassociated pointer.

  Standard:
    Fortran 95 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      PTR => NULL([MOLD])

  Example:
    .. code-block:: fortran

      REAL, POINTER, DIMENSION(:) :: VEC => NULL ()

  See also:
    :ref:`ASSOCIATED`
