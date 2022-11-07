..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FREE, pointer, cray

.. _free:

FREE --- Frees memory
*********************

.. function:: FREE(PTR)

  Frees memory previously allocated by ``MALLOC``. The ``FREE``
  intrinsic is an extension intended to be used with Cray pointers, and is
  provided in GNU Fortran to allow user to compile legacy code. For
  new code using Fortran 95 pointers, the memory de-allocation intrinsic is
  ``DEALLOCATE``.

  :param PTR:
    The type shall be ``INTEGER``. It represents the
    location of the memory that should be de-allocated.

  :return:
    None

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL FREE(PTR)

  Example:
    See ``MALLOC`` for an example.

  See also:
    :ref:`MALLOC`