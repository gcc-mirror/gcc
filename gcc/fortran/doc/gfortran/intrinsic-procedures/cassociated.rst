..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c_associated:

C_ASSOCIATED --- Status of a C pointer
**************************************

.. index:: C_ASSOCIATED, association status, C pointer, pointer, C association status

.. function:: C_ASSOCIATED(c_ptr_1, c_ptr_2)

  ``C_ASSOCIATED(c_ptr_1[, c_ptr_2])`` determines the status of the C pointer
  :samp:`{c_ptr_1}` or if :samp:`{c_ptr_1}` is associated with the target :samp:`{c_ptr_2}`.

  :param c_ptr_1:
    Scalar of the type ``C_PTR`` or ``C_FUNPTR``.

  :param c_ptr_2:
    (Optional) Scalar of the same type as :samp:`{c_ptr_1}`.

  :return:
    The return value is of type ``LOGICAL`` ; it is ``.false.`` if either
    :samp:`{c_ptr_1}` is a C NULL pointer or if :samp:`{c_ptr1}` and :samp:`{c_ptr_2}`
    point to different addresses.

  Standard:
    Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = C_ASSOCIATED(c_ptr_1[, c_ptr_2])

  Example:
    .. code-block:: fortran

      subroutine association_test(a,b)
        use iso_c_binding, only: c_associated, c_loc, c_ptr
        implicit none
        real, pointer :: a
        type(c_ptr) :: b
        if(c_associated(b, c_loc(a))) &
           stop 'b and a do not point to same target'
      end subroutine association_test

  See also:
    :ref:`C_LOC`,
    :ref:`C_FUNLOC`