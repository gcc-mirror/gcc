..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: C_LOC, procedure pointer, convert C to Fortran

.. _c_loc:

C_LOC --- Obtain the C address of an object
*******************************************

.. function:: C_LOC(X)

  ``C_LOC(X)`` determines the C address of the argument.

  :param X:
    Shall have either the POINTER or TARGET attribute. It shall not be a coindexed object. It shall either be a variable with interoperable type and kind type parameters, or be a scalar, nonpolymorphic variable with no length type parameters.

  :return:
    The return value is of type ``C_PTR`` and contains the C address
    of the argument.

  Standard:
    Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = C_LOC(X)

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
    :ref:`C_ASSOCIATED`,
    :ref:`C_FUNLOC`,
    :ref:`C_F_POINTER`,
    :ref:`C_F_PROCPOINTER`
