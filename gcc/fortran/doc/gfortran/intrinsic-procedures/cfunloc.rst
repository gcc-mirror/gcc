..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: C_FUNLOC, pointer, C address of procedures

.. _c_funloc:

C_FUNLOC --- Obtain the C address of a procedure
************************************************

.. function:: C_FUNLOC(x)

  ``C_FUNLOC(x)`` determines the C address of the argument.

  :param x:
    Interoperable function or pointer to such function.

  :return:
    The return value is of type ``C_FUNPTR`` and contains the C address
    of the argument.

  Standard:
    Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = C_FUNLOC(x)

  Example:
    .. code-block:: fortran

      module x
        use iso_c_binding
        implicit none
      contains
        subroutine sub(a) bind(c)
          real(c_float) :: a
          a = sqrt(a)+5.0
        end subroutine sub
      end module x
      program main
        use iso_c_binding
        use x
        implicit none
        interface
          subroutine my_routine(p) bind(c,name='myC_func')
            import :: c_funptr
            type(c_funptr), intent(in) :: p
          end subroutine
        end interface
        call my_routine(c_funloc(sub))
      end program main

  See also:
    :ref:`C_ASSOCIATED`,
    :ref:`C_LOC`,
    :ref:`C_F_POINTER`,
    :ref:`C_F_PROCPOINTER`