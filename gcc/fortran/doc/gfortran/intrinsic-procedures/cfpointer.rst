..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c_f_pointer:

C_F_POINTER --- Convert C into Fortran pointer
**********************************************

.. index:: C_F_POINTER, pointer, convert C to Fortran

.. function:: C_F_POINTER(CPTR, FPTR, SHAPE)

  ``C_F_POINTER(CPTR, FPTR[, SHAPE])`` assigns the target of the C pointer
  :samp:`{CPTR}` to the Fortran pointer :samp:`{FPTR}` and specifies its shape.

  :param CPTR:
    scalar of the type ``C_PTR``. It is
    ``INTENT(IN)``.

  :param FPTR:
    pointer interoperable with :samp:`{cptr}`. It is
    ``INTENT(OUT)``.

  :param SHAPE:
    (Optional) Rank-one array of type ``INTEGER``
    with ``INTENT(IN)``. It shall be present
    if and only if :samp:`{fptr}` is an array. The size
    must be equal to the rank of :samp:`{fptr}`.

  Standard:
    Fortran 2003 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL C_F_POINTER(CPTR, FPTR[, SHAPE])

  Example:
    .. code-block:: fortran

      program main
        use iso_c_binding
        implicit none
        interface
          subroutine my_routine(p) bind(c,name='myC_func')
            import :: c_ptr
            type(c_ptr), intent(out) :: p
          end subroutine
        end interface
        type(c_ptr) :: cptr
        real,pointer :: a(:)
        call my_routine(cptr)
        call c_f_pointer(cptr, a, [12])
      end program main

  See also:
    :ref:`C_LOC`,
    :ref:`C_F_PROCPOINTER`
