..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _dim:

.. index:: DIM

.. index:: IDIM

.. index:: DDIM

.. index:: positive difference

DIM --- Positive difference
***************************

.. function:: DIM(X,Y)

  ``DIM(X,Y)`` returns the difference ``X-Y`` if the result is positive;
  otherwise returns zero.

  :param X:
    The type shall be ``INTEGER`` or ``REAL``

  :param Y:
    The type shall be the same type and kind as :samp:`{X}`.  (As
    a GNU extension, arguments of different kinds are permitted.)

  :return:
    The return value is of type ``INTEGER`` or ``REAL``.  (As a GNU
    extension, kind is the largest kind of the actual arguments.)

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = DIM(X, Y)

  Example:
    .. code-block:: fortran

      program test_dim
          integer :: i
          real(8) :: x
          i = dim(4, 15)
          x = dim(4.345_8, 2.111_8)
          print *, i
          print *, x
      end program test_dim

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DIM(X,Y)``
         - ``REAL(4) X, Y``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``IDIM(X,Y)``
         - ``INTEGER(4) X, Y``
         - ``INTEGER(4)``
         - Fortran 77 and later
       * - ``DDIM(X,Y)``
         - ``REAL(8) X, Y``
         - ``REAL(8)``
         - Fortran 77 and later