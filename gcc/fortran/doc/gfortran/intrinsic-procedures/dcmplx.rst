..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _dcmplx:

DCMPLX --- Double complex conversion function
*********************************************

.. index:: DCMPLX, complex numbers, conversion to, conversion, to complex

.. function:: DCMPLX(X, Y)

  ``DCMPLX(X [,Y])`` returns a double complex number where :samp:`{X}` is
  converted to the real component.  If :samp:`{Y}` is present it is converted to the
  imaginary component.  If :samp:`{Y}` is not present then the imaginary component is
  set to 0.0.  If :samp:`{X}` is complex then :samp:`{Y}` must not be present.

  :param X:
    The type may be ``INTEGER``, ``REAL``,
    or ``COMPLEX``.

  :param Y:
    (Optional if :samp:`{X}` is not ``COMPLEX``.) May be
    ``INTEGER`` or ``REAL``.

  :return:
    The return value is of type ``COMPLEX(8)``

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = DCMPLX(X [, Y])

  Example:
    .. code-block:: fortran

      program test_dcmplx
          integer :: i = 42
          real :: x = 3.14
          complex :: z
          z = cmplx(i, x)
          print *, dcmplx(i)
          print *, dcmplx(x)
          print *, dcmplx(z)
          print *, dcmplx(x,i)
      end program test_dcmplx
