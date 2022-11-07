..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cmplx:

CMPLX --- Complex conversion function
*************************************

.. index:: CMPLX, complex numbers, conversion to, conversion, to complex

.. function:: CMPLX(X, Y, KIND)

  ``CMPLX(X [, Y [, KIND]])`` returns a complex number where :samp:`{X}` is converted to
  the real component.  If :samp:`{Y}` is present it is converted to the imaginary
  component.  If :samp:`{Y}` is not present then the imaginary component is set to
  0.0.  If :samp:`{X}` is complex then :samp:`{Y}` must not be present.

  :param X:
    The type may be ``INTEGER``, ``REAL``,
    or ``COMPLEX``.

  :param Y:
    (Optional; only allowed if :samp:`{X}` is not
    ``COMPLEX``.)  May be ``INTEGER`` or ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of ``COMPLEX`` type, with a kind equal to
    :samp:`{KIND}` if it is specified.  If :samp:`{KIND}` is not specified, the
    result is of the default ``COMPLEX`` kind, regardless of the kinds of
    :samp:`{X}` and :samp:`{Y}`.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = CMPLX(X [, Y [, KIND]])

  Example:
    .. code-block:: fortran

      program test_cmplx
          integer :: i = 42
          real :: x = 3.14
          complex :: z
          z = cmplx(i, x)
          print *, z, cmplx(x)
      end program test_cmplx

  See also:
    :ref:`COMPLEX`