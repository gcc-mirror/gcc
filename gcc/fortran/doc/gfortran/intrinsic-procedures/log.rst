..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _log:

.. index:: LOG

.. index:: ALOG

.. index:: DLOG

.. index:: CLOG

.. index:: ZLOG

.. index:: CDLOG

.. index:: exponential function, inverse

.. index:: logarithm function

.. index:: natural logarithm function

LOG --- Natural logarithm function
**********************************

.. function:: LOG(X)

  ``LOG(X)`` computes the natural logarithm of :samp:`{X}`, i.e. the
  logarithm to the base e.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value is of type ``REAL`` or ``COMPLEX``.
    The kind type parameter is the same as :samp:`{X}`.
    If :samp:`{X}` is ``COMPLEX``, the imaginary part \omega is in the range
    -\pi < \omega \leq \pi.

  Standard:
    Fortran 77 and later, has GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = LOG(X)

  Example:
    .. code-block:: fortran

      program test_log
        real(8) :: x = 2.7182818284590451_8
        complex :: z = (1.0, 2.0)
        x = log(x)    ! will yield (approximately) 1
        z = log(z)
      end program test_log

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ALOG(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 or later
       * - ``DLOG(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 or later
       * - ``CLOG(X)``
         - ``COMPLEX(4) X``
         - ``COMPLEX(4)``
         - Fortran 77 or later
       * - ``ZLOG(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
       * - ``CDLOG(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
