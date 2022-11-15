..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SELECTED_REAL_KIND, real kind, kind, real, radix, real

.. _selected_real_kind:

SELECTED_REAL_KIND --- Choose real kind
***************************************

.. function:: SELECTED_REAL_KIND(P,R)

  ``SELECTED_REAL_KIND(P,R)`` returns the kind value of a real data type
  with decimal precision of at least ``P`` digits, exponent range of
  at least ``R``, and with a radix of ``RADIX``.

  :param P:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :param R:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :param RADIX:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :return:
    ``SELECTED_REAL_KIND`` returns the value of the kind type parameter of
    a real data type with decimal precision of at least ``P`` digits, a
    decimal exponent range of at least ``R``, and with the requested
    ``RADIX``. If the ``RADIX`` parameter is absent, real kinds with
    any radix can be returned. If more than one real data type meet the
    criteria, the kind of the data type with the smallest decimal precision
    is returned. If no real data type matches the criteria, the result is

  Standard:
    Fortran 90 and later, with ``RADIX`` Fortran 2008 or later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = SELECTED_REAL_KIND([P, R, RADIX])

  Example:
    .. code-block:: fortran

      program real_kinds
        integer,parameter :: p6 = selected_real_kind(6)
        integer,parameter :: p10r100 = selected_real_kind(10,100)
        integer,parameter :: r400 = selected_real_kind(r=400)
        real(kind=p6) :: x
        real(kind=p10r100) :: y
        real(kind=r400) :: z

        print *, precision(x), range(x)
        print *, precision(y), range(y)
        print *, precision(z), range(z)
      end program real_kinds

  See also:
    :ref:`PRECISION`,
    :ref:`RANGE`,
    :ref:`RADIX`
