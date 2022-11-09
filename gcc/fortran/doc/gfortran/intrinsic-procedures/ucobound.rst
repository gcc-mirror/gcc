..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: UCOBOUND, coarray, upper bound

.. _ucobound:

UCOBOUND --- Upper codimension bounds of an array
*************************************************

.. function:: UCOBOUND(COARRAY , DIM , KIND)

  Returns the upper cobounds of a coarray, or a single upper cobound
  along the :samp:`{DIM}` codimension.

  :param ARRAY:
    Shall be an coarray, of any type.

  :param DIM:
    (Optional) Shall be a scalar ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.
    If :samp:`{DIM}` is absent, the result is an array of the lower cobounds of
    :samp:`{COARRAY}`.  If :samp:`{DIM}` is present, the result is a scalar
    corresponding to the lower cobound of the array along that codimension.

  Standard:
    Fortran 2008 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = UCOBOUND(COARRAY [, DIM [, KIND]])

  See also:
    :ref:`LCOBOUND`,
    :ref:`LBOUND`
