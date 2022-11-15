..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: LCOBOUND, coarray, lower bound

.. _lcobound:

LCOBOUND --- Lower codimension bounds of an array
*************************************************

.. function:: LCOBOUND(COARRAY , DIM , KIND)

  Returns the lower bounds of a coarray, or a single lower cobound
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

      RESULT = LCOBOUND(COARRAY [, DIM [, KIND]])

  See also:
    :ref:`UCOBOUND`,
    :ref:`LBOUND`
