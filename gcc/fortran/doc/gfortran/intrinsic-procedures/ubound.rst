..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: UBOUND, array, upper bound

.. _ubound:

UBOUND --- Upper dimension bounds of an array
*********************************************

.. function:: UBOUND(ARRAY , DIM , KIND)

  Returns the upper bounds of an array, or a single upper bound
  along the :samp:`{DIM}` dimension.

  :param ARRAY:
    Shall be an array, of any type.

  :param DIM:
    (Optional) Shall be a scalar ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.
    If :samp:`{DIM}` is absent, the result is an array of the upper bounds of
    :samp:`{ARRAY}`.  If :samp:`{DIM}` is present, the result is a scalar
    corresponding to the upper bound of the array along that dimension.  If
    :samp:`{ARRAY}` is an expression rather than a whole array or array
    structure component, or if it has a zero extent along the relevant
    dimension, the upper bound is taken to be the number of elements along
    the relevant dimension.

  Standard:
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = UBOUND(ARRAY [, DIM [, KIND]])

  See also:
    :ref:`LBOUND`,
    :ref:`LCOBOUND`