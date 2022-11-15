..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cshift:

CSHIFT --- Circular shift elements of an array
**********************************************

.. index:: CSHIFT, array, shift circularly, array, permutation, array, rotate

.. function:: CSHIFT(ARRAY, SHIFT, DIM)

  ``CSHIFT(ARRAY, SHIFT [, DIM])`` performs a circular shift on elements of
  :samp:`{ARRAY}` along the dimension of :samp:`{DIM}`.  If :samp:`{DIM}` is omitted it is
  taken to be ``1``.  :samp:`{DIM}` is a scalar of type ``INTEGER`` in the
  range of 1 \leq DIM \leq n) where n is the rank of :samp:`{ARRAY}`.
  If the rank of :samp:`{ARRAY}` is one, then all elements of :samp:`{ARRAY}` are shifted
  by :samp:`{SHIFT}` places.  If rank is greater than one, then all complete rank one
  sections of :samp:`{ARRAY}` along the given dimension are shifted.  Elements
  shifted out one end of each rank one section are shifted back in the other end.

  :param ARRAY:
    Shall be an array of any type.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :param DIM:
    The type shall be ``INTEGER``.

  :return:
    Returns an array of same type and rank as the :samp:`{ARRAY}` argument.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = CSHIFT(ARRAY, SHIFT [, DIM])

  Example:
    .. code-block:: fortran

      program test_cshift
          integer, dimension(3,3) :: a
          a = reshape( (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))
          print '(3i3)', a(1,:)
          print '(3i3)', a(2,:)
          print '(3i3)', a(3,:)
          a = cshift(a, SHIFT=(/1, 2, -1/), DIM=2)
          print *
          print '(3i3)', a(1,:)
          print '(3i3)', a(2,:)
          print '(3i3)', a(3,:)
      end program test_cshift
