..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _eoshift:

EOSHIFT --- End-off shift elements of an array
**********************************************

.. index:: EOSHIFT, array, shift

.. function:: EOSHIFT(ARRAY, SHIFT, BOUNDARY, DIM)

  ``EOSHIFT(ARRAY, SHIFT[, BOUNDARY, DIM])`` performs an end-off shift on
  elements of :samp:`{ARRAY}` along the dimension of :samp:`{DIM}`.  If :samp:`{DIM}` is
  omitted it is taken to be ``1``.  :samp:`{DIM}` is a scalar of type
  ``INTEGER`` in the range of 1 \leq DIM \leq n) where n is the
  rank of :samp:`{ARRAY}`.  If the rank of :samp:`{ARRAY}` is one, then all elements of
  :samp:`{ARRAY}` are shifted by :samp:`{SHIFT}` places.  If rank is greater than one,
  then all complete rank one sections of :samp:`{ARRAY}` along the given dimension are
  shifted.  Elements shifted out one end of each rank one section are dropped.  If
  :samp:`{BOUNDARY}` is present then the corresponding value of from :samp:`{BOUNDARY}`
  is copied back in the other end.  If :samp:`{BOUNDARY}` is not present then the
  following are copied in depending on the type of :samp:`{ARRAY}`.

  :param ARRAY:
    May be any type, not scalar.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :param BOUNDARY:
    Same type as :samp:`{ARRAY}`.

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

      RESULT = EOSHIFT(ARRAY, SHIFT [, BOUNDARY, DIM])

  Example:
    .. code-block:: fortran

      program test_eoshift
          integer, dimension(3,3) :: a
          a = reshape( (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))
          print '(3i3)', a(1,:)
          print '(3i3)', a(2,:)
          print '(3i3)', a(3,:)
          a = EOSHIFT(a, SHIFT=(/1, 2, 1/), BOUNDARY=-5, DIM=2)
          print *
          print '(3i3)', a(1,:)
          print '(3i3)', a(2,:)
          print '(3i3)', a(3,:)
      end program test_eoshift
