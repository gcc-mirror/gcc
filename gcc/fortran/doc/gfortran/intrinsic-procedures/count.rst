..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: COUNT, array, conditionally count elements, array, element counting, array, number of elements

.. _count:

COUNT --- Count function
************************

.. function:: COUNT(MASK , DIM, KIND)

  Counts the number of ``.TRUE.`` elements in a logical :samp:`{MASK}`,
  or, if the :samp:`{DIM}` argument is supplied, counts the number of
  elements along each row of the array in the :samp:`{DIM}` direction.
  If the array has zero size, or all of the elements of :samp:`{MASK}` are
  ``.FALSE.``, then the result is ``0``.

  :param MASK:
    The type shall be ``LOGICAL``.

  :param DIM:
    (Optional) The type shall be ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.
    If :samp:`{DIM}` is present, the result is an array with a rank one less
    than the rank of :samp:`{ARRAY}`, and a size corresponding to the shape
    of :samp:`{ARRAY}` with the :samp:`{DIM}` dimension removed.

  Standard:
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = COUNT(MASK [, DIM, KIND])

  Example:
    .. code-block:: fortran

      program test_count
          integer, dimension(2,3) :: a, b
          logical, dimension(2,3) :: mask
          a = reshape( (/ 1, 2, 3, 4, 5, 6 /), (/ 2, 3 /))
          b = reshape( (/ 0, 7, 3, 4, 5, 8 /), (/ 2, 3 /))
          print '(3i3)', a(1,:)
          print '(3i3)', a(2,:)
          print *
          print '(3i3)', b(1,:)
          print '(3i3)', b(2,:)
          print *
          mask = a.ne.b
          print '(3l3)', mask(1,:)
          print '(3l3)', mask(2,:)
          print *
          print '(3i3)', count(mask)
          print *
          print '(3i3)', count(mask, 1)
          print *
          print '(3i3)', count(mask, 2)
      end program test_count