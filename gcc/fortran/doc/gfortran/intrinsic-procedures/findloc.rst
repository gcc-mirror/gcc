..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _findloc:

FINDLOC --- Search an array for a value
***************************************

.. index:: FINDLOC, findloc

.. function:: FINDLOC(ARRAY, VALUE, MASK, KIND, BACK)

  Determines the location of the element in the array with the value
  given in the :samp:`{VALUE}` argument, or, if the :samp:`{DIM}` argument is
  supplied, determines the locations of the elements equal to the
  :samp:`{VALUE}` argument element along each
  row of the array in the :samp:`{DIM}` direction.  If :samp:`{MASK}` is
  present, only the elements for which :samp:`{MASK}` is ``.TRUE.`` are
  considered.  If more than one element in the array has the value
  :samp:`{VALUE}`, the location returned is that of the first such element
  in array element order if the :samp:`{BACK}` is not present or if it is
  ``.FALSE.``. If :samp:`{BACK}` is true, the location returned is that
  of the last such element. If the array has zero size, or all of the
  elements of :samp:`{MASK}` are ``.FALSE.``, then the result is an array
  of zeroes.  Similarly, if :samp:`{DIM}` is supplied and all of the
  elements of :samp:`{MASK}` along a given row are zero, the result value
  for that row is zero.

  :param ARRAY:
    Shall be an array of intrinsic type.

  :param VALUE:
    A scalar of intrinsic type which is in type
    conformance with :samp:`{ARRAY}`.

  :param DIM:
    (Optional) Shall be a scalar of type
    ``INTEGER``, with a value between one and the rank of :samp:`{ARRAY}`,
    inclusive.  It may not be an optional dummy argument.

  :param MASK:
    (Optional) Shall be of type ``LOGICAL``,
    and conformable with :samp:`{ARRAY}`.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :param BACK:
    (Optional) A scalar of type ``LOGICAL``.

  :return:
    If :samp:`{DIM}` is absent, the result is a rank-one array with a length
    equal to the rank of :samp:`{ARRAY}`.  If :samp:`{DIM}` is present, the result
    is an array with a rank one less than the rank of :samp:`{ARRAY}`, and a
    size corresponding to the size of :samp:`{ARRAY}` with the :samp:`{DIM}`
    dimension removed.  If :samp:`{DIM}` is present and :samp:`{ARRAY}` has a rank
    of one, the result is a scalar.  If the optional argument :samp:`{KIND}`
    is present, the result is an integer of kind :samp:`{KIND}`, otherwise it
    is of default kind.

  Standard:
    Fortran 2008 and later.

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = FINDLOC(ARRAY, VALUE, DIM [, MASK] [,KIND] [,BACK])
      RESULT = FINDLOC(ARRAY, VALUE, [, MASK] [,KIND] [,BACK])

  See also:
    :ref:`MAXLOC`,
    :ref:`MINLOC`
