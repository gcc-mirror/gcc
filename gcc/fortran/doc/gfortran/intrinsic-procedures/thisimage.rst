..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: THIS_IMAGE, coarray, THIS_IMAGE, images, index of this image

.. _this_image:

THIS_IMAGE --- Function that returns the cosubscript index of this image
************************************************************************

.. function:: THIS_IMAGE(COARRAY , DIM)

  Returns the cosubscript for this image.

  :param DISTANCE:
    (optional, intent(in)) Nonnegative scalar integer
    (not permitted together with :samp:`{COARRAY}`).

  :param COARRAY:
    Coarray of any type  (optional; if :samp:`{DIM}`
    present, required).

  :param DIM:
    default integer scalar (optional). If present,
    :samp:`{DIM}` shall be between one and the corank of :samp:`{COARRAY}`.

  :return:
    Default integer. If :samp:`{COARRAY}` is not present, it is scalar; if
    :samp:`{DISTANCE}` is not present or has value 0, its value is the image index on
    the invoking image for the current team, for values smaller or equal
    distance to the initial team, it returns the image index on the ancestor team
    which has a distance of :samp:`{DISTANCE}` from the invoking team. If
    :samp:`{DISTANCE}` is larger than the distance to the initial team, the image
    index of the initial team is returned. Otherwise when the :samp:`{COARRAY}` is
    present, if :samp:`{DIM}` is not present, a rank-1 array with corank elements is
    returned, containing the cosubscripts for :samp:`{COARRAY}` specifying the invoking
    image. If :samp:`{DIM}` is present, a scalar is returned, with the value of
    the :samp:`{DIM}` element of ``THIS_IMAGE(COARRAY)``.

  Standard:
    Fortran 2008 and later. With :samp:`{DISTANCE}` argument,
    Technical Specification (TS) 18508 or later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = THIS_IMAGE()
      RESULT = THIS_IMAGE(DISTANCE)
      RESULT = THIS_IMAGE(COARRAY [, DIM])

  Example:
    .. code-block:: fortran

      INTEGER :: value[*]
      INTEGER :: i
      value = THIS_IMAGE()
      SYNC ALL
      IF (THIS_IMAGE() == 1) THEN
        DO i = 1, NUM_IMAGES()
          WRITE(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
        END DO
      END IF

      ! Check whether the current image is the initial image
      IF (THIS_IMAGE(HUGE(1)) /= THIS_IMAGE())
        error stop "something is rotten here"

  See also:
    :ref:`NUM_IMAGES`,
    :ref:`IMAGE_INDEX`
