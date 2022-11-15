..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: NUM_IMAGES, coarray, NUM_IMAGES, images, number of

.. _num_images:

NUM_IMAGES --- Function that returns the number of images
*********************************************************

.. function:: NUM_IMAGES(DISTANCE, FAILED)

  Returns the number of images.

  :param DISTANCE:
    (optional, intent(in)) Nonnegative scalar integer

  :param FAILED:
    (optional, intent(in)) Scalar logical expression

  :return:
    Scalar default-kind integer.  If :samp:`{DISTANCE}` is not present or has value 0,
    the number of images in the current team is returned. For values smaller or
    equal distance to the initial team, it returns the number of images index
    on the ancestor team which has a distance of :samp:`{DISTANCE}` from the invoking
    team. If :samp:`{DISTANCE}` is larger than the distance to the initial team, the
    number of images of the initial team is returned. If :samp:`{FAILED}` is not present
    the total number of images is returned; if it has the value ``.TRUE.``,
    the number of failed images is returned, otherwise, the number of images which
    do have not the failed status.

  Standard:
    Fortran 2008 and later. With :samp:`{DISTANCE}` or :samp:`{FAILED}` argument,
    Technical Specification (TS) 18508 or later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = NUM_IMAGES(DISTANCE, FAILED)

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

  See also:
    :ref:`THIS_IMAGE`,
    :ref:`IMAGE_INDEX`
