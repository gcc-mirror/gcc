..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IMAGE_INDEX, coarray, IMAGE_INDEX, images, cosubscript to image index conversion

.. _image_index:

IMAGE_INDEX --- Function that converts a cosubscript to an image index
**********************************************************************

.. function:: IMAGE_INDEX(COARRAY, SUB)

  Returns the image index belonging to a cosubscript.

  :param COARRAY:
    Coarray of any type.

  :param SUB:
    default integer rank-1 array of a size equal to
    the corank of :samp:`{COARRAY}`.

  :return:
    Scalar default integer with the value of the image index which corresponds
    to the cosubscripts. For invalid cosubscripts the result is zero.

  Standard:
    Fortran 2008 and later

  Class:
    Inquiry function.

  Syntax:
    .. code-block:: fortran

      RESULT = IMAGE_INDEX(COARRAY, SUB)

  Example:
    .. code-block:: fortran

      INTEGER :: array[2,-1:4,8,*]
      ! Writes  28 (or 0 if there are fewer than 28 images)
      WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1])

  See also:
    :ref:`THIS_IMAGE`,
    :ref:`NUM_IMAGES`