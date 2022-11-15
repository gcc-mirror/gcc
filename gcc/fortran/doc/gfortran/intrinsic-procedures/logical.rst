..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _logical:

LOGICAL --- Convert to logical type
***********************************

.. index:: LOGICAL, conversion, to logical

.. function:: LOGICAL(L, KIND)

  Converts one kind of ``LOGICAL`` variable to another.

  :param L:
    The type shall be ``LOGICAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is a ``LOGICAL`` value equal to :samp:`{L}`, with a
    kind corresponding to :samp:`{KIND}`, or of the default logical kind if
    :samp:`{KIND}` is not given.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = LOGICAL(L [, KIND])

  See also:
    :ref:`INT`,
    :ref:`REAL`,
    :ref:`CMPLX`
