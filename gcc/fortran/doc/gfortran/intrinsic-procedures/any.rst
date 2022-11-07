..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _any:

ANY --- Any value in MASK along DIM is true
********************************************

.. index:: ANY, array, apply condition, array, condition testing

.. function:: ANY(MASK, DIM)

  ``ANY(MASK [, DIM])`` determines if any of the values in the logical array
  :samp:`{MASK}` along dimension :samp:`{DIM}` are ``.TRUE.``.

  :param MASK:
    The type of the argument shall be ``LOGICAL`` and
    it shall not be scalar.

  :param DIM:
    (Optional) :samp:`{DIM}` shall be a scalar integer
    with a value that lies between one and the rank of :samp:`{MASK}`.

  :return:
    ``ANY(MASK)`` returns a scalar value of type ``LOGICAL`` where
    the kind type parameter is the same as the kind type parameter of
    :samp:`{MASK}`.  If :samp:`{DIM}` is present, then ``ANY(MASK, DIM)`` returns
    an array with the rank of :samp:`{MASK}` minus 1.  The shape is determined from
    the shape of :samp:`{MASK}` where the :samp:`{DIM}` dimension is elided.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = ANY(MASK [, DIM])

  Example:
    .. code-block:: fortran

      program test_any
        logical l
        l = any((/.true., .true., .true./))
        print *, l
        call section
        contains
          subroutine section
            integer a(2,3), b(2,3)
            a = 1
            b = 1
            b(2,2) = 2
            print *, any(a .eq. b, 1)
            print *, any(a .eq. b, 2)
          end subroutine section
      end program test_any