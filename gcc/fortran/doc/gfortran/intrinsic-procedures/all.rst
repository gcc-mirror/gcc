..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _all:

ALL --- All values in MASK along DIM are true
**********************************************

.. index:: ALL, array, apply condition, array, condition testing

.. function:: ALL(MASK, DIM)

  ``ALL(MASK [, DIM])`` determines if all the values are true in :samp:`{MASK}`
  in the array along dimension :samp:`{DIM}`.

  :param MASK:
    The type of the argument shall be ``LOGICAL`` and
    it shall not be scalar.

  :param DIM:
    (Optional) :samp:`{DIM}` shall be a scalar integer
    with a value that lies between one and the rank of :samp:`{MASK}`.

  :return:
    ``ALL(MASK)`` returns a scalar value of type ``LOGICAL`` where
    the kind type parameter is the same as the kind type parameter of
    :samp:`{MASK}`.  If :samp:`{DIM}` is present, then ``ALL(MASK, DIM)`` returns
    an array with the rank of :samp:`{MASK}` minus 1.  The shape is determined from
    the shape of :samp:`{MASK}` where the :samp:`{DIM}` dimension is elided.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = ALL(MASK [, DIM])

  Example:
    .. code-block:: fortran

      program test_all
        logical l
        l = all((/.true., .true., .true./))
        print *, l
        call section
        contains
          subroutine section
            integer a(2,3), b(2,3)
            a = 1
            b = 1
            b(2,2) = 2
            print *, all(a .eq. b, 1)
            print *, all(a .eq. b, 2)
          end subroutine section
      end program test_all
