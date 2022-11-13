..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: PRESENT

.. _present:

PRESENT --- Determine whether an optional dummy argument is specified
*********************************************************************

.. function:: PRESENT(A)

  Determines whether an optional dummy argument is present.

  :param A:
    May be of any type and may be a pointer, scalar or array
    value, or a dummy procedure. It shall be the name of an optional dummy argument
    accessible within the current subroutine or function.

  :return:
    Returns either ``TRUE`` if the optional argument :samp:`{A}` is present, or
    ``FALSE`` otherwise.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = PRESENT(A)

  Example:
    .. code-block:: fortran

      PROGRAM test_present
        WRITE(*,*) f(), f(42)      ! "F T"
      CONTAINS
        LOGICAL FUNCTION f(x)
          INTEGER, INTENT(IN), OPTIONAL :: x
          f = PRESENT(x)
        END FUNCTION
      END PROGRAM