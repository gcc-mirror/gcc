..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: REPEAT, string, repeat, string, concatenate

.. _repeat:

REPEAT --- Repeated string concatenation
*****************************************

.. function:: REPEAT(STRING, NCOPIES)

  Concatenates :samp:`{NCOPIES}` copies of a string.

  :param STRING:
    Shall be scalar and of type ``CHARACTER``.

  :param NCOPIES:
    Shall be scalar and of type ``INTEGER``.

  :return:
    A new scalar of type ``CHARACTER`` built up from :samp:`{NCOPIES}` copies
    of :samp:`{STRING}`.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = REPEAT(STRING, NCOPIES)

  Example:
    .. code-block:: fortran

      program test_repeat
        write(*,*) repeat("x", 5)   ! "xxxxx"
      end program