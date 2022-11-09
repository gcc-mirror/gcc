..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RANDOM_NUMBER, random number generation

.. _random_number:

RANDOM_NUMBER --- Pseudo-random number
**************************************

.. function:: RANDOM_NUMBER(HARVEST)

  Returns a single pseudorandom number or an array of pseudorandom numbers
  from the uniform distribution over the range 0 \leq x < 1.

  :param HARVEST:
    Shall be a scalar or an array of type ``REAL``.

  Standard:
    Fortran 90 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL RANDOM_NUMBER(HARVEST)

  Example:
    .. code-block:: fortran

      program test_random_number
        REAL :: r(5,5)
        CALL RANDOM_NUMBER(r)
      end program

  See also:
    :ref:`RANDOM_SEED`,
    :ref:`RANDOM_INIT`
