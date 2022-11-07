..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RANDOM_INIT, random number generation, initialization

.. _random_init:

RANDOM_INIT --- Initialize a pseudo-random number generator
***********************************************************

.. function:: RANDOM_INIT(REPEATABLE, IMAGE_DISTINCT)

  Initializes the state of the pseudorandom number generator used by
  ``RANDOM_NUMBER``.

  :param REPEATABLE:
    Shall be a scalar with a ``LOGICAL`` type,
    and it is ``INTENT(IN)``.  If it is ``.true.``, the seed is set to
    a processor-dependent value that is the same each time ``RANDOM_INIT``
    is called from the same image.  The term 'same image' means a single
    instance of program execution.  The sequence of random numbers is different
    for repeated execution of the program.  If it is ``.false.``, the seed
    is set to a processor-dependent value.

  :param IMAGE_DISTINCT:
    Shall be a scalar with a
    ``LOGICAL`` type, and it is ``INTENT(IN)``.  If it is ``.true.``,
    the seed is set to a processor-dependent value that is distinct from th
    seed set by a call to ``RANDOM_INIT`` in another image.  If it is
    ``.false.``, the seed is set to a value that does depend which image called
    ``RANDOM_INIT``.

  Standard:
    Fortran 2018

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL RANDOM_INIT(REPEATABLE, IMAGE_DISTINCT)

  Example:
    .. code-block:: fortran

      program test_random_seed
        implicit none
        real x(3), y(3)
        call random_init(.true., .true.)
        call random_number(x)
        call random_init(.true., .true.)
        call random_number(y)
        ! x and y are the same sequence
        if (any(x /= y)) call abort
      end program test_random_seed

  See also:
    :ref:`RANDOM_NUMBER`,
    :ref:`RANDOM_SEED`