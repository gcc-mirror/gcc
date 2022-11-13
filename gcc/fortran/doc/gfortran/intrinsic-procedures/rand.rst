..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RAND, random number generation

.. _rand:

RAND --- Real pseudo-random number
**********************************

.. function:: RAND(FLAG)

  ``RAND(FLAG)`` returns a pseudo-random number from a uniform
  distribution between 0 and 1. If :samp:`{FLAG}` is 0, the next number
  in the current sequence is returned; if :samp:`{FLAG}` is 1, the generator
  is restarted by ``CALL SRAND(0)`` ; if :samp:`{FLAG}` has any other value,
  it is used as a new seed with ``SRAND``.

  :param I:
    Shall be a scalar ``INTEGER`` of kind 4.

  :return:
    The return value is of ``REAL`` type and the default kind.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = RAND(I)

  Example:
    .. code-block:: fortran

      program test_rand
        integer,parameter :: seed = 86456

        call srand(seed)
        print *, rand(), rand(), rand(), rand()
        print *, rand(seed), rand(), rand(), rand()
      end program test_rand

  See also:
    :ref:`SRAND`,
    :ref:`RANDOM_NUMBER`