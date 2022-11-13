..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MODULO, modulo, division, modulo

.. _modulo:

MODULO --- Modulo function
**************************

.. function:: MODULO(A,P)

  ``MODULO(A,P)`` computes the :samp:`{A}` modulo :samp:`{P}`.

  :param A:
    Shall be a scalar of type ``INTEGER`` or ``REAL``.

  :param P:
    Shall be a scalar of the same type and kind as :samp:`{A}`.
    It shall not be zero.  (As a GNU extension, arguments of different kinds are
    permitted.)

  :return:
    The type and kind of the result are those of the arguments.  (As a GNU
    extension, kind is the largest kind of the actual arguments.)

  Standard:
    Fortran 95 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = MODULO(A, P)

  Example:
    .. code-block:: fortran

      program test_modulo
        print *, modulo(17,3)
        print *, modulo(17.5,5.5)

        print *, modulo(-17,3)
        print *, modulo(-17.5,5.5)

        print *, modulo(17,-3)
        print *, modulo(17.5,-5.5)
      end program

  See also:
    :ref:`MOD`