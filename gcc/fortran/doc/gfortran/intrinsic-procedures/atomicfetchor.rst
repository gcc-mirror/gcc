..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ATOMIC_FETCH_OR, Atomic subroutine, OR with fetch

.. _atomic_fetch_or:

ATOMIC_FETCH_OR --- Atomic bitwise OR operation with prior fetch
****************************************************************

.. function:: ATOMIC_FETCH_OR (ATOM, VALUE, OLD , STAT)

  ``ATOMIC_OR(ATOM, VALUE)`` atomically stores the value of :samp:`{ATOM}` in
  :samp:`{OLD}` and defines :samp:`{ATOM}` with the bitwise OR between the values of
  :samp:`{ATOM}` and :samp:`{VALUE}`. When :samp:`{STAT}` is present and the invocation was
  successful, it is assigned the value 0. If it is present and the invocation has
  failed, it is assigned a positive value; in particular, for a coindexed
  :samp:`{ATOM}`, if the remote image has stopped, it is assigned the value of
  ``ISO_FORTRAN_ENV`` 's ``STAT_STOPPED_IMAGE`` and if the remote image has
  failed, the value ``STAT_FAILED_IMAGE``.

  :param ATOM:
    Scalar coarray or coindexed variable of integer
    type with ``ATOMIC_INT_KIND`` kind.

  :param VALUE:
    Scalar of the same type as :samp:`{ATOM}`. If the kind
    is different, the value is converted to the kind of :samp:`{ATOM}`.

  :param OLD:
    Scalar of the same type and kind as :samp:`{ATOM}`.

  :param STAT:
    (optional) Scalar default-kind integer variable.

  Standard:
    TS 18508 or later

  Class:
    Atomic subroutine

  Syntax:
    .. code-block:: fortran

      CALL ATOMIC_FETCH_OR (ATOM, VALUE, OLD [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        integer(atomic_int_kind) :: atom[*], old
        call atomic_fetch_or (atom[1], int(b'10100011101'), old)
      end program atomic

  See also:
    :ref:`ATOMIC_DEFINE`,
    :ref:`ATOMIC_OR`,
    :ref:`ISO_FORTRAN_ENV`,
    :ref:`ATOMIC_FETCH_ADD`,
    :ref:`ATOMIC_FETCH_AND`,
    :ref:`ATOMIC_FETCH_XOR`