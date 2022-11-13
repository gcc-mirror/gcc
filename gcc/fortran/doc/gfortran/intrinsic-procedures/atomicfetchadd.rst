..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ATOMIC_FETCH_ADD, Atomic subroutine, ADD with fetch

.. _atomic_fetch_add:

ATOMIC_FETCH_ADD --- Atomic ADD operation with prior fetch
**********************************************************

.. function:: ATOMIC_FETCH_ADD(ATOM, VALUE, OLD)

  ``ATOMIC_FETCH_ADD(ATOM, VALUE, OLD)`` atomically stores the value of
  :samp:`{ATOM}` in :samp:`{OLD}` and adds the value of :samp:`{VALUE}` to the
  variable :samp:`{ATOM}`. When :samp:`{STAT}` is present and the invocation was
  successful, it is assigned the value 0. If it is present and the invocation
  has failed, it is assigned a positive value; in particular, for a coindexed
  :samp:`{ATOM}`, if the remote image has stopped, it is assigned the value of
  ``ISO_FORTRAN_ENV`` 's ``STAT_STOPPED_IMAGE`` and if the remote image has
  failed, the value ``STAT_FAILED_IMAGE``.

  :param ATOM:
    Scalar coarray or coindexed variable of integer
    type with ``ATOMIC_INT_KIND`` kind.
    ``ATOMIC_LOGICAL_KIND`` kind.

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

      CALL ATOMIC_FETCH_ADD (ATOM, VALUE, old [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        integer(atomic_int_kind) :: atom[*], old
        call atomic_add (atom[1], this_image(), old)
      end program atomic

  See also:
    :ref:`ATOMIC_DEFINE`,
    :ref:`ATOMIC_ADD`,
    :ref:`ISO_FORTRAN_ENV`,
    :ref:`ATOMIC_FETCH_AND`,
    :ref:`ATOMIC_FETCH_OR`,
    :ref:`ATOMIC_FETCH_XOR`