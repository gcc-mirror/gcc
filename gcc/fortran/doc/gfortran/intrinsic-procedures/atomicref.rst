..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ATOMIC_REF, Atomic subroutine, reference

.. _atomic_ref:

ATOMIC_REF --- Obtaining the value of a variable atomically
***********************************************************

.. function:: ATOMIC_REF(VALUE, ATOM , STAT)

  ``ATOMIC_DEFINE(ATOM, VALUE)`` atomically assigns the value of the
  variable :samp:`{ATOM}` to :samp:`{VALUE}`. When :samp:`{STAT}` is present and the
  invocation was successful, it is assigned the value 0. If it is present and the
  invocation has failed, it is assigned a positive value; in particular, for a
  coindexed :samp:`{ATOM}`, if the remote image has stopped, it is assigned the value
  of ``ISO_FORTRAN_ENV`` 's ``STAT_STOPPED_IMAGE`` and if the remote image
  has failed, the value ``STAT_FAILED_IMAGE``.

  :param VALUE:
    Scalar of the same type as :samp:`{ATOM}`. If the kind
    is different, the value is converted to the kind of :samp:`{ATOM}`.

  :param ATOM:
    Scalar coarray or coindexed variable of either integer
    type with ``ATOMIC_INT_KIND`` kind or logical type with
    ``ATOMIC_LOGICAL_KIND`` kind.

  :param STAT:
    (optional) Scalar default-kind integer variable.

  Standard:
    Fortran 2008 and later; with :samp:`{STAT}`, TS 18508 or later

  Class:
    Atomic subroutine

  Syntax:
    .. code-block:: fortran

      CALL ATOMIC_REF(VALUE, ATOM [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        logical(atomic_logical_kind) :: atom[*]
        logical :: val
        call atomic_ref (atom, .false.)
        ! ...
        call atomic_ref (atom, val)
        if (val) then
          print *, "Obtained"
        end if
      end program atomic

  See also:
    :ref:`ATOMIC_DEFINE`,
    :ref:`ATOMIC_CAS`,
    :ref:`ISO_FORTRAN_ENV`,
    :ref:`ATOMIC_FETCH_ADD`,
    :ref:`ATOMIC_FETCH_AND`,
    :ref:`ATOMIC_FETCH_OR`,
    :ref:`ATOMIC_FETCH_XOR`
