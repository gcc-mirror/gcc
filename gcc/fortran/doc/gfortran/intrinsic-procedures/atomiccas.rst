..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _atomic_cas:

ATOMIC_CAS --- Atomic compare and swap
**************************************

.. index:: ATOMIC_DEFINE, Atomic subroutine, compare and swap

.. function:: ATOMIC_CAS(ATOM, OLD, COMPARE, NEW, STAT)

  ``ATOMIC_CAS`` compares the variable :samp:`{ATOM}` with the value of
  :samp:`{COMPARE}` ; if the value is the same, :samp:`{ATOM}` is set to the value
  of :samp:`{NEW}`. Additionally, :samp:`{OLD}` is set to the value of :samp:`{ATOM}`
  that was used for the comparison.  When :samp:`{STAT}` is present and the invocation
  was successful, it is assigned the value 0. If it is present and the invocation
  has failed, it is assigned a positive value; in particular, for a coindexed
  :samp:`{ATOM}`, if the remote image has stopped, it is assigned the value of
  ``ISO_FORTRAN_ENV`` 's ``STAT_STOPPED_IMAGE`` and if the remote image has
  failed, the value ``STAT_FAILED_IMAGE``.

  :param ATOM:
    Scalar coarray or coindexed variable of either integer
    type with ``ATOMIC_INT_KIND`` kind or logical type with
    ``ATOMIC_LOGICAL_KIND`` kind.

  :param OLD:
    Scalar of the same type and kind as :samp:`{ATOM}`.

  :param COMPARE:
    Scalar variable of the same type and kind as
    :samp:`{ATOM}`.

  :param NEW:
    Scalar variable of the same type as :samp:`{ATOM}`. If kind
    is different, the value is converted to the kind of :samp:`{ATOM}`.

  :param STAT:
    (optional) Scalar default-kind integer variable.

  Standard:
    TS 18508 or later

  Class:
    Atomic subroutine

  Syntax:
    .. code-block:: fortran

      CALL ATOMIC_CAS (ATOM, OLD, COMPARE, NEW [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        logical(atomic_logical_kind) :: atom[*], prev
        call atomic_cas (atom[1], prev, .false., .true.))
      end program atomic

  See also:
    :ref:`ATOMIC_DEFINE`,
    :ref:`ATOMIC_REF`,
    :ref:`ISO_FORTRAN_ENV`
