..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _event_query:

EVENT_QUERY --- Query whether a coarray event has occurred
**********************************************************

.. index:: EVENT_QUERY, Events, EVENT_QUERY

.. function:: EVENT_QUERY(EVENT, COUNT, STAT)

  ``EVENT_QUERY`` assignes the number of events to :samp:`{COUNT}` which have been
  posted to the :samp:`{EVENT}` variable and not yet been removed by calling
  ``EVENT WAIT``. When :samp:`{STAT}` is present and the invocation was successful,
  it is assigned the value 0. If it is present and the invocation has failed,
  it is assigned a positive value and :samp:`{COUNT}` is assigned the value -1.

  :param EVENT:
    (intent(IN)) Scalar of type ``EVENT_TYPE``,
    defined in ``ISO_FORTRAN_ENV`` ; shall not be coindexed.

  :param COUNT:
    (intent(out))Scalar integer with at least the
    precision of default integer.

  :param STAT:
    (optional) Scalar default-kind integer variable.

  Standard:
    TS 18508 or later

  Class:
    subroutine

  Syntax:
    .. code-block:: fortran

      CALL EVENT_QUERY (EVENT, COUNT [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        implicit none
        type(event_type) :: event_value_has_been_set[*]
        integer :: cnt
        if (this_image() == 1) then
          call event_query (event_value_has_been_set, cnt)
          if (cnt > 0) write(*,*) "Value has been set"
        elseif (this_image() == 2) then
          event post (event_value_has_been_set[1])
        end if
      end program atomic