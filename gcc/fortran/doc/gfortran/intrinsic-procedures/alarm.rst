..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _alarm:

ALARM --- Execute a routine after a given delay
***********************************************

.. index:: ALARM, delayed execution

.. function:: ALARM(SECONDS, HANDLER, STATUS)

  ``ALARM(SECONDS, HANDLER [, STATUS])`` causes external subroutine :samp:`{HANDLER}`
  to be executed after a delay of :samp:`{SECONDS}` by using ``alarm(2)`` to
  set up a signal and ``signal(2)`` to catch it. If :samp:`{STATUS}` is
  supplied, it will be returned with the number of seconds remaining until
  any previously scheduled alarm was due to be delivered, or zero if there
  was no previously scheduled alarm.

  :param SECONDS:
    The type of the argument shall be a scalar
    ``INTEGER``. It is ``INTENT(IN)``.

  :param HANDLER:
    Signal handler (``INTEGER FUNCTION`` or
    ``SUBROUTINE``) or dummy/global ``INTEGER`` scalar. The scalar
    values may be either ``SIG_IGN=1`` to ignore the alarm generated
    or ``SIG_DFL=0`` to set the default action. It is ``INTENT(IN)``.

  :param STATUS:
    (Optional) :samp:`{STATUS}` shall be a scalar
    variable of the default ``INTEGER`` kind. It is ``INTENT(OUT)``.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL ALARM(SECONDS, HANDLER [, STATUS])

  Example:
    .. code-block:: fortran

      program test_alarm
        external handler_print
        integer i
        call alarm (3, handler_print, i)
        print *, i
        call sleep(10)
      end program test_alarm

    This will cause the external routine :samp:`{handler_print}` to be called
    after 3 seconds.
