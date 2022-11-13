..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _signal:

SIGNAL --- Signal handling subroutine (or function)
***************************************************

.. index:: SIGNAL, system, signal handling

.. function:: SIGNAL(NUMBER, HANDLER, STATUS)

  ``SIGNAL(NUMBER, HANDLER [, STATUS])`` causes external subroutine
  :samp:`{HANDLER}` to be executed with a single integer argument when signal
  :samp:`{NUMBER}` occurs.  If :samp:`{HANDLER}` is an integer, it can be used to
  turn off handling of signal :samp:`{NUMBER}` or revert to its default
  action.  See ``signal(2)``.

  :param NUMBER:
    Shall be a scalar integer, with ``INTENT(IN)``

  :param HANDLER:
    Signal handler (``INTEGER FUNCTION`` or
    ``SUBROUTINE``) or dummy/global ``INTEGER`` scalar.
    ``INTEGER``. It is ``INTENT(IN)``.

  :param STATUS:
    (Optional) :samp:`{STATUS}` shall be a scalar
    integer. It has ``INTENT(OUT)``.

  :return:
    The ``SIGNAL`` function returns the value returned by ``signal(2)``.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL SIGNAL(NUMBER, HANDLER [, STATUS])
      STATUS = SIGNAL(NUMBER, HANDLER)

  Example:
    .. code-block:: fortran

      program test_signal
        intrinsic signal
        external handler_print

        call signal (12, handler_print)
        call signal (10, 1)

        call sleep (30)
      end program test_signal