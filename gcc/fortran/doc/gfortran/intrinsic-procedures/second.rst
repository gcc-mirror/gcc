..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SECOND, time, elapsed, elapsed time

.. _second:

SECOND --- CPU time function
****************************

.. function:: SECOND()

  Returns a ``REAL(4)`` value representing the elapsed CPU time in
  seconds.  This provides the same functionality as the standard
  ``CPU_TIME`` intrinsic, and is only included for backwards
  compatibility.

  :param TIME:
    Shall be of type ``REAL(4)``.

  :return:
    In either syntax, :samp:`{TIME}` is set to the process's current runtime in
    seconds.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL SECOND(TIME)
      TIME = SECOND()

  See also:
    :ref:`CPU_TIME`
