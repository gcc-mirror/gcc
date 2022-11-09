..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfortran_error_backtrace:

GFORTRAN_ERROR_BACKTRACE---Show backtrace on run-time errors
************************************************************

If the :envvar:`GFORTRAN_ERROR_BACKTRACE` variable is set to :samp:`y`,
:samp:`Y` or :samp:`1` (only the first letter is relevant) then a
backtrace is printed when a serious run-time error occurs.  To disable
the backtracing, set the variable to :samp:`n`, :samp:`N`, :samp:`0`.
Default is to print a backtrace unless the :option:`-fno-backtrace`
compile option was used.
