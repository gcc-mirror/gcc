..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfortran_unbuffered_preconnected:

GFORTRAN_UNBUFFERED_PRECONNECTED---Do not buffer I/O on preconnected units
**************************************************************************

The environment variable named :envvar:`GFORTRAN_UNBUFFERED_PRECONNECTED` controls
whether I/O on a preconnected unit (i.e. STDOUT or STDERR) is unbuffered.  If
the first letter is :samp:`y`, :samp:`Y` or :samp:`1`, I/O is unbuffered.  This
will slow down small sequential reads and writes.  If the first letter
is :samp:`n`, :samp:`N` or :samp:`0`, I/O is buffered.  This is the default.
