..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfortran_unbuffered_all:

GFORTRAN_UNBUFFERED_ALL---Do not buffer I/O on all units
********************************************************

This environment variable controls whether all I/O is unbuffered.  If
the first letter is :samp:`y`, :samp:`Y` or :samp:`1`, all I/O is
unbuffered.  This will slow down small sequential reads and writes.  If
the first letter is :samp:`n`, :samp:`N` or :samp:`0`, I/O is buffered.
This is the default.
