..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Building in parallel
********************

GNU Make 3.80 and above, which is necessary to build GCC, support
building in parallel.  To activate this, you can use :samp:`make -j 2`
instead of :samp:`make`.  You can also specify a bigger number, and
in most cases using a value greater than the number of processors in
your machine will result in fewer and shorter I/O latency hits, thus
improving overall throughput; this is especially true for slow drives
and network filesystems.