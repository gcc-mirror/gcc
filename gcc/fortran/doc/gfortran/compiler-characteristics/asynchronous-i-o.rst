..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: input/output, asynchronous, asynchronous I/O

.. _asynchronous-i-o:

Asynchronous I/O
****************

Asynchronous I/O is supported if the program is linked against the
POSIX thread library. If that is not the case, all I/O is performed
as synchronous. On systems which do not support pthread condition
variables, such as AIX, I/O is also performed as synchronous.

On some systems, such as Darwin or Solaris, the POSIX thread library
is always linked in, so asynchronous I/O is always performed. On other
sytems, such as Linux, it is necessary to specify :option:`-pthread`,
:option:`-lpthread` or :option:`-fopenmp` during the linking step.