..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: //, C++ comments, comments, C++ style

.. _c++-comments:

C++ Style Comments
******************

In GNU C, you may use C++ style comments, which start with :samp:`//` and
continue until the end of the line.  Many other C implementations allow
such comments, and they are included in the 1999 C standard.  However,
C++ style comments are not recognized if you specify an :option:`-std`
option specifying a version of ISO C before C99, or :option:`-ansi`
(equivalent to :option:`-std=c90`).
