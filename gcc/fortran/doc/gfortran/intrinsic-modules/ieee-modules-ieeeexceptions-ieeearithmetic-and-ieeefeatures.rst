..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ieee-modules:

IEEE modules: IEEE_EXCEPTIONS, IEEE_ARITHMETIC, and IEEE_FEATURES
*****************************************************************

:samp:`{Standard}:`
  Fortran 2003 and later

  The ``IEEE_EXCEPTIONS``, ``IEEE_ARITHMETIC``, and ``IEEE_FEATURES``
  intrinsic modules provide support for exceptions and IEEE arithmetic, as
  defined in Fortran 2003 and later standards, and the IEC 60559:1989 standard
  (*Binary floating-point arithmetic for microprocessor systems*). These
  modules are only provided on the following supported platforms:

  * i386 and x86_64 processors
  * platforms which use the GNU C Library (glibc)
  * platforms with support for SysV/386 routines for floating point
    interface (including Solaris and BSDs)
  * platforms with the AIX OS

For full compliance with the Fortran standards, code using the
``IEEE_EXCEPTIONS`` or ``IEEE_ARITHMETIC`` modules should be compiled
with the following options: ``-fno-unsafe-math-optimizations
-frounding-math -fsignaling-nans``.
