..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: implementation limits

.. _implementation-limits:

Implementation limits
*********************

CPP has a small number of internal limits.  This section lists the
limits which the C standard requires to be no lower than some minimum,
and all the others known.  It is intended that there should be as few limits
as possible.  If you encounter an undocumented or inconvenient limit,
please report that as a bug.  See :ref:`gcc:bugs`.

Where we say something is limited :dfn:`only by available memory`, that
means that internal data structures impose no intrinsic limit, and space
is allocated with ``malloc`` or equivalent.  The actual limit will
therefore depend on many things, such as the size of other things
allocated by the compiler at the same time, the amount of memory
consumed by other processes on the same computer, etc.

* Nesting levels of :samp:`#include` files.

  We impose an arbitrary limit of 200 levels, to avoid runaway recursion.
  The standard requires at least 15 levels.

* Nesting levels of conditional inclusion.

  The C standard mandates this be at least 63.  CPP is limited only by
  available memory.

* Levels of parenthesized expressions within a full expression.

  The C standard requires this to be at least 63.  In preprocessor
  conditional expressions, it is limited only by available memory.

* Significant initial characters in an identifier or macro name.

  The preprocessor treats all characters as significant.  The C standard
  requires only that the first 63 be significant.

* Number of macros simultaneously defined in a single translation unit.

  The standard requires at least 4095 be possible.  CPP is limited only
  by available memory.

* Number of parameters in a macro definition and arguments in a macro call.

  We allow ``USHRT_MAX``, which is no smaller than 65,535.  The minimum
  required by the standard is 127.

* Number of characters on a logical source line.

  The C standard requires a minimum of 4096 be permitted.  CPP places
  no limits on this, but you may get incorrect column numbers reported in
  diagnostics for lines longer than 65,535 characters.

* Maximum size of a source file.

  The standard does not specify any lower limit on the maximum size of a
  source file.  GNU cpp maps files into memory, so it is limited by the
  available address space.  This is generally at least two gigabytes.
  Depending on the operating system, the size of physical memory may or
  may not be a limitation.