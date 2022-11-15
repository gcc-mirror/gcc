..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

  Contributed by Aldy Hernandez <aldy@quesejoda.com>

.. _libgcc:

The GCC low-level runtime library
---------------------------------

GCC provides a low-level runtime library, :samp:`libgcc.a` or
:samp:`libgcc_s.so.1` on some platforms.  GCC generates calls to
routines in this library automatically, whenever it needs to perform
some operation that is too complicated to emit inline code for.

Most of the routines in ``libgcc`` handle arithmetic operations
that the target processor cannot perform directly.  This includes
integer multiply and divide on some machines, and all floating-point
and fixed-point operations on other machines.  ``libgcc`` also includes
routines for exception handling, and a handful of miscellaneous operations.

Some of these routines can be defined in mostly machine-independent C.
Others must be hand-written in assembly language for each processor
that needs them.

GCC will also generate calls to C library routines, such as
``memcpy`` and ``memset``, in some cases.  The set of routines
that GCC may possibly use is documented in :ref:`gcc:other-builtins`.

These routines take arguments and return values of a specific machine
mode, not a specific C type.  See :ref:`machine-modes`, for an explanation
of this concept.  For illustrative purposes, in this chapter the
floating point type ``float`` is assumed to correspond to ``SFmode`` ;
``double`` to ``DFmode`` ; and ``long double`` to both
``TFmode`` and ``XFmode``.  Similarly, the integer types ``int``
and ``unsigned int`` correspond to ``SImode`` ; ``long`` and
``unsigned long`` to ``DImode`` ; and ``long long`` and
``unsigned long long`` to ``TImode``.

.. toctree::
  :maxdepth: 2

  the-gcc-low-level-runtime-library/routines-for-integer-arithmetic
  the-gcc-low-level-runtime-library/routines-for-floating-point-emulation
  the-gcc-low-level-runtime-library/routines-for-decimal-floating-point-emulation
  the-gcc-low-level-runtime-library/routines-for-fixed-point-fractional-emulation
  the-gcc-low-level-runtime-library/language-independent-routines-for-exception-handling
  the-gcc-low-level-runtime-library/miscellaneous-runtime-library-routines
