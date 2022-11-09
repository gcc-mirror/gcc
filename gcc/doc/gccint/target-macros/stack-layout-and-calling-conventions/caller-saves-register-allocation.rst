..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _caller-saves:

Caller-Saves Register Allocation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you enable it, GCC can save registers around function calls.  This
makes it possible to use call-clobbered registers to hold variables that
must live across calls.

.. c:macro:: HARD_REGNO_CALLER_SAVE_MODE (regno, nregs)

  A C expression specifying which mode is required for saving :samp:`{nregs}`
  of a pseudo-register in call-clobbered hard register :samp:`{regno}`.  If
  :samp:`{regno}` is unsuitable for caller save, ``VOIDmode`` should be
  returned.  For most machines this macro need not be defined since GCC
  will select the smallest suitable mode.
