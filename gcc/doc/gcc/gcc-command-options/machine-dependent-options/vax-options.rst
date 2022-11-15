..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: VAX

.. index:: VAX options

.. _vax-options:

VAX Options
^^^^^^^^^^^

These :samp:`-m` options are defined for the VAX:

.. option:: -munix

  Do not output certain jump instructions (``aobleq`` and so on)
  that the Unix assembler for the VAX cannot handle across long
  ranges.

.. option:: -mgnu

  Do output those jump instructions, on the assumption that the
  GNU assembler is being used.

.. option:: -mg

  Output code for G-format floating-point numbers instead of D-format.

.. option:: -mlra, -mno-lra

  Enable Local Register Allocation.  This is still experimental for the VAX,
  so by default the compiler uses standard reload.
