..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: FT32

.. index:: FT32 Options

.. _ft32-options:

FT32 Options
^^^^^^^^^^^^

These options are defined specifically for the FT32 port.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  an alternate runtime startup and library to be linked.
  You must not use this option when generating programs that will run on
  real hardware; you must provide your own runtime library for whatever
  I/O functions are needed.

.. option:: -mlra

  Enable Local Register Allocation.  This is still experimental for FT32,
  so by default the compiler uses standard reload.

.. option:: -mnodiv

  Do not use div and mod instructions.

.. option:: -mft32b

  Enable use of the extended instructions of the FT32B processor.

.. option:: -mcompress

  Compress all code using the Ft32B code compression scheme.

.. option:: -mnopm

  Do not generate code that reads program memory.
