..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Score

.. index:: Score Options

.. _score-options:

Score Options
^^^^^^^^^^^^^

These options are defined for Score implementations:

.. option:: -meb

  Compile code for big-endian mode.  This is the default.

.. option:: -mel

  Compile code for little-endian mode.

.. option:: -mnhwloop

  Disable generation of ``bcnz`` instructions.

.. option:: -muls

  Enable generation of unaligned load and store instructions.

.. option:: -mmac

  Enable the use of multiply-accumulate instructions. Disabled by default.

.. option:: -mscore5

  Specify the SCORE5 as the target architecture.

.. option:: -mscore5u

  Specify the SCORE5U of the target architecture.

.. option:: -mscore7

  Specify the SCORE7 as the target architecture. This is the default.

.. option:: -mscore7d

  Specify the SCORE7D as the target architecture.
