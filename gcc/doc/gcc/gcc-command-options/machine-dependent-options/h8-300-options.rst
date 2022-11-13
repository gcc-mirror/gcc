..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: H8/300

.. _h8-300-options:

H8/300 Options
^^^^^^^^^^^^^^

These :samp:`-m` options are defined for the H8/300 implementations:

.. option:: -mrelax

  Shorten some address references at link time, when possible; uses the
  linker option :option:`-relax`.  See `Using ld <https://sourceware.org/binutils/docs/ld/H8_002f300.html>`_
  for a fuller description.

.. option:: -mh

  Generate code for the H8/300H.

.. option:: -ms

  Generate code for the H8S.

.. option:: -mn

  Generate code for the H8S and H8/300H in the normal mode.  This switch
  must be used either with :option:`-mh` or :option:`-ms`.

.. option:: -ms2600

  Generate code for the H8S/2600.  This switch must be used with :option:`-ms`.

.. option:: -mexr

  Extended registers are stored on stack before execution of function
  with monitor attribute. Default option is :option:`-mexr`.
  This option is valid only for H8S targets.

.. option:: -mno-exr

  Extended registers are not stored on stack before execution of function
  with monitor attribute. Default option is :option:`-mno-exr`.
  This option is valid only for H8S targets.

.. option:: -mexr

  Default setting; overrides :option:`-mno-exr`.

.. option:: -mint32

  Make ``int`` data 32 bits by default.

.. option:: -malign-300

  On the H8/300H and H8S, use the same alignment rules as for the H8/300.
  The default for the H8/300H and H8S is to align longs and floats on
  4-byte boundaries.
  :option:`-malign-300` causes them to be aligned on 2-byte boundaries.
  This option has no effect on the H8/300.