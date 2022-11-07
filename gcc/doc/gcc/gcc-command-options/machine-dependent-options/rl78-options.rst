..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: RL78

.. index:: RL78 Options

.. _rl78-options:

RL78 Options
^^^^^^^^^^^^

.. option:: -msim

  Links in additional target libraries to support operation within a
  simulator.

.. option:: -mmul=none

  Specifies the type of hardware multiplication and division support to
  be used.  The simplest is ``none``, which uses software for both
  multiplication and division.  This is the default.  The ``g13``
  value is for the hardware multiply/divide peripheral found on the
  RL78/G13 (S2 core) targets.  The ``g14`` value selects the use of
  the multiplication and division instructions supported by the RL78/G14
  (S3 core) parts.  The value ``rl78`` is an alias for ``g14`` and
  the value ``mg10`` is an alias for ``none``.

  In addition a C preprocessor macro is defined, based upon the setting
  of this option.  Possible values are: ``__RL78_MUL_NONE__``,
  ``__RL78_MUL_G13__`` or ``__RL78_MUL_G14__``.

.. option:: -mcpu=g10

  Specifies the RL78 core to target.  The default is the G14 core, also
  known as an S3 core or just RL78.  The G13 or S2 core does not have
  multiply or divide instructions, instead it uses a hardware peripheral
  for these operations.  The G10 or S1 core does not have register
  banks, so it uses a different calling convention.

  If this option is set it also selects the type of hardware multiply
  support to use, unless this is overridden by an explicit
  :option:`-mmul=none` option on the command line.  Thus specifying
  :option:`-mcpu=g13` enables the use of the G13 hardware multiply
  peripheral and specifying :option:`-mcpu=g10` disables the use of
  hardware multiplications altogether.

  Note, although the RL78/G14 core is the default target, specifying
  :option:`-mcpu=g14` or :option:`-mcpu=rl78` on the command line does
  change the behavior of the toolchain since it also enables G14
  hardware multiply support.  If these options are not specified on the
  command line then software multiplication routines will be used even
  though the code targets the RL78 core.  This is for backwards
  compatibility with older toolchains which did not have hardware
  multiply and divide support.

  In addition a C preprocessor macro is defined, based upon the setting
  of this option.  Possible values are: ``__RL78_G10__``,
  ``__RL78_G13__`` or ``__RL78_G14__``.

.. option:: -mg10, -mg13, -mg14, -mrl78

  These are aliases for the corresponding :option:`-mcpu=` option.  They
  are provided for backwards compatibility.

.. option:: -mallregs

  Allow the compiler to use all of the available registers.  By default
  registers ``r24..r31`` are reserved for use in interrupt handlers.
  With this option enabled these registers can be used in ordinary
  functions as well.

.. option:: -m64bit-doubles, -m32bit-doubles

  Make the ``double`` data type be 64 bits (:option:`-m64bit-doubles`)
  or 32 bits (:option:`-m32bit-doubles`) in size.  The default is
  :option:`-m32bit-doubles`.

.. option:: -msave-mduc-in-interrupts, -mno-save-mduc-in-interrupts

  Specifies that interrupt handler functions should preserve the
  MDUC registers.  This is only necessary if normal code might use
  the MDUC registers, for example because it performs multiplication
  and division operations.  The default is to ignore the MDUC registers
  as this makes the interrupt handlers faster.  The target option -mg13
  needs to be passed for this to work as this feature is only available
  on the G13 target (S2 core).  The MDUC registers will only be saved
  if the interrupt handler performs a multiplication or division
  operation or it calls another function.