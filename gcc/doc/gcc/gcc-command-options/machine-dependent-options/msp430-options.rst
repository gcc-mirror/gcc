..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MSP430

.. index:: MSP430 Options

.. _msp430-options:

MSP430 Options
^^^^^^^^^^^^^^

These options are defined for the MSP430:

.. option:: -masm-hex

  Force assembly output to always use hex constants.  Normally such
  constants are signed decimals, but this option is available for
  testsuite and/or aesthetic purposes.

.. option:: -mmcu=

  Select the MCU to target.  This is used to create a C preprocessor
  symbol based upon the MCU name, converted to upper case and pre- and
  post-fixed with :samp:`__`.  This in turn is used by the
  :samp:`msp430.h` header file to select an MCU-specific supplementary
  header file.

  The option also sets the ISA to use.  If the MCU name is one that is
  known to only support the 430 ISA then that is selected, otherwise the
  430X ISA is selected.  A generic MCU name of :samp:`msp430` can also be
  used to select the 430 ISA.  Similarly the generic :samp:`msp430x` MCU
  name selects the 430X ISA.

  In addition an MCU-specific linker script is added to the linker
  command line.  The script's name is the name of the MCU with
  :samp:`.ld` appended.  Thus specifying :option:`-mmcu=xxx` on the :command:`gcc`
  command line defines the C preprocessor symbol ``__XXX__`` and
  cause the linker to search for a script called :samp:`xxx.ld`.

  The ISA and hardware multiply supported for the different MCUs is hard-coded
  into GCC.  However, an external :samp:`devices.csv` file can be used to
  extend device support beyond those that have been hard-coded.

  GCC searches for the :samp:`devices.csv` file using the following methods in the
  given precedence order, where the first method takes precendence over the
  second which takes precedence over the third.

  Include path specified with -I and -L

    :samp:`devices.csv` will be searched for in each of the directories specified by
    include paths and linker library search paths.

  Path specified by the environment variable MSP430_GCC_INCLUDE_DIR

    Define the value of the global environment variable
    :samp:`MSP430_GCC_INCLUDE_DIR`
    to the full path to the directory containing devices.csv, and GCC will search
    this directory for devices.csv.  If devices.csv is found, this directory will
    also be registered as an include path, and linker library path.  Header files
    and linker scripts in this directory can therefore be used without manually
    specifying ``-I`` and ``-L`` on the command line.

  The msp430-elf{,bare}/include/devices directory

    Finally, GCC will examine :samp:`msp430-elf{,bare}/include/devices` from the
    toolchain root directory.  This directory does not exist in a default
    installation, but if the user has created it and copied :samp:`devices.csv`
    there, then the MCU data will be read.  As above, this directory will
    also be registered as an include path, and linker library path.

  If none of the above search methods find :samp:`devices.csv`, then the
  hard-coded MCU data is used.

.. option:: -mwarn-mcu, -mno-warn-mcu

  This option enables or disables warnings about conflicts between the
  MCU name specified by the :option:`-mmcu` option and the ISA set by the
  :option:`-mcpu` option and/or the hardware multiply support set by the
  :option:`-mhwmult` option.  It also toggles warnings about unrecognized
  MCU names.  This option is on by default.

.. option:: -mcpu=

  Specifies the ISA to use.  Accepted values are :samp:`msp430`,
  :samp:`msp430x` and :samp:`msp430xv2`.  This option is deprecated.  The
  :option:`-mmcu=` option should be used to select the ISA.

.. option:: -msim

  Link to the simulator runtime libraries and linker script.  Overrides
  any scripts that would be selected by the :option:`-mmcu=` option.

.. option:: -mlarge

  Use large-model addressing (20-bit pointers, 20-bit ``size_t``).

.. option:: -msmall

  Use small-model addressing (16-bit pointers, 16-bit ``size_t``).

.. option:: -mrelax

  This option is passed to the assembler and linker, and allows the
  linker to perform certain optimizations that cannot be done until
  the final link.

.. option:: mhwmult=

  Describes the type of hardware multiply supported by the target.
  Accepted values are :samp:`none` for no hardware multiply, :samp:`16bit`
  for the original 16-bit-only multiply supported by early MCUs.
  :samp:`32bit` for the 16/32-bit multiply supported by later MCUs and
  :samp:`f5series` for the 16/32-bit multiply supported by F5-series MCUs.
  A value of :samp:`auto` can also be given.  This tells GCC to deduce
  the hardware multiply support based upon the MCU name provided by the
  :option:`-mmcu` option.  If no :option:`-mmcu` option is specified or if
  the MCU name is not recognized then no hardware multiply support is
  assumed.  ``auto`` is the default setting.

  Hardware multiplies are normally performed by calling a library
  routine.  This saves space in the generated code.  When compiling at
  :option:`-O3` or higher however the hardware multiplier is invoked
  inline.  This makes for bigger, but faster code.

  The hardware multiply routines disable interrupts whilst running and
  restore the previous interrupt state when they finish.  This makes
  them safe to use inside interrupt handlers as well as in normal code.

.. option:: -minrt

  Enable the use of a minimum runtime environment - no static
  initializers or constructors.  This is intended for memory-constrained
  devices.  The compiler includes special symbols in some objects
  that tell the linker and runtime which code fragments are required.

.. option:: -mtiny-printf

  Enable reduced code size ``printf`` and ``puts`` library functions.
  The :samp:`tiny` implementations of these functions are not reentrant, so
  must be used with caution in multi-threaded applications.

  Support for streams has been removed and the string to be printed will
  always be sent to stdout via the ``write`` syscall.  The string is not
  buffered before it is sent to write.

  This option requires Newlib Nano IO, so GCC must be configured with
  :samp:`--enable-newlib-nano-formatted-io`.

.. option:: -mmax-inline-shift=

  This option takes an integer between 0 and 64 inclusive, and sets
  the maximum number of inline shift instructions which should be emitted to
  perform a shift operation by a constant amount.  When this value needs to be
  exceeded, an mspabi helper function is used instead.  The default value is 4.

  This only affects cases where a shift by multiple positions cannot be
  completed with a single instruction (e.g. all shifts >1 on the 430 ISA).

  Shifts of a 32-bit value are at least twice as costly, so the value passed for
  this option is divided by 2 and the resulting value used instead.

.. option:: -mcode-region=

  These options tell the compiler where to place functions and data that
  do not have one of the :msp430-fn-attr:`lower`, :msp430-fn-attr:`upper`, ``either`` or
  ``section`` attributes.  Possible values are :msp430-fn-attr:`lower`,
  :msp430-fn-attr:`upper`, ``either`` or ``any``.  The first three behave
  like the corresponding attribute.  The fourth possible value -
  ``any`` - is the default.  It leaves placement entirely up to the
  linker script and how it assigns the standard sections
  (``.text``, ``.data``, etc) to the memory regions.

.. option:: -msilicon-errata=

  This option passes on a request to assembler to enable the fixes for
  the named silicon errata.

.. option:: -msilicon-errata-warn=

  This option passes on a request to the assembler to enable warning
  messages when a silicon errata might need to be applied.

.. option:: -mwarn-devices-csv, -mno-warn-devices-csv

  Warn if :samp:`devices.csv` is not found or there are problem parsing it
  (default: on).
