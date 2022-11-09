..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MeP

.. index:: MeP options

.. _mep-options:

MeP Options
^^^^^^^^^^^

.. option:: -mabsdiff

  Enables the ``abs`` instruction, which is the absolute difference
  between two registers.

.. option:: -mall-opts

  Enables all the optional instructions---average, multiply, divide, bit
  operations, leading zero, absolute difference, min/max, clip, and
  saturation.

.. option:: -maverage

  Enables the ``ave`` instruction, which computes the average of two
  registers.

.. option:: -mbased={n}

  Variables of size :samp:`{n}` bytes or smaller are placed in the
  ``.based`` section by default.  Based variables use the ``$tp``
  register as a base register, and there is a 128-byte limit to the
  ``.based`` section.

.. option:: -mbitops

  Enables the bit operation instructions---bit test (``btstm``), set
  (``bsetm``), clear (``bclrm``), invert (``bnotm``), and
  test-and-set (``tas``).

.. option:: -mc={name}

  Selects which section constant data is placed in.  :samp:`{name}` may
  be :samp:`tiny`, :samp:`near`, or :samp:`far`.

.. option:: -mclip

  Enables the ``clip`` instruction.  Note that :option:`-mclip` is not
  useful unless you also provide :option:`-mminmax`.

.. option:: -mconfig={name}

  Selects one of the built-in core configurations.  Each MeP chip has
  one or more modules in it; each module has a core CPU and a variety of
  coprocessors, optional instructions, and peripherals.  The
  ``MeP-Integrator`` tool, not part of GCC, provides these
  configurations through this option; using this option is the same as
  using all the corresponding command-line options.  The default
  configuration is :samp:`default`.

.. option:: -mcop

  Enables the coprocessor instructions.  By default, this is a 32-bit
  coprocessor.  Note that the coprocessor is normally enabled via the
  :option:`-mconfig=` option.

.. option:: -mcop32

  Enables the 32-bit coprocessor's instructions.

.. option:: -mcop64

  Enables the 64-bit coprocessor's instructions.

.. option:: -mivc2

  Enables IVC2 scheduling.  IVC2 is a 64-bit VLIW coprocessor.

.. option:: -mdc

  Causes constant variables to be placed in the ``.near`` section.

.. option:: -mdiv

  Enables the ``div`` and ``divu`` instructions.

.. option:: -meb

  Generate big-endian code.

.. option:: -mel

  Generate little-endian code.

.. option:: -mio-volatile

  Tells the compiler that any variable marked with the :mep-var-attr:`io`
  attribute is to be considered volatile.

.. option:: -ml

  Causes variables to be assigned to the ``.far`` section by default.

.. option:: -mleadz

  Enables the ``leadz`` (leading zero) instruction.

.. option:: -mm

  Causes variables to be assigned to the ``.near`` section by default.

.. option:: -mminmax

  Enables the ``min`` and ``max`` instructions.

.. option:: -mmult

  Enables the multiplication and multiply-accumulate instructions.

.. option:: -mno-opts

  Disables all the optional instructions enabled by :option:`-mall-opts`.

.. option:: -mrepeat

  Enables the ``repeat`` and ``erepeat`` instructions, used for
  low-overhead looping.

.. option:: -ms

  Causes all variables to default to the ``.tiny`` section.  Note
  that there is a 65536-byte limit to this section.  Accesses to these
  variables use the ``%gp`` base register.

.. option:: -msatur

  Enables the saturation instructions.  Note that the compiler does not
  currently generate these itself, but this option is included for
  compatibility with other tools, like ``as``.

.. option:: -msdram

  Link the SDRAM-based runtime instead of the default ROM-based runtime.

.. option:: -msim

  Link the simulator run-time libraries.

.. option:: -msimnovec

  Link the simulator runtime libraries, excluding built-in support
  for reset and exception vectors and tables.

.. option:: -mtf

  Causes all functions to default to the ``.far`` section.  Without
  this option, functions default to the ``.near`` section.

.. option:: -mtiny={n}

  Variables that are :samp:`{n}` bytes or smaller are allocated to the
  ``.tiny`` section.  These variables use the ``$gp`` base
  register.  The default for this option is 4, but note that there's a
  65536-byte limit to the ``.tiny`` section.
