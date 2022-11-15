..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Visium

.. index:: Visium options

.. _visium-options:

Visium Options
^^^^^^^^^^^^^^

.. option:: -mdebug

  A program which performs file I/O and is destined to run on an MCM target
  should be linked with this option.  It causes the libraries libc.a and
  libdebug.a to be linked.  The program should be run on the target under
  the control of the GDB remote debugging stub.

.. option:: -msim

  A program which performs file I/O and is destined to run on the simulator
  should be linked with option.  This causes libraries libc.a and libsim.a to
  be linked.

.. option:: -mfpu, -mhard-float

  Generate code containing floating-point instructions.  This is the
  default.

.. option:: -mno-fpu, -msoft-float

  Generate code containing library calls for floating-point.

  :option:`-msoft-float` changes the calling convention in the output file;
  therefore, it is only useful if you compile *all* of a program with
  this option.  In particular, you need to compile :samp:`libgcc.a`, the
  library that comes with GCC, with :option:`-msoft-float` in order for
  this to work.

.. option:: -mcpu={cpu_type}

  Set the instruction set, register set, and instruction scheduling parameters
  for machine type :samp:`{cpu_type}`.  Supported values for :samp:`{cpu_type}` are
  :samp:`mcm`, :samp:`gr5` and :samp:`gr6`.

  :samp:`mcm` is a synonym of :samp:`gr5` present for backward compatibility.

  By default (unless configured otherwise), GCC generates code for the GR5
  variant of the Visium architecture.

  With :option:`-mcpu=gr6`, GCC generates code for the GR6 variant of the Visium
  architecture.  The only difference from GR5 code is that the compiler will
  generate block move instructions.

.. option:: -mtune={cpu_type}

  Set the instruction scheduling parameters for machine type :samp:`{cpu_type}`,
  but do not set the instruction set or register set that the option
  :option:`-mcpu=cpu_type` would.

.. option:: -msv-mode

  Generate code for the supervisor mode, where there are no restrictions on
  the access to general registers.  This is the default.

.. option:: -muser-mode

  Generate code for the user mode, where the access to some general registers
  is forbidden: on the GR5, registers r24 to r31 cannot be accessed in this
  mode; on the GR6, only registers r29 to r31 are affected.
