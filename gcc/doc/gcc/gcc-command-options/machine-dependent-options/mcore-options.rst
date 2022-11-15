..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MCore

.. index:: MCore options

.. _mcore-options:

MCore Options
^^^^^^^^^^^^^

These are the :samp:`-m` options defined for the Motorola M\*Core
processors.

.. option:: -mhardlit, -mno-hardlit

  Inline constants into the code stream if it can be done in two
  instructions or less.

.. option:: -mdiv, -mno-div

  Use the divide instruction.  (Enabled by default).

.. option:: -mrelax-immediate, -mno-relax-immediate

  Allow arbitrary-sized immediates in bit operations.

.. option:: -mwide-bitfields, -mno-wide-bitfields

  Always treat bit-fields as ``int`` -sized.

.. option:: -m4byte-functions, -mno-4byte-functions

  Force all functions to be aligned to a 4-byte boundary.

.. option:: -mcallgraph-data, -mno-callgraph-data

  Emit callgraph information.

.. option:: -mslow-bytes, -mno-slow-bytes

  Prefer word access when reading byte quantities.

.. option:: -mlittle-endian, -mbig-endian

  Generate code for a little-endian target.

.. option:: -m210, -m340

  Generate code for the 210 processor.

.. option:: -mno-lsim

  Assume that runtime support has been provided and so omit the
  simulator library (:samp:`libsim.a)` from the linker command line.

.. option:: -mstack-increment={size}

  Set the maximum amount for a single stack increment operation.  Large
  values can increase the speed of programs that contain functions
  that need a large amount of stack space, but they can also trigger a
  segmentation fault if the stack is extended too much.  The default
  value is 0x1000.
