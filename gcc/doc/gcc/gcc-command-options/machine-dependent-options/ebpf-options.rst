..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: eBPF

.. index:: eBPF Options

.. _ebpf-options:

eBPF Options
^^^^^^^^^^^^

.. index:: eBPF Options

.. option:: -mframe-limit=bytes

  This specifies the hard limit for frame sizes, in bytes.  Currently,
  the value that can be specified should be less than or equal to
  :samp:`32767`.  Defaults to whatever limit is imposed by the version of
  the Linux kernel targeted.

.. option:: -mkernel={version}

  This specifies the minimum version of the kernel that will run the
  compiled program.  GCC uses this version to determine which
  instructions to use, what kernel helpers to allow, etc.  Currently,
  :samp:`{version}` can be one of :samp:`4.0`, :samp:`4.1`, :samp:`4.2`,
  :samp:`4.3`, :samp:`4.4`, :samp:`4.5`, :samp:`4.6`, :samp:`4.7`,
  :samp:`4.8`, :samp:`4.9`, :samp:`4.10`, :samp:`4.11`, :samp:`4.12`,
  :samp:`4.13`, :samp:`4.14`, :samp:`4.15`, :samp:`4.16`, :samp:`4.17`,
  :samp:`4.18`, :samp:`4.19`, :samp:`4.20`, :samp:`5.0`, :samp:`5.1`,
  :samp:`5.2`, :samp:`latest` and :samp:`native`.

.. option:: -mbig-endian

  Generate code for a big-endian target.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default.

.. option:: -mjmpext

  Enable generation of extra conditional-branch instructions.
  Enabled for CPU v2 and above.

.. option:: -mjmp32

  Enable 32-bit jump instructions. Enabled for CPU v3 and above.

.. option:: -malu32

  Enable 32-bit ALU instructions. Enabled for CPU v3 and above.

.. option:: -mcpu={version}

  This specifies which version of the eBPF ISA to target. Newer versions
  may not be supported by all kernels. The default is :samp:`v3`.

  Supported values for :samp:`{version}` are:

  :samp:`v1`
    The first stable eBPF ISA with no special features or extensions.

  :samp:`v2`
    Supports the jump extensions, as in :option:`-mjmpext`.

  :samp:`v3`
    All features of v2, plus:

    * 32-bit jump operations, as in :option:`-mjmp32`

    * 32-bit ALU operations, as in :option:`-malu32`

.. option:: -mco-re

  Enable BPF Compile Once - Run Everywhere (CO-RE) support. Requires and
  is implied by :option:`-gbtf`.

.. option:: -mno-co-re

  Disable BPF Compile Once - Run Everywhere (CO-RE) support. BPF CO-RE
  support is enabled by default when generating BTF debug information for
  the BPF target.

.. option:: -mxbpf

  Generate code for an expanded version of BPF, which relaxes some of
  the restrictions imposed by the BPF architecture:

  * Save and restore callee-saved registers at function entry and
    exit, respectively.
