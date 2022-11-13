..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: submodel options, specifying hardware config, hardware models and configurations, specifying, target-dependent options, machine-dependent options

.. _submodel-options:

Machine-Dependent Options
*************************

Each target machine supported by GCC can have its own options---for
example, to allow you to compile for a particular processor variant or
ABI, or to control optimizations specific to that machine.  By
convention, the names of machine-specific options start with
:samp:`-m`.

Some configurations of the compiler also support additional target-specific
options, usually for compatibility with other compilers on the same
platform.

.. This list is ordered alphanumerically by subsection name.
   It should be the same order and spelling as these options are listed
   in Machine Dependent Options

.. toctree::
  :maxdepth: 1

  machine-dependent-options/aarch64-options
  machine-dependent-options/adapteva-epiphany-options
  machine-dependent-options/amd-gcn-options
  machine-dependent-options/arc-options
  machine-dependent-options/arm-options
  machine-dependent-options/avr-options
  machine-dependent-options/blackfin-options
  machine-dependent-options/c6x-options
  machine-dependent-options/cris-options
  machine-dependent-options/c-sky-options
  machine-dependent-options/darwin-options
  machine-dependent-options/dec-alpha-options
  machine-dependent-options/ebpf-options
  machine-dependent-options/fr30-options
  machine-dependent-options/ft32-options
  machine-dependent-options/frv-options
  machine-dependent-options/gnu-linux-options
  machine-dependent-options/h8-300-options
  machine-dependent-options/hppa-options
  machine-dependent-options/ia-64-options
  machine-dependent-options/lm32-options
  machine-dependent-options/loongarch-options
  machine-dependent-options/m32c-options
  machine-dependent-options/m32r-d-options
  machine-dependent-options/m680x0-options
  machine-dependent-options/mcore-options
  machine-dependent-options/mep-options
  machine-dependent-options/microblaze-options
  machine-dependent-options/mips-options
  machine-dependent-options/mmix-options
  machine-dependent-options/mn10300-options
  machine-dependent-options/moxie-options
  machine-dependent-options/msp430-options
  machine-dependent-options/nds32-options
  machine-dependent-options/nios-ii-options
  machine-dependent-options/nvidia-ptx-options
  machine-dependent-options/openrisc-options
  machine-dependent-options/pdp-11-options
  machine-dependent-options/picochip-options
  machine-dependent-options/powerpc-options
  machine-dependent-options/pru-options
  machine-dependent-options/risc-v-options
  machine-dependent-options/rl78-options
  machine-dependent-options/ibm-rs-6000-and-powerpc-options
  machine-dependent-options/rx-options
  machine-dependent-options/s-390-and-zseries-options
  machine-dependent-options/score-options
  machine-dependent-options/sh-options
  machine-dependent-options/solaris-2-options
  machine-dependent-options/sparc-options
  machine-dependent-options/options-for-system-v
  machine-dependent-options/v850-options
  machine-dependent-options/vax-options
  machine-dependent-options/visium-options
  machine-dependent-options/vms-options
  machine-dependent-options/vxworks-options
  machine-dependent-options/x86-options
  machine-dependent-options/x86-windows-options
  machine-dependent-options/xstormy16-options
  machine-dependent-options/xtensa-options
  machine-dependent-options/zseries-options

.. program:: None