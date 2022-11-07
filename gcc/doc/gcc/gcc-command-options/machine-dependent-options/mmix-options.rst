..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MMIX

.. index:: MMIX Options

.. _mmix-options:

MMIX Options
^^^^^^^^^^^^

These options are defined for the MMIX:

.. option:: -mlibfuncs, -mno-libfuncs

  Specify that intrinsic library functions are being compiled, passing all
  values in registers, no matter the size.

.. option:: -mepsilon, -mno-epsilon

  Generate floating-point comparison instructions that compare with respect
  to the ``rE`` epsilon register.

.. option:: -mabi=mmixware

  Generate code that passes function parameters and return values that (in
  the called function) are seen as registers ``$0`` and up, as opposed to
  the GNU ABI which uses global registers ``$231`` and up.

.. option:: -mzero-extend, -mno-zero-extend

  When reading data from memory in sizes shorter than 64 bits, use (do not
  use) zero-extending load instructions by default, rather than
  sign-extending ones.

.. option:: -mknuthdiv, -mno-knuthdiv

  Make the result of a division yielding a remainder have the same sign as
  the divisor.  With the default, :option:`-mno-knuthdiv`, the sign of the
  remainder follows the sign of the dividend.  Both methods are
  arithmetically valid, the latter being almost exclusively used.

.. option:: -mtoplevel-symbols, -mno-toplevel-symbols

  Prepend (do not prepend) a :samp:`:` to all global symbols, so the assembly
  code can be used with the ``PREFIX`` assembly directive.

.. option:: -melf

  Generate an executable in the ELF format, rather than the default
  :samp:`mmo` format used by the :command:`mmix` simulator.

.. option:: -mbranch-predict, -mno-branch-predict

  Use (do not use) the probable-branch instructions, when static branch
  prediction indicates a probable branch.

.. option:: -mbase-addresses, -mno-base-addresses

  Generate (do not generate) code that uses *base addresses*.  Using a
  base address automatically generates a request (handled by the assembler
  and the linker) for a constant to be set up in a global register.  The
  register is used for one or more base address requests within the range 0
  to 255 from the value held in the register.  The generally leads to short
  and fast code, but the number of different data items that can be
  addressed is limited.  This means that a program that uses lots of static
  data may require :option:`-mno-base-addresses`.

.. option:: -msingle-exit, -mno-single-exit

  Force (do not force) generated code to have a single exit point in each
  function.