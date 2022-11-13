..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: picoChip

.. index:: picoChip options

.. _picochip-options:

picoChip Options
^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for picoChip implementations:

.. option:: -mae={ae_type}

  Set the instruction set, register set, and instruction scheduling
  parameters for array element type :samp:`{ae_type}`.  Supported values
  for :samp:`{ae_type}` are :samp:`ANY`, :samp:`MUL`, and :samp:`MAC`.

  :option:`-mae=ANY` selects a completely generic AE type.  Code
  generated with this option runs on any of the other AE types.  The
  code is not as efficient as it would be if compiled for a specific
  AE type, and some types of operation (e.g., multiplication) do not
  work properly on all types of AE.

  :option:`-mae=MUL` selects a MUL AE type.  This is the most useful AE type
  for compiled code, and is the default.

  :option:`-mae=MAC` selects a DSP-style MAC AE.  Code compiled with this
  option may suffer from poor performance of byte (char) manipulation,
  since the DSP AE does not provide hardware support for byte load/stores.

.. option:: -msymbol-as-address

  Enable the compiler to directly use a symbol name as an address in a
  load/store instruction, without first loading it into a
  register.  Typically, the use of this option generates larger
  programs, which run faster than when the option isn't used.  However, the
  results vary from program to program, so it is left as a user option,
  rather than being permanently enabled.

.. option:: -mno-inefficient-warnings

  Disables warnings about the generation of inefficient code.  These
  warnings can be generated, for example, when compiling code that
  performs byte-level memory operations on the MAC AE type.  The MAC AE has
  no hardware support for byte-level memory operations, so all byte
  load/stores must be synthesized from word load/store operations.  This is
  inefficient and a warning is generated to indicate
  that you should rewrite the code to avoid byte operations, or to target
  an AE type that has the necessary hardware support.  This option disables
  these warnings.