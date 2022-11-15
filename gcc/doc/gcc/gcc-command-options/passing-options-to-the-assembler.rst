..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _assembler-options:

Passing Options to the Assembler
********************************

.. prevent bad page break with this line

You can pass options to the assembler.

.. option:: -Wa,option

  Pass :samp:`{option}` as an option to the assembler.  If :samp:`{option}`
  contains commas, it is split into multiple options at the commas.

.. option:: -Xassembler {option}

  Pass :samp:`{option}` as an option to the assembler.  You can use this to
  supply system-specific assembler options that GCC does not
  recognize.

  If you want to pass an option that takes an argument, you must use
  :option:`-Xassembler` twice, once for the option and once for the argument.
