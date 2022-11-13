..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: MN10300

.. index:: MN10300 options

.. _mn10300-options:

MN10300 Options
^^^^^^^^^^^^^^^

These :option:`-m` options are defined for Matsushita MN10300 architectures:

.. option:: -mmult-bug

  Generate code to avoid bugs in the multiply instructions for the MN10300
  processors.  This is the default.

.. option:: -mno-mult-bug

  Do not generate code to avoid bugs in the multiply instructions for the
  MN10300 processors.

.. option:: -mam33

  Generate code using features specific to the AM33 processor.

.. option:: -mno-am33

  Do not generate code using features specific to the AM33 processor.  This
  is the default.

.. option:: -mam33-2

  Generate code using features specific to the AM33/2.0 processor.

.. option:: -mam34

  Generate code using features specific to the AM34 processor.

.. option:: -mtune={cpu-type}

  Use the timing characteristics of the indicated CPU type when
  scheduling instructions.  This does not change the targeted processor
  type.  The CPU type must be one of :samp:`mn10300`, :samp:`am33`,
  :samp:`am33-2` or :samp:`am34`.

.. option:: -mreturn-pointer-on-d0

  When generating a function that returns a pointer, return the pointer
  in both ``a0`` and ``d0``.  Otherwise, the pointer is returned
  only in ``a0``, and attempts to call such functions without a prototype
  result in errors.  Note that this option is on by default; use
  :option:`-mno-return-pointer-on-d0` to disable it.

.. option:: -mno-crt0

  Do not link in the C run-time initialization object file.

.. option:: -mrelax

  Indicate to the linker that it should perform a relaxation optimization pass
  to shorten branches, calls and absolute memory addresses.  This option only
  has an effect when used on the command line for the final link step.

  This option makes symbolic debugging impossible.

.. option:: -mliw

  Allow the compiler to generate *Long Instruction Word*
  instructions if the target is the :samp:`AM33` or later.  This is the
  default.  This option defines the preprocessor macro ``__LIW__``.

.. option:: -mno-liw

  Do not allow the compiler to generate *Long Instruction Word*
  instructions.  This option defines the preprocessor macro
  ``__NO_LIW__``.

.. option:: -msetlb

  Allow the compiler to generate the *SETLB* and *Lcc*
  instructions if the target is the :samp:`AM33` or later.  This is the
  default.  This option defines the preprocessor macro ``__SETLB__``.

.. option:: -mno-setlb

  Do not allow the compiler to generate *SETLB* or *Lcc*
  instructions.  This option defines the preprocessor macro
  ``__NO_SETLB__``.