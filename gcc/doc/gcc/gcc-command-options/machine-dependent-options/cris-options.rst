..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: CRIS

.. index:: CRIS Options

.. _cris-options:

CRIS Options
^^^^^^^^^^^^

These options are defined specifically for the CRIS ports.

.. option:: -march={architecture-type}

  Generate code for the specified architecture.  The choices for
  :samp:`{architecture-type}` are :samp:`v3`, :samp:`v8` and :samp:`v10` for
  respectively ETRAX4, ETRAX100, and ETRAX100LX.
  Default is :samp:`v0`.

.. option:: -mtune={architecture-type}

  Tune to :samp:`{architecture-type}` everything applicable about the generated
  code, except for the ABI and the set of available instructions.  The
  choices for :samp:`{architecture-type}` are the same as for
  :option:`-march=architecture-type`.

.. option:: -mmax-stack-frame={n}

  Warn when the stack frame of a function exceeds :samp:`{n}` bytes.

.. option:: -metrax4, -metrax100

  The options :option:`-metrax4` and :option:`-metrax100` are synonyms for
  :option:`-march=v3` and :option:`-march=v8` respectively.

.. option:: -mmul-bug-workaround, -mno-mul-bug-workaround

  Work around a bug in the ``muls`` and ``mulu`` instructions for CPU
  models where it applies.  This option is disabled by default.

.. option:: -mpdebug

  Enable CRIS-specific verbose debug-related information in the assembly
  code.  This option also has the effect of turning off the :samp:`#NO_APP`
  formatted-code indicator to the assembler at the beginning of the
  assembly file.

.. option:: -mcc-init

  Do not use condition-code results from previous instruction; always emit
  compare and test instructions before use of condition codes.

.. option:: -mno-side-effects

  Do not emit instructions with side effects in addressing modes other than
  post-increment.

.. option:: -mside-effects

  Default setting; overrides :option:`-mno-side-effects`.

.. option:: -mstack-align, -mno-stack-align, -mdata-align, -mno-data-align, -mconst-align, -mno-const-align

  These options (:samp:`no-` options) arrange (eliminate arrangements) for the
  stack frame, individual data and constants to be aligned for the maximum
  single data access size for the chosen CPU model.  The default is to
  arrange for 32-bit alignment.  ABI details such as structure layout are
  not affected by these options.

.. option:: -m32-bit, -m16-bit, -m8-bit

  Similar to the stack- data- and const-align options above, these options
  arrange for stack frame, writable data and constants to all be 32-bit,
  16-bit or 8-bit aligned.  The default is 32-bit alignment.

.. option:: -mno-prologue-epilogue, -mprologue-epilogue

  With :option:`-mno-prologue-epilogue`, the normal function prologue and
  epilogue which set up the stack frame are omitted and no return
  instructions or return sequences are generated in the code.  Use this
  option only together with visual inspection of the compiled code: no
  warnings or errors are generated when call-saved registers must be saved,
  or storage for local variables needs to be allocated.

.. option:: -melf

  Legacy no-op option.

.. option:: -sim

  This option arranges
  to link with input-output functions from a simulator library.  Code,
  initialized data and zero-initialized data are allocated consecutively.

.. option:: -sim2

  Like :option:`-sim`, but pass linker options to locate initialized data at
  0x40000000 and zero-initialized data at 0x80000000.