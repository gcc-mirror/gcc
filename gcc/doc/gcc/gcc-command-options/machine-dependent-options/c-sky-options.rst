..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: C-SKY

.. index:: C-SKY Options

.. _c-sky-options:

C-SKY Options
^^^^^^^^^^^^^

GCC supports these options when compiling for C-SKY V2 processors.

.. option:: -march={arch}

  Specify the C-SKY target architecture.  Valid values for :samp:`{arch}` are:
  :samp:`ck801`, :samp:`ck802`, :samp:`ck803`, :samp:`ck807`, and :samp:`ck810`.
  The default is :samp:`ck810`.

.. option:: -mcpu={cpu}

  Specify the C-SKY target processor.  Valid values for :samp:`{cpu}` are:
  :samp:`ck801`, :samp:`ck801t`,
  :samp:`ck802`, :samp:`ck802t`, :samp:`ck802j`,
  :samp:`ck803`, :samp:`ck803h`, :samp:`ck803t`, :samp:`ck803ht`,
  :samp:`ck803f`, :samp:`ck803fh`, :samp:`ck803e`, :samp:`ck803eh`,
  :samp:`ck803et`, :samp:`ck803eht`, :samp:`ck803ef`, :samp:`ck803efh`,
  :samp:`ck803ft`, :samp:`ck803eft`, :samp:`ck803efht`, :samp:`ck803r1`,
  :samp:`ck803hr1`, :samp:`ck803tr1`, :samp:`ck803htr1`, :samp:`ck803fr1`,
  :samp:`ck803fhr1`, :samp:`ck803er1`, :samp:`ck803ehr1`, :samp:`ck803etr1`,
  :samp:`ck803ehtr1`, :samp:`ck803efr1`, :samp:`ck803efhr1`, :samp:`ck803ftr1`,
  :samp:`ck803eftr1`, :samp:`ck803efhtr1`,
  :samp:`ck803s`, :samp:`ck803st`, :samp:`ck803se`, :samp:`ck803sf`,
  :samp:`ck803sef`, :samp:`ck803seft`,
  :samp:`ck807e`, :samp:`ck807ef`, :samp:`ck807`, :samp:`ck807f`,
  :samp:`ck810e`, :samp:`ck810et`, :samp:`ck810ef`, :samp:`ck810eft`,
  :samp:`ck810`, :samp:`ck810v`, :samp:`ck810f`, :samp:`ck810t`, :samp:`ck810fv`,
  :samp:`ck810tv`, :samp:`ck810ft`, and :samp:`ck810ftv`.

.. option:: -mbig-endian, -EB, -mlittle-endian, -EL

  Select big- or little-endian code.  The default is little-endian.

.. option:: -mfloat-abi={name}

  Specifies which floating-point ABI to use.  Permissible values
  are: :samp:`soft`, :samp:`softfp` and :samp:`hard`.

  Specifying :samp:`soft` causes GCC to generate output containing
  library calls for floating-point operations.
  :samp:`softfp` allows the generation of code using hardware floating-point
  instructions, but still uses the soft-float calling conventions.
  :samp:`hard` allows generation of floating-point instructions
  and uses FPU-specific calling conventions.

  The default depends on the specific target configuration.  Note that
  the hard-float and soft-float ABIs are not link-compatible; you must
  compile your entire program with the same ABI, and link with a
  compatible set of libraries.

.. option:: -mhard-float, -msoft-float

  Select hardware or software floating-point implementations.
  The default is soft float.

.. option:: -mdouble-float, -mno-double-float

  When :option:`-mhard-float` is in effect, enable generation of
  double-precision float instructions.  This is the default except
  when compiling for CK803.

.. option:: -mfdivdu, -mno-fdivdu

  When :option:`-mhard-float` is in effect, enable generation of
  ``frecipd``, ``fsqrtd``, and ``fdivd`` instructions.
  This is the default except when compiling for CK803.

.. option:: -mfpu={fpu}

  Select the floating-point processor.  This option can only be used with
  :option:`-mhard-float`.
  Values for :samp:`{fpu}` are
  :samp:`fpv2_sf` (equivalent to :samp:`-mno-double-float -mno-fdivdu`),
  :samp:`fpv2` (:samp:`-mdouble-float -mno-divdu`), and
  :samp:`fpv2_divd` (:samp:`-mdouble-float -mdivdu`).

.. option:: -melrw, -mno-elrw

  Enable the extended ``lrw`` instruction.  This option defaults to on
  for CK801 and off otherwise.

.. option:: -mistack, -mno-istack

  Enable interrupt stack instructions; the default is off.

  The :option:`-mistack` option is required to handle the
  :c-sky-fn-attr:`interrupt` and :c-sky-fn-attr:`isr` function attributes
  (see :ref:`c-sky-function-attributes`).

.. option:: -mmp

  Enable multiprocessor instructions; the default is off.

.. option:: -mcp

  Enable coprocessor instructions; the default is off.

.. option:: -mcache

  Enable coprocessor instructions; the default is off.

.. option:: -msecurity

  Enable C-SKY security instructions; the default is off.

.. option:: -mtrust

  Enable C-SKY trust instructions; the default is off.

.. option:: -mdsp, -medsp, -mvdsp

  Enable C-SKY DSP, Enhanced DSP, or Vector DSP instructions, respectively.
  All of these options default to off.

.. option:: -mdiv, -mno-div

  Generate divide instructions.  Default is off.

.. option:: -msmart, -mno-smart

  Generate code for Smart Mode, using only registers numbered 0-7 to allow
  use of 16-bit instructions.  This option is ignored for CK801 where this
  is the required behavior, and it defaults to on for CK802.
  For other targets, the default is off.

.. option:: -mhigh-registers, -mno-high-registers

  Generate code using the high registers numbered 16-31.  This option
  is not supported on CK801, CK802, or CK803, and is enabled by default
  for other processors.

.. option:: -manchor, -mno-anchor

  Generate code using global anchor symbol addresses.

.. option:: -mpushpop, -mno-pushpop

  Generate code using ``push`` and ``pop`` instructions.  This option
  defaults to on.

.. option:: -mmultiple-stld, -mstm, -mno-multiple-stld, -mno-stm

  Generate code using ``stm`` and ``ldm`` instructions.  This option
  isn't supported on CK801 but is enabled by default on other processors.

.. option:: -mconstpool, -mno-constpool

  Create constant pools in the compiler instead of deferring it to the
  assembler.  This option is the default and required for correct code
  generation on CK801 and CK802, and is optional on other processors.

.. option:: -mstack-size, -mno-stack-size

  Emit ``.stack_size`` directives for each function in the assembly
  output.  This option defaults to off.

.. option:: -mstack-size

  Default setting; overrides :option:`-mno-stack-size`.

.. option:: -mccrt, -mno-ccrt

  Generate code for the C-SKY compiler runtime instead of libgcc.  This
  option defaults to off.

.. option:: -mbranch-cost={n}

  Set the branch costs to roughly ``n`` instructions.  The default is 1.

.. option:: -msched-prolog, -mno-sched-prolog

  Permit scheduling of function prologue and epilogue sequences.  Using
  this option can result in code that is not compliant with the C-SKY V2 ABI
  prologue requirements and that cannot be debugged or backtraced.
  It is disabled by default.

.. option:: -msim

  Links the library libsemi.a which is in compatible with simulator. Applicable
  to ELF compiler only.
