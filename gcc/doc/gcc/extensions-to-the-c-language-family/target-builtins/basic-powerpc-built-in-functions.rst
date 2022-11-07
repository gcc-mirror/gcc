..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _basic-powerpc-built-in-functions:

Basic PowerPC Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::
  :maxdepth: 2


This section describes PowerPC built-in functions that do not require
the inclusion of any special header files to declare prototypes or
provide macro definitions.  The sections that follow describe
additional PowerPC built-in functions.

.. _basic-powerpc-built-in-functions-available-on-all-configurations:

Basic PowerPC Built-in Functions Available on all Configurations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: void __builtin_cpu_init (void)

  This function is a ``nop`` on the PowerPC platform and is included solely
  to maintain API compatibility with the x86 builtins.

.. function:: int __builtin_cpu_is (const char *cpuname)

  This function returns a value of ``1`` if the run-time CPU is of type
  :samp:`{cpuname}` and returns ``0`` otherwise

  The ``__builtin_cpu_is`` function requires GLIBC 2.23 or newer
  which exports the hardware capability bits.  GCC defines the macro
  ``__BUILTIN_CPU_SUPPORTS__`` if the ``__builtin_cpu_supports``
  built-in function is fully supported.

  If GCC was configured to use a GLIBC before 2.23, the built-in
  function ``__builtin_cpu_is`` always returns a 0 and the compiler
  issues a warning.

  The following CPU names can be detected:

  :samp:`power10`
    IBM POWER10 Server CPU.

  :samp:`power9`
    IBM POWER9 Server CPU.

  :samp:`power8`
    IBM POWER8 Server CPU.

  :samp:`power7`
    IBM POWER7 Server CPU.

  :samp:`power6x`
    IBM POWER6 Server CPU (RAW mode).

  :samp:`power6`
    IBM POWER6 Server CPU (Architected mode).

  :samp:`power5+`
    IBM POWER5+ Server CPU.

  :samp:`power5`
    IBM POWER5 Server CPU.

  :samp:`ppc970`
    IBM 970 Server CPU (ie, Apple G5).

  :samp:`power4`
    IBM POWER4 Server CPU.

  :samp:`ppca2`
    IBM A2 64-bit Embedded CPU

  :samp:`ppc476`
    IBM PowerPC 476FP 32-bit Embedded CPU.

  :samp:`ppc464`
    IBM PowerPC 464 32-bit Embedded CPU.

  :samp:`ppc440`
    PowerPC 440 32-bit Embedded CPU.

  :samp:`ppc405`
    PowerPC 405 32-bit Embedded CPU.

  :samp:`ppc-cell-be`
    IBM PowerPC Cell Broadband Engine Architecture CPU.

    Here is an example:

  .. code-block:: c++

    #ifdef __BUILTIN_CPU_SUPPORTS__
      if (__builtin_cpu_is ("power8"))
        {
           do_power8 (); // POWER8 specific implementation.
        }
      else
    #endif
        {
           do_generic (); // Generic implementation.
        }

.. function:: int __builtin_cpu_supports (const char *feature)

  This function returns a value of ``1`` if the run-time CPU supports the HWCAP
  feature :samp:`{feature}` and returns ``0`` otherwise.

  The ``__builtin_cpu_supports`` function requires GLIBC 2.23 or
  newer which exports the hardware capability bits.  GCC defines the
  macro ``__BUILTIN_CPU_SUPPORTS__`` if the
  ``__builtin_cpu_supports`` built-in function is fully supported.

  If GCC was configured to use a GLIBC before 2.23, the built-in
  function ``__builtin_cpu_supports`` always returns a 0 and the
  compiler issues a warning.

  The following features can be
  detected:

  :samp:`4xxmac`
    4xx CPU has a Multiply Accumulator.

  :samp:`altivec`
    CPU has a SIMD/Vector Unit.

  :samp:`arch_2_05`
    CPU supports ISA 2.05 (eg, POWER6)

  :samp:`arch_2_06`
    CPU supports ISA 2.06 (eg, POWER7)

  :samp:`arch_2_07`
    CPU supports ISA 2.07 (eg, POWER8)

  :samp:`arch_3_00`
    CPU supports ISA 3.0 (eg, POWER9)

  :samp:`arch_3_1`
    CPU supports ISA 3.1 (eg, POWER10)

  :samp:`archpmu`
    CPU supports the set of compatible performance monitoring events.

  :samp:`booke`
    CPU supports the Embedded ISA category.

  :samp:`cellbe`
    CPU has a CELL broadband engine.

  :samp:`darn`
    CPU supports the ``darn`` (deliver a random number) instruction.

  :samp:`dfp`
    CPU has a decimal floating point unit.

  :samp:`dscr`
    CPU supports the data stream control register.

  :samp:`ebb`
    CPU supports event base branching.

  :samp:`efpdouble`
    CPU has a SPE double precision floating point unit.

  :samp:`efpsingle`
    CPU has a SPE single precision floating point unit.

  :samp:`fpu`
    CPU has a floating point unit.

  :samp:`htm`
    CPU has hardware transaction memory instructions.

  :samp:`htm-nosc`
    Kernel aborts hardware transactions when a syscall is made.

  :samp:`htm-no-suspend`
    CPU supports hardware transaction memory but does not support the
    ``tsuspend.`` instruction.

  :samp:`ic_snoop`
    CPU supports icache snooping capabilities.

  :samp:`ieee128`
    CPU supports 128-bit IEEE binary floating point instructions.

  :samp:`isel`
    CPU supports the integer select instruction.

  :samp:`mma`
    CPU supports the matrix-multiply assist instructions.

  :samp:`mmu`
    CPU has a memory management unit.

  :samp:`notb`
    CPU does not have a timebase (eg, 601 and 403gx).

  :samp:`pa6t`
    CPU supports the PA Semi 6T CORE ISA.

  :samp:`power4`
    CPU supports ISA 2.00 (eg, POWER4)

  :samp:`power5`
    CPU supports ISA 2.02 (eg, POWER5)

  :samp:`power5+`
    CPU supports ISA 2.03 (eg, POWER5+)

  :samp:`power6x`
    CPU supports ISA 2.05 (eg, POWER6) extended opcodes mffgpr and mftgpr.

  :samp:`ppc32`
    CPU supports 32-bit mode execution.

  :samp:`ppc601`
    CPU supports the old POWER ISA (eg, 601)

  :samp:`ppc64`
    CPU supports 64-bit mode execution.

  :samp:`ppcle`
    CPU supports a little-endian mode that uses address swizzling.

  :samp:`scv`
    Kernel supports system call vectored.

  :samp:`smt`
    CPU support simultaneous multi-threading.

  :samp:`spe`
    CPU has a signal processing extension unit.

  :samp:`tar`
    CPU supports the target address register.

  :samp:`true_le`
    CPU supports true little-endian mode.

  :samp:`ucache`
    CPU has unified I/D cache.

  :samp:`vcrypto`
    CPU supports the vector cryptography instructions.

  :samp:`vsx`
    CPU supports the vector-scalar extension.

    Here is an example:

  .. code-block:: c++

    #ifdef __BUILTIN_CPU_SUPPORTS__
      if (__builtin_cpu_supports ("fpu"))
        {
           asm("fadd %0,%1,%2" : "=d"(dst) : "d"(src1), "d"(src2));
        }
      else
    #endif
        {
           dst = __fadd (src1, src2); // Software FP addition function.
        }

The following built-in functions are also available on all PowerPC
processors:

.. code-block:: c++

  uint64_t __builtin_ppc_get_timebase ();
  unsigned long __builtin_ppc_mftb ();
  double __builtin_unpack_ibm128 (__ibm128, int);
  __ibm128 __builtin_pack_ibm128 (double, double);
  double __builtin_mffs (void);
  void __builtin_mtfsf (const int, double);
  void __builtin_mtfsb0 (const int);
  void __builtin_mtfsb1 (const int);
  void __builtin_set_fpscr_rn (int);

The ``__builtin_ppc_get_timebase`` and ``__builtin_ppc_mftb``
functions generate instructions to read the Time Base Register.  The
``__builtin_ppc_get_timebase`` function may generate multiple
instructions and always returns the 64 bits of the Time Base Register.
The ``__builtin_ppc_mftb`` function always generates one instruction and
returns the Time Base Register value as an unsigned long, throwing away
the most significant word on 32-bit environments.  The ``__builtin_mffs``
return the value of the FPSCR register.  Note, ISA 3.0 supports the
``__builtin_mffsl()`` which permits software to read the control and
non-sticky status bits in the FSPCR without the higher latency associated with
accessing the sticky status bits.  The ``__builtin_mtfsf`` takes a constant
8-bit integer field mask and a double precision floating point argument
and generates the ``mtfsf`` (extended mnemonic) instruction to write new
values to selected fields of the FPSCR.  The
``__builtin_mtfsb0`` and ``__builtin_mtfsb1`` take the bit to change
as an argument.  The valid bit range is between 0 and 31.  The builtins map to
the ``mtfsb0`` and ``mtfsb1`` instructions which take the argument and
add 32.  Hence these instructions only modify the FPSCR[32:63] bits by
changing the specified bit to a zero or one respectively.  The
``__builtin_set_fpscr_rn`` builtin allows changing both of the floating
point rounding mode bits.  The argument is a 2-bit value.  The argument can
either be a ``const int`` or stored in a variable. The builtin uses
the ISA 3.0
instruction ``mffscrn`` if available, otherwise it reads the FPSCR, masks
the current rounding mode bits out and OR's in the new value.

.. _basic-powerpc-built-in-functions-available-on-isa-2.05:

Basic PowerPC Built-in Functions Available on ISA 2.05
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.05
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power6` has the effect of
enabling the :option:`-mpowerpc64`, :option:`-mpowerpc-gpopt`,
:option:`-mpowerpc-gfxopt`, :option:`-mmfcrf`, :option:`-mpopcntb`,
:option:`-mfprnd`, :option:`-mcmpb`, :option:`-mhard-dfp`, and
:option:`-mrecip-precision` options.  Specify the
:option:`-maltivec` option explicitly in
combination with the above options if desired.

The following functions require option :option:`-mcmpb`.

.. code-block:: c++

  unsigned long long __builtin_cmpb (unsigned long long int, unsigned long long int);
  unsigned int __builtin_cmpb (unsigned int, unsigned int);

The ``__builtin_cmpb`` function
performs a byte-wise compare on the contents of its two arguments,
returning the result of the byte-wise comparison as the returned
value.  For each byte comparison, the corresponding byte of the return
value holds 0xff if the input bytes are equal and 0 if the input bytes
are not equal.  If either of the arguments to this built-in function
is wider than 32 bits, the function call expands into the form that
expects ``unsigned long long int`` arguments
which is only available on 64-bit targets.

The following built-in functions are available
when hardware decimal floating point
(:option:`-mhard-dfp`) is available:

.. code-block:: c++

  void __builtin_set_fpscr_drn(int);
  _Decimal64 __builtin_ddedpd (int, _Decimal64);
  _Decimal128 __builtin_ddedpdq (int, _Decimal128);
  _Decimal64 __builtin_denbcd (int, _Decimal64);
  _Decimal128 __builtin_denbcdq (int, _Decimal128);
  _Decimal64 __builtin_diex (long long, _Decimal64);
  _Decimal128 _builtin_diexq (long long, _Decimal128);
  _Decimal64 __builtin_dscli (_Decimal64, int);
  _Decimal128 __builtin_dscliq (_Decimal128, int);
  _Decimal64 __builtin_dscri (_Decimal64, int);
  _Decimal128 __builtin_dscriq (_Decimal128, int);
  long long __builtin_dxex (_Decimal64);
  long long __builtin_dxexq (_Decimal128);
  _Decimal128 __builtin_pack_dec128 (unsigned long long, unsigned long long);
  unsigned long long __builtin_unpack_dec128 (_Decimal128, int);

The __builtin_set_fpscr_drn builtin allows changing the three decimal
floating point rounding mode bits.  The argument is a 3-bit value.  The
argument can either be a const int or the value can be stored in
a variable.
The builtin uses the ISA 3.0 instruction mffscdrn if available.
Otherwise the builtin reads the FPSCR, masks the current decimal rounding
mode bits out and OR's in the new value.

The following functions require :option:`-mhard-float`,
:option:`-mpowerpc-gfxopt`, and :option:`-mpopcntb` options.

.. code-block:: c++

  double __builtin_recipdiv (double, double);
  float __builtin_recipdivf (float, float);
  double __builtin_rsqrt (double);
  float __builtin_rsqrtf (float);

The ``vec_rsqrt``, ``__builtin_rsqrt``, and
``__builtin_rsqrtf`` functions generate multiple instructions to
implement the reciprocal sqrt functionality using reciprocal sqrt
estimate instructions.

The ``__builtin_recipdiv``, and ``__builtin_recipdivf``
functions generate multiple instructions to implement division using
the reciprocal estimate instructions.

The following functions require :option:`-mhard-float` and
:option:`-mmultiple` options.

The ``__builtin_unpack_longdouble`` function takes a
``long double`` argument and a compile time constant of 0 or 1.  If
the constant is 0, the first ``double`` within the
``long double`` is returned, otherwise the second ``double``
is returned.  The ``__builtin_unpack_longdouble`` function is only
available if ``long double`` uses the IBM extended double
representation.

The ``__builtin_pack_longdouble`` function takes two ``double``
arguments and returns a ``long double`` value that combines the two
arguments.  The ``__builtin_pack_longdouble`` function is only
available if ``long double`` uses the IBM extended double
representation.

The ``__builtin_unpack_ibm128`` function takes a ``__ibm128``
argument and a compile time constant of 0 or 1.  If the constant is 0,
the first ``double`` within the ``__ibm128`` is returned,
otherwise the second ``double`` is returned.

The ``__builtin_pack_ibm128`` function takes two ``double``
arguments and returns a ``__ibm128`` value that combines the two
arguments.

Additional built-in functions are available for the 64-bit PowerPC
family of processors, for efficient use of 128-bit floating point
(``__float128``) values.

.. _basic-powerpc-built-in-functions-available-on-isa-2.06:

Basic PowerPC Built-in Functions Available on ISA 2.06
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.05
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power7` has the effect of
enabling all the same options as for :option:`-mcpu=power6` in
addition to the :option:`-maltivec`, :option:`-mpopcntd`, and
:option:`-mvsx` options.

The following basic built-in functions require :option:`-mpopcntd` :

.. code-block:: c++

  unsigned int __builtin_addg6s (unsigned int, unsigned int);
  long long __builtin_bpermd (long long, long long);
  unsigned int __builtin_cbcdtd (unsigned int);
  unsigned int __builtin_cdtbcd (unsigned int);
  long long __builtin_divde (long long, long long);
  unsigned long long __builtin_divdeu (unsigned long long, unsigned long long);
  int __builtin_divwe (int, int);
  unsigned int __builtin_divweu (unsigned int, unsigned int);
  vector __int128 __builtin_pack_vector_int128 (long long, long long);
  void __builtin_rs6000_speculation_barrier (void);
  long long __builtin_unpack_vector_int128 (vector __int128, signed char);

Of these, the ``__builtin_divde`` and ``__builtin_divdeu`` functions
require a 64-bit environment.

The following basic built-in functions, which are also supported on
x86 targets, require :option:`-mfloat128`.

.. code-block:: c++

  __float128 __builtin_fabsq (__float128);
  __float128 __builtin_copysignq (__float128, __float128);
  __float128 __builtin_infq (void);
  __float128 __builtin_huge_valq (void);
  __float128 __builtin_nanq (void);
  __float128 __builtin_nansq (void);

  __float128 __builtin_sqrtf128 (__float128);
  __float128 __builtin_fmaf128 (__float128, __float128, __float128);

.. _basic-powerpc-built-in-functions-available-on-isa-2.07:

Basic PowerPC Built-in Functions Available on ISA 2.07
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.07
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power8` has the effect of
enabling all the same options as for :option:`-mcpu=power7` in
addition to the :option:`-mpower8-fusion`, :option:`-mpower8-vector`,
:option:`-mcrypto`, :option:`-mhtm`, :option:`-mquad-memory`, and
:option:`-mquad-memory-atomic` options.

This section intentionally empty.

.. _basic-powerpc-built-in-functions-available-on-isa-3.0:

Basic PowerPC Built-in Functions Available on ISA 3.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 3.0
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power9` has the effect of
enabling all the same options as for :option:`-mcpu=power8` in
addition to the :option:`-misel` option.

The following built-in functions are available on Linux 64-bit systems
that use the ISA 3.0 instruction set (:option:`-mcpu=power9`):

.. function:: __float128 __builtin_addf128_round_to_odd (__float128, __float128)

  Perform a 128-bit IEEE floating point add using round to odd as the
  rounding mode.

  .. index:: __builtin_addf128_round_to_odd

.. function:: __float128 __builtin_subf128_round_to_odd (__float128, __float128)

  Perform a 128-bit IEEE floating point subtract using round to odd as
  the rounding mode.

  .. index:: __builtin_subf128_round_to_odd

.. function:: __float128 __builtin_mulf128_round_to_odd (__float128, __float128)

  Perform a 128-bit IEEE floating point multiply using round to odd as
  the rounding mode.

  .. index:: __builtin_mulf128_round_to_odd

.. function:: __float128 __builtin_divf128_round_to_odd (__float128, __float128)

  Perform a 128-bit IEEE floating point divide using round to odd as
  the rounding mode.

  .. index:: __builtin_divf128_round_to_odd

.. function:: __float128 __builtin_sqrtf128_round_to_odd (__float128)

  Perform a 128-bit IEEE floating point square root using round to odd
  as the rounding mode.

  .. index:: __builtin_sqrtf128_round_to_odd

.. function:: __float128 __builtin_fmaf128_round_to_odd (__float128, __float128, __float128)

  Perform a 128-bit IEEE floating point fused multiply and add operation
  using round to odd as the rounding mode.

  .. index:: __builtin_fmaf128_round_to_odd

.. function:: double __builtin_truncf128_round_to_odd (__float128)

  Convert a 128-bit IEEE floating point value to ``double`` using
  round to odd as the rounding mode.

  .. index:: __builtin_truncf128_round_to_odd

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0 or later:

.. code-block:: c++

  long long __builtin_darn (void);
  long long __builtin_darn_raw (void);
  int __builtin_darn_32 (void);

The ``__builtin_darn`` and ``__builtin_darn_raw``
functions require a
64-bit environment supporting ISA 3.0 or later.
The ``__builtin_darn`` function provides a 64-bit conditioned
random number.  The ``__builtin_darn_raw`` function provides a
64-bit raw random number.  The ``__builtin_darn_32`` function
provides a 32-bit conditioned random number.

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0 or later:

.. code-block:: c++

  int __builtin_byte_in_set (unsigned char u, unsigned long long set);
  int __builtin_byte_in_range (unsigned char u, unsigned int range);
  int __builtin_byte_in_either_range (unsigned char u, unsigned int ranges);

  int __builtin_dfp_dtstsfi_lt (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_lt (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_lt_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_lt_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_gt (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_gt (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_gt_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_gt_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_eq (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_eq (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_eq_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_eq_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_ov (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_ov (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_ov_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_ov_td (unsigned int comparison, _Decimal128 value);

  double __builtin_mffsl(void);

The ``__builtin_byte_in_set`` function requires a
64-bit environment supporting ISA 3.0 or later.  This function returns
a non-zero value if and only if its ``u`` argument exactly equals one of
the eight bytes contained within its 64-bit ``set`` argument.

The ``__builtin_byte_in_range`` and
``__builtin_byte_in_either_range`` require an environment
supporting ISA 3.0 or later.  For these two functions, the
``range`` argument is encoded as 4 bytes, organized as
``hi_1:lo_1:hi_2:lo_2``.
The ``__builtin_byte_in_range`` function returns a
non-zero value if and only if its ``u`` argument is within the
range bounded between ``lo_2`` and ``hi_2`` inclusive.
The ``__builtin_byte_in_either_range`` function returns non-zero if
and only if its ``u`` argument is within either the range bounded
between ``lo_1`` and ``hi_1`` inclusive or the range bounded
between ``lo_2`` and ``hi_2`` inclusive.

The ``__builtin_dfp_dtstsfi_lt`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
is less than its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_lt_dd`` and
``__builtin_dfp_dtstsfi_lt_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_gt`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
is greater than its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_gt_dd`` and
``__builtin_dfp_dtstsfi_gt_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_eq`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
equals its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_eq_dd`` and
``__builtin_dfp_dtstsfi_eq_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_ov`` function returns a non-zero value
if and only if its ``value`` argument has an undefined number of
significant digits, such as when ``value`` is an encoding of ``NaN``.
The ``__builtin_dfp_dtstsfi_ov_dd`` and
``__builtin_dfp_dtstsfi_ov_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_mffsl`` uses the ISA 3.0 ``mffsl`` instruction to read
the FPSCR.  The instruction is a lower latency version of the ``mffs``
instruction.  If the ``mffsl`` instruction is not available, then the
builtin uses the older ``mffs`` instruction to read the FPSCR.

.. _basic-powerpc-built-in-functions-available-on-isa-3.1:

Basic PowerPC Built-in Functions Available on ISA 3.1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 3.1.
Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power10` has the effect of
enabling all the same options as for :option:`-mcpu=power9`.

The following built-in functions are available on Linux 64-bit systems
that use a future architecture instruction set (:option:`-mcpu=power10`):

.. function:: unsigned long long __builtin_cfuged (unsigned long long, unsigned long long int)

  Perform a 64-bit centrifuge operation, as if implemented by the
  ``cfuged`` instruction.

.. function:: unsigned long long __builtin_cntlzdm (unsigned long long int, unsigned long long int)

  Perform a 64-bit count leading zeros operation under mask, as if
  implemented by the ``cntlzdm`` instruction.

.. function:: unsigned long long __builtin_cnttzdm (unsigned long long, unsigned long long)

  Perform a 64-bit count trailing zeros operation under mask, as if
  implemented by the ``cnttzdm`` instruction.

.. function:: unsigned long long __builtin_pdepd (unsigned long long int, unsigned long long int)

  Perform a 64-bit parallel bits deposit operation, as if implemented by the
  ``pdepd`` instruction.

.. function:: unsigned long long __builtin_pextd (unsigned long long, unsigned long long)

  Perform a 64-bit parallel bits extract operation, as if implemented by the
  ``pextd`` instruction.

.. code-block:: c++

  vector signed __int128 vsx_xl_sext (signed long long, signed char *)
  vector signed __int128 vsx_xl_sext (signed long long, signed short *)
  vector signed __int128 vsx_xl_sext (signed long long, signed int *)
  vector signed __int128 vsx_xl_sext (signed long long, signed long long *)
  vector unsigned __int128 vsx_xl_zext (signed long long, unsigned char *)
  vector unsigned __int128 vsx_xl_zext (signed long long, unsigned short *)
  vector unsigned __int128 vsx_xl_zext (signed long long, unsigned int *)
  vector unsigned __int128 vsx_xl_zext (signed long long, unsigned long long *)

Load (and sign extend) to an __int128 vector, as if implemented by the ISA 3.1
``lxvrbx``, ``lxvrhx``, ``lxvrwx``, and  ``lxvrdx`` instructions.

.. index:: vsx_xl_sext, vsx_xl_zext

.. code-block:: c++

  void vec_xst_trunc (vector signed __int128, signed long long, signed char *)
  void vec_xst_trunc (vector signed __int128, signed long long, signed short *)
  void vec_xst_trunc (vector signed __int128, signed long long, signed int *)
  void vec_xst_trunc (vector signed __int128, signed long long, signed long long *)
  void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned char *)
  void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned short *)
  void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned int *)
  void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned long long *)

Truncate and store the rightmost element of a vector, as if implemented by the
ISA 3.1 ``stxvrbx``, ``stxvrhx``, ``stxvrwx``, and ``stxvrdx``
instructions.

.. index:: vec_xst_trunc