..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _x86-built-in-functions:

x86 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the x86-32 and x86-64 family
of computers, depending on the command-line switches used.

If you specify command-line switches such as :option:`-msse`,
the compiler could use the extended instruction sets even if the built-ins
are not used explicitly in the program.  For this reason, applications
that perform run-time CPU detection must compile separate files for each
supported architecture, using the appropriate flags.  In particular,
the file containing the CPU detection code should be compiled without
these options.

The following machine modes are available for use with MMX built-in functions
(see :ref:`vector-extensions`): ``V2SI`` for a vector of two 32-bit integers,
``V4HI`` for a vector of four 16-bit integers, and ``V8QI`` for a
vector of eight 8-bit integers.  Some of the built-in functions operate on
MMX registers as a whole 64-bit entity, these use ``V1DI`` as their mode.

If 3DNow! extensions are enabled, ``V2SF`` is used as a mode for a vector
of two 32-bit floating-point values.

If SSE extensions are enabled, ``V4SF`` is used for a vector of four 32-bit
floating-point values.  Some instructions use a vector of four 32-bit
integers, these use ``V4SI``.  Finally, some instructions operate on an
entire vector register, interpreting it as a 128-bit integer, these use mode
``TI``.

The x86-32 and x86-64 family of processors use additional built-in
functions for efficient use of ``TF`` (``__float128``) 128-bit
floating point and ``TC`` 128-bit complex floating-point values.

The following floating-point built-in functions are always available.  All
of them implement the function that is part of the name.

.. code-block:: c++

  __float128 __builtin_fabsq (__float128)
  __float128 __builtin_copysignq (__float128, __float128)

The following built-in functions are always available.

.. function:: __float128 __builtin_infq (void)

  Similar to ``__builtin_inf``, except the return type is ``__float128``.

  .. index:: __builtin_infq

.. function:: __float128 __builtin_huge_valq (void)

  Similar to ``__builtin_huge_val``, except the return type is ``__float128``.

  .. index:: __builtin_huge_valq

.. function:: __float128 __builtin_nanq (void)

  Similar to ``__builtin_nan``, except the return type is ``__float128``.

  .. index:: __builtin_nanq

.. function:: __float128 __builtin_nansq (void)

  Similar to ``__builtin_nans``, except the return type is ``__float128``.

  .. index:: __builtin_nansq

  The following built-in function is always available.

.. function:: void __builtin_ia32_pause (void)

  Generates the ``pause`` machine instruction with a compiler memory
  barrier.

The following built-in functions are always available and can be used to
check the target platform type.

.. function:: void __builtin_cpu_init (void)

  This function runs the CPU detection code to check the type of CPU and the
  features supported.  This built-in function needs to be invoked along with the built-in functions
  to check CPU type and features, ``__builtin_cpu_is`` and
  ``__builtin_cpu_supports``, only when used in a function that is
  executed before any constructors are called.  The CPU detection code is
  automatically executed in a very high priority constructor.

  For example, this function has to be used in ``ifunc`` resolvers that
  check for CPU type using the built-in functions ``__builtin_cpu_is``
  and ``__builtin_cpu_supports``, or in constructors on targets that
  don't support constructor priority.

  .. code-block:: c++

    static void (*resolve_memcpy (void)) (void)
    {
      // ifunc resolvers fire before constructors, explicitly call the init
      // function.
      __builtin_cpu_init ();
      if (__builtin_cpu_supports ("ssse3"))
        return ssse3_memcpy; // super fast memcpy with ssse3 instructions.
      else
        return default_memcpy;
    }

    void *memcpy (void *, const void *, size_t)
         __attribute__ ((ifunc ("resolve_memcpy")));

.. function:: int __builtin_cpu_is (const char *cpuname)

  This function returns a positive integer if the run-time CPU
  is of type :samp:`{cpuname}`
  and returns ``0`` otherwise. The following CPU names can be detected:

  :samp:`amd`
    AMD CPU.

  :samp:`intel`
    Intel CPU.

  :samp:`atom`
    Intel Atom CPU.

  :samp:`slm`
    Intel Silvermont CPU.

  :samp:`core2`
    Intel Core 2 CPU.

  :samp:`corei7`
    Intel Core i7 CPU.

  :samp:`nehalem`
    Intel Core i7 Nehalem CPU.

  :samp:`westmere`
    Intel Core i7 Westmere CPU.

  :samp:`sandybridge`
    Intel Core i7 Sandy Bridge CPU.

  :samp:`ivybridge`
    Intel Core i7 Ivy Bridge CPU.

  :samp:`haswell`
    Intel Core i7 Haswell CPU.

  :samp:`broadwell`
    Intel Core i7 Broadwell CPU.

  :samp:`skylake`
    Intel Core i7 Skylake CPU.

  :samp:`skylake-avx512`
    Intel Core i7 Skylake AVX512 CPU.

  :samp:`cannonlake`
    Intel Core i7 Cannon Lake CPU.

  :samp:`icelake-client`
    Intel Core i7 Ice Lake Client CPU.

  :samp:`icelake-server`
    Intel Core i7 Ice Lake Server CPU.

  :samp:`cascadelake`
    Intel Core i7 Cascadelake CPU.

  :samp:`tigerlake`
    Intel Core i7 Tigerlake CPU.

  :samp:`cooperlake`
    Intel Core i7 Cooperlake CPU.

  :samp:`sapphirerapids`
    Intel Core i7 sapphirerapids CPU.

  :samp:`alderlake`
    Intel Core i7 Alderlake CPU.

  :samp:`rocketlake`
    Intel Core i7 Rocketlake CPU.

  :samp:`graniterapids`
    Intel Core i7 graniterapids CPU.

  :samp:`bonnell`
    Intel Atom Bonnell CPU.

  :samp:`silvermont`
    Intel Atom Silvermont CPU.

  :samp:`goldmont`
    Intel Atom Goldmont CPU.

  :samp:`goldmont-plus`
    Intel Atom Goldmont Plus CPU.

  :samp:`tremont`
    Intel Atom Tremont CPU.

  :samp:`sierraforest`
    Intel Atom Sierra Forest CPU.

  :samp:`grandridge`
    Intel Atom Grand Ridge CPU.

  :samp:`knl`
    Intel Knights Landing CPU.

  :samp:`knm`
    Intel Knights Mill CPU.

  :samp:`lujiazui`
    ZHAOXIN lujiazui CPU.

  :samp:`amdfam10h`
    AMD Family 10h CPU.

  :samp:`barcelona`
    AMD Family 10h Barcelona CPU.

  :samp:`shanghai`
    AMD Family 10h Shanghai CPU.

  :samp:`istanbul`
    AMD Family 10h Istanbul CPU.

  :samp:`btver1`
    AMD Family 14h CPU.

  :samp:`amdfam15h`
    AMD Family 15h CPU.

  :samp:`bdver1`
    AMD Family 15h Bulldozer version 1.

  :samp:`bdver2`
    AMD Family 15h Bulldozer version 2.

  :samp:`bdver3`
    AMD Family 15h Bulldozer version 3.

  :samp:`bdver4`
    AMD Family 15h Bulldozer version 4.

  :samp:`btver2`
    AMD Family 16h CPU.

  :samp:`amdfam17h`
    AMD Family 17h CPU.

  :samp:`znver1`
    AMD Family 17h Zen version 1.

  :samp:`znver2`
    AMD Family 17h Zen version 2.

  :samp:`amdfam19h`
    AMD Family 19h CPU.

  :samp:`znver3`
    AMD Family 19h Zen version 3.

  :samp:`znver4`
    AMD Family 19h Zen version 4.

  :samp:`x86-64`
    Baseline x86-64 microarchitecture level (as defined in x86-64 psABI).

  :samp:`x86-64-v2`
    x86-64-v2 microarchitecture level.

  :samp:`x86-64-v3`
    x86-64-v3 microarchitecture level.

  :samp:`x86-64-v4`
    x86-64-v4 microarchitecture level.

    Here is an example:

  .. code-block:: c++

    if (__builtin_cpu_is ("corei7"))
      {
         do_corei7 (); // Core i7 specific implementation.
      }
    else
      {
         do_generic (); // Generic implementation.
      }

.. function:: int __builtin_cpu_supports (const char *feature)

  This function returns a positive integer if the run-time CPU
  supports :samp:`{feature}`
  and returns ``0`` otherwise. The following features can be detected:

  :samp:`cmov`
    CMOV instruction.

  :samp:`mmx`
    MMX instructions.

  :samp:`popcnt`
    POPCNT instruction.

  :samp:`sse`
    SSE instructions.

  :samp:`sse2`
    SSE2 instructions.

  :samp:`sse3`
    SSE3 instructions.

  :samp:`ssse3`
    SSSE3 instructions.

  :samp:`sse4.1`
    SSE4.1 instructions.

  :samp:`sse4.2`
    SSE4.2 instructions.

  :samp:`avx`
    AVX instructions.

  :samp:`avx2`
    AVX2 instructions.

  :samp:`sse4a`
    SSE4A instructions.

  :samp:`fma4`
    FMA4 instructions.

  :samp:`xop`
    XOP instructions.

  :samp:`fma`
    FMA instructions.

  :samp:`avx512f`
    AVX512F instructions.

  :samp:`bmi`
    BMI instructions.

  :samp:`bmi2`
    BMI2 instructions.

  :samp:`aes`
    AES instructions.

  :samp:`pclmul`
    PCLMUL instructions.

  :samp:`avx512vl`
    AVX512VL instructions.

  :samp:`avx512bw`
    AVX512BW instructions.

  :samp:`avx512dq`
    AVX512DQ instructions.

  :samp:`avx512cd`
    AVX512CD instructions.

  :samp:`avx512er`
    AVX512ER instructions.

  :samp:`avx512pf`
    AVX512PF instructions.

  :samp:`avx512vbmi`
    AVX512VBMI instructions.

  :samp:`avx512ifma`
    AVX512IFMA instructions.

  :samp:`avx5124vnniw`
    AVX5124VNNIW instructions.

  :samp:`avx5124fmaps`
    AVX5124FMAPS instructions.

  :samp:`avx512vpopcntdq`
    AVX512VPOPCNTDQ instructions.

  :samp:`avx512vbmi2`
    AVX512VBMI2 instructions.

  :samp:`gfni`
    GFNI instructions.

  :samp:`vpclmulqdq`
    VPCLMULQDQ instructions.

  :samp:`avx512vnni`
    AVX512VNNI instructions.

  :samp:`avx512bitalg`
    AVX512BITALG instructions.

  Here is an example:

  .. code-block:: c++

    if (__builtin_cpu_supports ("popcnt"))
      {
         asm("popcnt %1,%0" : "=r"(count) : "rm"(n) : "cc");
      }
    else
      {
         count = generic_countbits (n); //generic implementation.
      }

The following built-in functions are made available by :option:`-mmmx`.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v8qi __builtin_ia32_paddb (v8qi, v8qi);
  v4hi __builtin_ia32_paddw (v4hi, v4hi);
  v2si __builtin_ia32_paddd (v2si, v2si);
  v8qi __builtin_ia32_psubb (v8qi, v8qi);
  v4hi __builtin_ia32_psubw (v4hi, v4hi);
  v2si __builtin_ia32_psubd (v2si, v2si);
  v8qi __builtin_ia32_paddsb (v8qi, v8qi);
  v4hi __builtin_ia32_paddsw (v4hi, v4hi);
  v8qi __builtin_ia32_psubsb (v8qi, v8qi);
  v4hi __builtin_ia32_psubsw (v4hi, v4hi);
  v8qi __builtin_ia32_paddusb (v8qi, v8qi);
  v4hi __builtin_ia32_paddusw (v4hi, v4hi);
  v8qi __builtin_ia32_psubusb (v8qi, v8qi);
  v4hi __builtin_ia32_psubusw (v4hi, v4hi);
  v4hi __builtin_ia32_pmullw (v4hi, v4hi);
  v4hi __builtin_ia32_pmulhw (v4hi, v4hi);
  di __builtin_ia32_pand (di, di);
  di __builtin_ia32_pandn (di,di);
  di __builtin_ia32_por (di, di);
  di __builtin_ia32_pxor (di, di);
  v8qi __builtin_ia32_pcmpeqb (v8qi, v8qi);
  v4hi __builtin_ia32_pcmpeqw (v4hi, v4hi);
  v2si __builtin_ia32_pcmpeqd (v2si, v2si);
  v8qi __builtin_ia32_pcmpgtb (v8qi, v8qi);
  v4hi __builtin_ia32_pcmpgtw (v4hi, v4hi);
  v2si __builtin_ia32_pcmpgtd (v2si, v2si);
  v8qi __builtin_ia32_punpckhbw (v8qi, v8qi);
  v4hi __builtin_ia32_punpckhwd (v4hi, v4hi);
  v2si __builtin_ia32_punpckhdq (v2si, v2si);
  v8qi __builtin_ia32_punpcklbw (v8qi, v8qi);
  v4hi __builtin_ia32_punpcklwd (v4hi, v4hi);
  v2si __builtin_ia32_punpckldq (v2si, v2si);
  v8qi __builtin_ia32_packsswb (v4hi, v4hi);
  v4hi __builtin_ia32_packssdw (v2si, v2si);
  v8qi __builtin_ia32_packuswb (v4hi, v4hi);

  v4hi __builtin_ia32_psllw (v4hi, v4hi);
  v2si __builtin_ia32_pslld (v2si, v2si);
  v1di __builtin_ia32_psllq (v1di, v1di);
  v4hi __builtin_ia32_psrlw (v4hi, v4hi);
  v2si __builtin_ia32_psrld (v2si, v2si);
  v1di __builtin_ia32_psrlq (v1di, v1di);
  v4hi __builtin_ia32_psraw (v4hi, v4hi);
  v2si __builtin_ia32_psrad (v2si, v2si);
  v4hi __builtin_ia32_psllwi (v4hi, int);
  v2si __builtin_ia32_pslldi (v2si, int);
  v1di __builtin_ia32_psllqi (v1di, int);
  v4hi __builtin_ia32_psrlwi (v4hi, int);
  v2si __builtin_ia32_psrldi (v2si, int);
  v1di __builtin_ia32_psrlqi (v1di, int);
  v4hi __builtin_ia32_psrawi (v4hi, int);
  v2si __builtin_ia32_psradi (v2si, int);

The following built-in functions are made available either with
:option:`-msse`, or with :option:`-m3dnowa`.  All of them generate
the machine instruction that is part of the name.

.. code-block:: c++

  v4hi __builtin_ia32_pmulhuw (v4hi, v4hi);
  v8qi __builtin_ia32_pavgb (v8qi, v8qi);
  v4hi __builtin_ia32_pavgw (v4hi, v4hi);
  v1di __builtin_ia32_psadbw (v8qi, v8qi);
  v8qi __builtin_ia32_pmaxub (v8qi, v8qi);
  v4hi __builtin_ia32_pmaxsw (v4hi, v4hi);
  v8qi __builtin_ia32_pminub (v8qi, v8qi);
  v4hi __builtin_ia32_pminsw (v4hi, v4hi);
  int __builtin_ia32_pmovmskb (v8qi);
  void __builtin_ia32_maskmovq (v8qi, v8qi, char *);
  void __builtin_ia32_movntq (di *, di);
  void __builtin_ia32_sfence (void);

The following built-in functions are available when :option:`-msse` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ia32_comieq (v4sf, v4sf);
  int __builtin_ia32_comineq (v4sf, v4sf);
  int __builtin_ia32_comilt (v4sf, v4sf);
  int __builtin_ia32_comile (v4sf, v4sf);
  int __builtin_ia32_comigt (v4sf, v4sf);
  int __builtin_ia32_comige (v4sf, v4sf);
  int __builtin_ia32_ucomieq (v4sf, v4sf);
  int __builtin_ia32_ucomineq (v4sf, v4sf);
  int __builtin_ia32_ucomilt (v4sf, v4sf);
  int __builtin_ia32_ucomile (v4sf, v4sf);
  int __builtin_ia32_ucomigt (v4sf, v4sf);
  int __builtin_ia32_ucomige (v4sf, v4sf);
  v4sf __builtin_ia32_addps (v4sf, v4sf);
  v4sf __builtin_ia32_subps (v4sf, v4sf);
  v4sf __builtin_ia32_mulps (v4sf, v4sf);
  v4sf __builtin_ia32_divps (v4sf, v4sf);
  v4sf __builtin_ia32_addss (v4sf, v4sf);
  v4sf __builtin_ia32_subss (v4sf, v4sf);
  v4sf __builtin_ia32_mulss (v4sf, v4sf);
  v4sf __builtin_ia32_divss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpeqps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpltps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpleps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpgtps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpgeps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpunordps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpneqps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpnltps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpnleps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpngtps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpngeps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpordps (v4sf, v4sf);
  v4sf __builtin_ia32_cmpeqss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpltss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpless (v4sf, v4sf);
  v4sf __builtin_ia32_cmpunordss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpneqss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpnltss (v4sf, v4sf);
  v4sf __builtin_ia32_cmpnless (v4sf, v4sf);
  v4sf __builtin_ia32_cmpordss (v4sf, v4sf);
  v4sf __builtin_ia32_maxps (v4sf, v4sf);
  v4sf __builtin_ia32_maxss (v4sf, v4sf);
  v4sf __builtin_ia32_minps (v4sf, v4sf);
  v4sf __builtin_ia32_minss (v4sf, v4sf);
  v4sf __builtin_ia32_andps (v4sf, v4sf);
  v4sf __builtin_ia32_andnps (v4sf, v4sf);
  v4sf __builtin_ia32_orps (v4sf, v4sf);
  v4sf __builtin_ia32_xorps (v4sf, v4sf);
  v4sf __builtin_ia32_movss (v4sf, v4sf);
  v4sf __builtin_ia32_movhlps (v4sf, v4sf);
  v4sf __builtin_ia32_movlhps (v4sf, v4sf);
  v4sf __builtin_ia32_unpckhps (v4sf, v4sf);
  v4sf __builtin_ia32_unpcklps (v4sf, v4sf);
  v4sf __builtin_ia32_cvtpi2ps (v4sf, v2si);
  v4sf __builtin_ia32_cvtsi2ss (v4sf, int);
  v2si __builtin_ia32_cvtps2pi (v4sf);
  int __builtin_ia32_cvtss2si (v4sf);
  v2si __builtin_ia32_cvttps2pi (v4sf);
  int __builtin_ia32_cvttss2si (v4sf);
  v4sf __builtin_ia32_rcpps (v4sf);
  v4sf __builtin_ia32_rsqrtps (v4sf);
  v4sf __builtin_ia32_sqrtps (v4sf);
  v4sf __builtin_ia32_rcpss (v4sf);
  v4sf __builtin_ia32_rsqrtss (v4sf);
  v4sf __builtin_ia32_sqrtss (v4sf);
  v4sf __builtin_ia32_shufps (v4sf, v4sf, int);
  void __builtin_ia32_movntps (float *, v4sf);
  int __builtin_ia32_movmskps (v4sf);

The following built-in functions are available when :option:`-msse` is used.

.. function:: v4sf __builtin_ia32_loadups (float *)

  Generates the ``movups`` machine instruction as a load from memory.

.. function:: void __builtin_ia32_storeups (float *, v4sf)

  Generates the ``movups`` machine instruction as a store to memory.

.. function:: v4sf __builtin_ia32_loadss (float *)

  Generates the ``movss`` machine instruction as a load from memory.

.. function:: v4sf __builtin_ia32_loadhps (v4sf, const v2sf *)

  Generates the ``movhps`` machine instruction as a load from memory.

.. function:: v4sf __builtin_ia32_loadlps (v4sf, const v2sf *)

  Generates the ``movlps`` machine instruction as a load from memory

.. function:: void __builtin_ia32_storehps (v2sf *, v4sf)

  Generates the ``movhps`` machine instruction as a store to memory.

.. function:: void __builtin_ia32_storelps (v2sf *, v4sf)

  Generates the ``movlps`` machine instruction as a store to memory.

  The following built-in functions are available when :option:`-msse2` is used.

All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ia32_comisdeq (v2df, v2df);
  int __builtin_ia32_comisdlt (v2df, v2df);
  int __builtin_ia32_comisdle (v2df, v2df);
  int __builtin_ia32_comisdgt (v2df, v2df);
  int __builtin_ia32_comisdge (v2df, v2df);
  int __builtin_ia32_comisdneq (v2df, v2df);
  int __builtin_ia32_ucomisdeq (v2df, v2df);
  int __builtin_ia32_ucomisdlt (v2df, v2df);
  int __builtin_ia32_ucomisdle (v2df, v2df);
  int __builtin_ia32_ucomisdgt (v2df, v2df);
  int __builtin_ia32_ucomisdge (v2df, v2df);
  int __builtin_ia32_ucomisdneq (v2df, v2df);
  v2df __builtin_ia32_cmpeqpd (v2df, v2df);
  v2df __builtin_ia32_cmpltpd (v2df, v2df);
  v2df __builtin_ia32_cmplepd (v2df, v2df);
  v2df __builtin_ia32_cmpgtpd (v2df, v2df);
  v2df __builtin_ia32_cmpgepd (v2df, v2df);
  v2df __builtin_ia32_cmpunordpd (v2df, v2df);
  v2df __builtin_ia32_cmpneqpd (v2df, v2df);
  v2df __builtin_ia32_cmpnltpd (v2df, v2df);
  v2df __builtin_ia32_cmpnlepd (v2df, v2df);
  v2df __builtin_ia32_cmpngtpd (v2df, v2df);
  v2df __builtin_ia32_cmpngepd (v2df, v2df);
  v2df __builtin_ia32_cmpordpd (v2df, v2df);
  v2df __builtin_ia32_cmpeqsd (v2df, v2df);
  v2df __builtin_ia32_cmpltsd (v2df, v2df);
  v2df __builtin_ia32_cmplesd (v2df, v2df);
  v2df __builtin_ia32_cmpunordsd (v2df, v2df);
  v2df __builtin_ia32_cmpneqsd (v2df, v2df);
  v2df __builtin_ia32_cmpnltsd (v2df, v2df);
  v2df __builtin_ia32_cmpnlesd (v2df, v2df);
  v2df __builtin_ia32_cmpordsd (v2df, v2df);
  v2di __builtin_ia32_paddq (v2di, v2di);
  v2di __builtin_ia32_psubq (v2di, v2di);
  v2df __builtin_ia32_addpd (v2df, v2df);
  v2df __builtin_ia32_subpd (v2df, v2df);
  v2df __builtin_ia32_mulpd (v2df, v2df);
  v2df __builtin_ia32_divpd (v2df, v2df);
  v2df __builtin_ia32_addsd (v2df, v2df);
  v2df __builtin_ia32_subsd (v2df, v2df);
  v2df __builtin_ia32_mulsd (v2df, v2df);
  v2df __builtin_ia32_divsd (v2df, v2df);
  v2df __builtin_ia32_minpd (v2df, v2df);
  v2df __builtin_ia32_maxpd (v2df, v2df);
  v2df __builtin_ia32_minsd (v2df, v2df);
  v2df __builtin_ia32_maxsd (v2df, v2df);
  v2df __builtin_ia32_andpd (v2df, v2df);
  v2df __builtin_ia32_andnpd (v2df, v2df);
  v2df __builtin_ia32_orpd (v2df, v2df);
  v2df __builtin_ia32_xorpd (v2df, v2df);
  v2df __builtin_ia32_movsd (v2df, v2df);
  v2df __builtin_ia32_unpckhpd (v2df, v2df);
  v2df __builtin_ia32_unpcklpd (v2df, v2df);
  v16qi __builtin_ia32_paddb128 (v16qi, v16qi);
  v8hi __builtin_ia32_paddw128 (v8hi, v8hi);
  v4si __builtin_ia32_paddd128 (v4si, v4si);
  v2di __builtin_ia32_paddq128 (v2di, v2di);
  v16qi __builtin_ia32_psubb128 (v16qi, v16qi);
  v8hi __builtin_ia32_psubw128 (v8hi, v8hi);
  v4si __builtin_ia32_psubd128 (v4si, v4si);
  v2di __builtin_ia32_psubq128 (v2di, v2di);
  v8hi __builtin_ia32_pmullw128 (v8hi, v8hi);
  v8hi __builtin_ia32_pmulhw128 (v8hi, v8hi);
  v2di __builtin_ia32_pand128 (v2di, v2di);
  v2di __builtin_ia32_pandn128 (v2di, v2di);
  v2di __builtin_ia32_por128 (v2di, v2di);
  v2di __builtin_ia32_pxor128 (v2di, v2di);
  v16qi __builtin_ia32_pavgb128 (v16qi, v16qi);
  v8hi __builtin_ia32_pavgw128 (v8hi, v8hi);
  v16qi __builtin_ia32_pcmpeqb128 (v16qi, v16qi);
  v8hi __builtin_ia32_pcmpeqw128 (v8hi, v8hi);
  v4si __builtin_ia32_pcmpeqd128 (v4si, v4si);
  v16qi __builtin_ia32_pcmpgtb128 (v16qi, v16qi);
  v8hi __builtin_ia32_pcmpgtw128 (v8hi, v8hi);
  v4si __builtin_ia32_pcmpgtd128 (v4si, v4si);
  v16qi __builtin_ia32_pmaxub128 (v16qi, v16qi);
  v8hi __builtin_ia32_pmaxsw128 (v8hi, v8hi);
  v16qi __builtin_ia32_pminub128 (v16qi, v16qi);
  v8hi __builtin_ia32_pminsw128 (v8hi, v8hi);
  v16qi __builtin_ia32_punpckhbw128 (v16qi, v16qi);
  v8hi __builtin_ia32_punpckhwd128 (v8hi, v8hi);
  v4si __builtin_ia32_punpckhdq128 (v4si, v4si);
  v2di __builtin_ia32_punpckhqdq128 (v2di, v2di);
  v16qi __builtin_ia32_punpcklbw128 (v16qi, v16qi);
  v8hi __builtin_ia32_punpcklwd128 (v8hi, v8hi);
  v4si __builtin_ia32_punpckldq128 (v4si, v4si);
  v2di __builtin_ia32_punpcklqdq128 (v2di, v2di);
  v16qi __builtin_ia32_packsswb128 (v8hi, v8hi);
  v8hi __builtin_ia32_packssdw128 (v4si, v4si);
  v16qi __builtin_ia32_packuswb128 (v8hi, v8hi);
  v8hi __builtin_ia32_pmulhuw128 (v8hi, v8hi);
  void __builtin_ia32_maskmovdqu (v16qi, v16qi);
  v2df __builtin_ia32_loadupd (double *);
  void __builtin_ia32_storeupd (double *, v2df);
  v2df __builtin_ia32_loadhpd (v2df, double const *);
  v2df __builtin_ia32_loadlpd (v2df, double const *);
  int __builtin_ia32_movmskpd (v2df);
  int __builtin_ia32_pmovmskb128 (v16qi);
  void __builtin_ia32_movnti (int *, int);
  void __builtin_ia32_movnti64 (long long int *, long long int);
  void __builtin_ia32_movntpd (double *, v2df);
  void __builtin_ia32_movntdq (v2df *, v2df);
  v4si __builtin_ia32_pshufd (v4si, int);
  v8hi __builtin_ia32_pshuflw (v8hi, int);
  v8hi __builtin_ia32_pshufhw (v8hi, int);
  v2di __builtin_ia32_psadbw128 (v16qi, v16qi);
  v2df __builtin_ia32_sqrtpd (v2df);
  v2df __builtin_ia32_sqrtsd (v2df);
  v2df __builtin_ia32_shufpd (v2df, v2df, int);
  v2df __builtin_ia32_cvtdq2pd (v4si);
  v4sf __builtin_ia32_cvtdq2ps (v4si);
  v4si __builtin_ia32_cvtpd2dq (v2df);
  v2si __builtin_ia32_cvtpd2pi (v2df);
  v4sf __builtin_ia32_cvtpd2ps (v2df);
  v4si __builtin_ia32_cvttpd2dq (v2df);
  v2si __builtin_ia32_cvttpd2pi (v2df);
  v2df __builtin_ia32_cvtpi2pd (v2si);
  int __builtin_ia32_cvtsd2si (v2df);
  int __builtin_ia32_cvttsd2si (v2df);
  long long __builtin_ia32_cvtsd2si64 (v2df);
  long long __builtin_ia32_cvttsd2si64 (v2df);
  v4si __builtin_ia32_cvtps2dq (v4sf);
  v2df __builtin_ia32_cvtps2pd (v4sf);
  v4si __builtin_ia32_cvttps2dq (v4sf);
  v2df __builtin_ia32_cvtsi2sd (v2df, int);
  v2df __builtin_ia32_cvtsi642sd (v2df, long long);
  v4sf __builtin_ia32_cvtsd2ss (v4sf, v2df);
  v2df __builtin_ia32_cvtss2sd (v2df, v4sf);
  void __builtin_ia32_clflush (const void *);
  void __builtin_ia32_lfence (void);
  void __builtin_ia32_mfence (void);
  v16qi __builtin_ia32_loaddqu (const char *);
  void __builtin_ia32_storedqu (char *, v16qi);
  v1di __builtin_ia32_pmuludq (v2si, v2si);
  v2di __builtin_ia32_pmuludq128 (v4si, v4si);
  v8hi __builtin_ia32_psllw128 (v8hi, v8hi);
  v4si __builtin_ia32_pslld128 (v4si, v4si);
  v2di __builtin_ia32_psllq128 (v2di, v2di);
  v8hi __builtin_ia32_psrlw128 (v8hi, v8hi);
  v4si __builtin_ia32_psrld128 (v4si, v4si);
  v2di __builtin_ia32_psrlq128 (v2di, v2di);
  v8hi __builtin_ia32_psraw128 (v8hi, v8hi);
  v4si __builtin_ia32_psrad128 (v4si, v4si);
  v2di __builtin_ia32_pslldqi128 (v2di, int);
  v8hi __builtin_ia32_psllwi128 (v8hi, int);
  v4si __builtin_ia32_pslldi128 (v4si, int);
  v2di __builtin_ia32_psllqi128 (v2di, int);
  v2di __builtin_ia32_psrldqi128 (v2di, int);
  v8hi __builtin_ia32_psrlwi128 (v8hi, int);
  v4si __builtin_ia32_psrldi128 (v4si, int);
  v2di __builtin_ia32_psrlqi128 (v2di, int);
  v8hi __builtin_ia32_psrawi128 (v8hi, int);
  v4si __builtin_ia32_psradi128 (v4si, int);
  v4si __builtin_ia32_pmaddwd128 (v8hi, v8hi);
  v2di __builtin_ia32_movq128 (v2di);

The following built-in functions are available when :option:`-msse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2df __builtin_ia32_addsubpd (v2df, v2df);
  v4sf __builtin_ia32_addsubps (v4sf, v4sf);
  v2df __builtin_ia32_haddpd (v2df, v2df);
  v4sf __builtin_ia32_haddps (v4sf, v4sf);
  v2df __builtin_ia32_hsubpd (v2df, v2df);
  v4sf __builtin_ia32_hsubps (v4sf, v4sf);
  v16qi __builtin_ia32_lddqu (char const *);
  void __builtin_ia32_monitor (void *, unsigned int, unsigned int);
  v4sf __builtin_ia32_movshdup (v4sf);
  v4sf __builtin_ia32_movsldup (v4sf);
  void __builtin_ia32_mwait (unsigned int, unsigned int);

The following built-in functions are available when :option:`-mssse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2si __builtin_ia32_phaddd (v2si, v2si);
  v4hi __builtin_ia32_phaddw (v4hi, v4hi);
  v4hi __builtin_ia32_phaddsw (v4hi, v4hi);
  v2si __builtin_ia32_phsubd (v2si, v2si);
  v4hi __builtin_ia32_phsubw (v4hi, v4hi);
  v4hi __builtin_ia32_phsubsw (v4hi, v4hi);
  v4hi __builtin_ia32_pmaddubsw (v8qi, v8qi);
  v4hi __builtin_ia32_pmulhrsw (v4hi, v4hi);
  v8qi __builtin_ia32_pshufb (v8qi, v8qi);
  v8qi __builtin_ia32_psignb (v8qi, v8qi);
  v2si __builtin_ia32_psignd (v2si, v2si);
  v4hi __builtin_ia32_psignw (v4hi, v4hi);
  v1di __builtin_ia32_palignr (v1di, v1di, int);
  v8qi __builtin_ia32_pabsb (v8qi);
  v2si __builtin_ia32_pabsd (v2si);
  v4hi __builtin_ia32_pabsw (v4hi);

The following built-in functions are available when :option:`-mssse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v4si __builtin_ia32_phaddd128 (v4si, v4si);
  v8hi __builtin_ia32_phaddw128 (v8hi, v8hi);
  v8hi __builtin_ia32_phaddsw128 (v8hi, v8hi);
  v4si __builtin_ia32_phsubd128 (v4si, v4si);
  v8hi __builtin_ia32_phsubw128 (v8hi, v8hi);
  v8hi __builtin_ia32_phsubsw128 (v8hi, v8hi);
  v8hi __builtin_ia32_pmaddubsw128 (v16qi, v16qi);
  v8hi __builtin_ia32_pmulhrsw128 (v8hi, v8hi);
  v16qi __builtin_ia32_pshufb128 (v16qi, v16qi);
  v16qi __builtin_ia32_psignb128 (v16qi, v16qi);
  v4si __builtin_ia32_psignd128 (v4si, v4si);
  v8hi __builtin_ia32_psignw128 (v8hi, v8hi);
  v2di __builtin_ia32_palignr128 (v2di, v2di, int);
  v16qi __builtin_ia32_pabsb128 (v16qi);
  v4si __builtin_ia32_pabsd128 (v4si);
  v8hi __builtin_ia32_pabsw128 (v8hi);

The following built-in functions are available when :option:`-msse4.1` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v2df __builtin_ia32_blendpd (v2df, v2df, const int);
  v4sf __builtin_ia32_blendps (v4sf, v4sf, const int);
  v2df __builtin_ia32_blendvpd (v2df, v2df, v2df);
  v4sf __builtin_ia32_blendvps (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_dppd (v2df, v2df, const int);
  v4sf __builtin_ia32_dpps (v4sf, v4sf, const int);
  v4sf __builtin_ia32_insertps128 (v4sf, v4sf, const int);
  v2di __builtin_ia32_movntdqa (v2di *);
  v16qi __builtin_ia32_mpsadbw128 (v16qi, v16qi, const int);
  v8hi __builtin_ia32_packusdw128 (v4si, v4si);
  v16qi __builtin_ia32_pblendvb128 (v16qi, v16qi, v16qi);
  v8hi __builtin_ia32_pblendw128 (v8hi, v8hi, const int);
  v2di __builtin_ia32_pcmpeqq (v2di, v2di);
  v8hi __builtin_ia32_phminposuw128 (v8hi);
  v16qi __builtin_ia32_pmaxsb128 (v16qi, v16qi);
  v4si __builtin_ia32_pmaxsd128 (v4si, v4si);
  v4si __builtin_ia32_pmaxud128 (v4si, v4si);
  v8hi __builtin_ia32_pmaxuw128 (v8hi, v8hi);
  v16qi __builtin_ia32_pminsb128 (v16qi, v16qi);
  v4si __builtin_ia32_pminsd128 (v4si, v4si);
  v4si __builtin_ia32_pminud128 (v4si, v4si);
  v8hi __builtin_ia32_pminuw128 (v8hi, v8hi);
  v4si __builtin_ia32_pmovsxbd128 (v16qi);
  v2di __builtin_ia32_pmovsxbq128 (v16qi);
  v8hi __builtin_ia32_pmovsxbw128 (v16qi);
  v2di __builtin_ia32_pmovsxdq128 (v4si);
  v4si __builtin_ia32_pmovsxwd128 (v8hi);
  v2di __builtin_ia32_pmovsxwq128 (v8hi);
  v4si __builtin_ia32_pmovzxbd128 (v16qi);
  v2di __builtin_ia32_pmovzxbq128 (v16qi);
  v8hi __builtin_ia32_pmovzxbw128 (v16qi);
  v2di __builtin_ia32_pmovzxdq128 (v4si);
  v4si __builtin_ia32_pmovzxwd128 (v8hi);
  v2di __builtin_ia32_pmovzxwq128 (v8hi);
  v2di __builtin_ia32_pmuldq128 (v4si, v4si);
  v4si __builtin_ia32_pmulld128 (v4si, v4si);
  int __builtin_ia32_ptestc128 (v2di, v2di);
  int __builtin_ia32_ptestnzc128 (v2di, v2di);
  int __builtin_ia32_ptestz128 (v2di, v2di);
  v2df __builtin_ia32_roundpd (v2df, const int);
  v4sf __builtin_ia32_roundps (v4sf, const int);
  v2df __builtin_ia32_roundsd (v2df, v2df, const int);
  v4sf __builtin_ia32_roundss (v4sf, v4sf, const int);

The following built-in functions are available when :option:`-msse4.1` is
used.

.. function:: v4sf __builtin_ia32_vec_set_v4sf (v4sf, float, const int)

  Generates the ``insertps`` machine instruction.

.. function:: int __builtin_ia32_vec_ext_v16qi (v16qi, const int)

  Generates the ``pextrb`` machine instruction.

.. function:: v16qi __builtin_ia32_vec_set_v16qi (v16qi, int, const int)

  Generates the ``pinsrb`` machine instruction.

.. function:: v4si __builtin_ia32_vec_set_v4si (v4si, int, const int)

  Generates the ``pinsrd`` machine instruction.

.. function:: v2di __builtin_ia32_vec_set_v2di (v2di, long long, const int)

  Generates the ``pinsrq`` machine instruction in 64bit mode.

The following built-in functions are changed to generate new SSE4.1
instructions when :option:`-msse4.1` is used.

.. function:: float __builtin_ia32_vec_ext_v4sf (v4sf, const int)

  Generates the ``extractps`` machine instruction.

.. function:: int __builtin_ia32_vec_ext_v4si (v4si, const int)

  Generates the ``pextrd`` machine instruction.

.. function:: long long __builtin_ia32_vec_ext_v2di (v2di, const int)

  Generates the ``pextrq`` machine instruction in 64bit mode.

The following built-in functions are available when :option:`-msse4.2` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v16qi __builtin_ia32_pcmpestrm128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestri128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestria128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestric128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestrio128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestris128 (v16qi, int, v16qi, int, const int);
  int __builtin_ia32_pcmpestriz128 (v16qi, int, v16qi, int, const int);
  v16qi __builtin_ia32_pcmpistrm128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistri128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistria128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistric128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistrio128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistris128 (v16qi, v16qi, const int);
  int __builtin_ia32_pcmpistriz128 (v16qi, v16qi, const int);
  v2di __builtin_ia32_pcmpgtq (v2di, v2di);

The following built-in functions are available when :option:`-msse4.2` is
used.

.. function:: unsigned int __builtin_ia32_crc32qi (unsigned int, unsigned char)

  Generates the ``crc32b`` machine instruction.

.. function:: unsigned int __builtin_ia32_crc32hi (unsigned int, unsigned short)

  Generates the ``crc32w`` machine instruction.

.. function:: unsigned int __builtin_ia32_crc32si (unsigned int, unsigned int)

  Generates the ``crc32l`` machine instruction.

.. function:: unsigned long long __builtin_ia32_crc32di (unsigned long long, unsigned long long)

  Generates the ``crc32q`` machine instruction.

The following built-in functions are changed to generate new SSE4.2
instructions when :option:`-msse4.2` is used.

.. function:: int __builtin_popcount (unsigned int)

  Generates the ``popcntl`` machine instruction.

.. function:: int __builtin_popcountl (unsigned long)

  Generates the ``popcntl`` or ``popcntq`` machine instruction,
  depending on the size of ``unsigned long``.

.. function:: int __builtin_popcountll (unsigned long long)

  Generates the ``popcntq`` machine instruction.

The following built-in functions are available when :option:`-mavx` is
used. All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v4df __builtin_ia32_addpd256 (v4df,v4df);
  v8sf __builtin_ia32_addps256 (v8sf,v8sf);
  v4df __builtin_ia32_addsubpd256 (v4df,v4df);
  v8sf __builtin_ia32_addsubps256 (v8sf,v8sf);
  v4df __builtin_ia32_andnpd256 (v4df,v4df);
  v8sf __builtin_ia32_andnps256 (v8sf,v8sf);
  v4df __builtin_ia32_andpd256 (v4df,v4df);
  v8sf __builtin_ia32_andps256 (v8sf,v8sf);
  v4df __builtin_ia32_blendpd256 (v4df,v4df,int);
  v8sf __builtin_ia32_blendps256 (v8sf,v8sf,int);
  v4df __builtin_ia32_blendvpd256 (v4df,v4df,v4df);
  v8sf __builtin_ia32_blendvps256 (v8sf,v8sf,v8sf);
  v2df __builtin_ia32_cmppd (v2df,v2df,int);
  v4df __builtin_ia32_cmppd256 (v4df,v4df,int);
  v4sf __builtin_ia32_cmpps (v4sf,v4sf,int);
  v8sf __builtin_ia32_cmpps256 (v8sf,v8sf,int);
  v2df __builtin_ia32_cmpsd (v2df,v2df,int);
  v4sf __builtin_ia32_cmpss (v4sf,v4sf,int);
  v4df __builtin_ia32_cvtdq2pd256 (v4si);
  v8sf __builtin_ia32_cvtdq2ps256 (v8si);
  v4si __builtin_ia32_cvtpd2dq256 (v4df);
  v4sf __builtin_ia32_cvtpd2ps256 (v4df);
  v8si __builtin_ia32_cvtps2dq256 (v8sf);
  v4df __builtin_ia32_cvtps2pd256 (v4sf);
  v4si __builtin_ia32_cvttpd2dq256 (v4df);
  v8si __builtin_ia32_cvttps2dq256 (v8sf);
  v4df __builtin_ia32_divpd256 (v4df,v4df);
  v8sf __builtin_ia32_divps256 (v8sf,v8sf);
  v8sf __builtin_ia32_dpps256 (v8sf,v8sf,int);
  v4df __builtin_ia32_haddpd256 (v4df,v4df);
  v8sf __builtin_ia32_haddps256 (v8sf,v8sf);
  v4df __builtin_ia32_hsubpd256 (v4df,v4df);
  v8sf __builtin_ia32_hsubps256 (v8sf,v8sf);
  v32qi __builtin_ia32_lddqu256 (pcchar);
  v32qi __builtin_ia32_loaddqu256 (pcchar);
  v4df __builtin_ia32_loadupd256 (pcdouble);
  v8sf __builtin_ia32_loadups256 (pcfloat);
  v2df __builtin_ia32_maskloadpd (pcv2df,v2df);
  v4df __builtin_ia32_maskloadpd256 (pcv4df,v4df);
  v4sf __builtin_ia32_maskloadps (pcv4sf,v4sf);
  v8sf __builtin_ia32_maskloadps256 (pcv8sf,v8sf);
  void __builtin_ia32_maskstorepd (pv2df,v2df,v2df);
  void __builtin_ia32_maskstorepd256 (pv4df,v4df,v4df);
  void __builtin_ia32_maskstoreps (pv4sf,v4sf,v4sf);
  void __builtin_ia32_maskstoreps256 (pv8sf,v8sf,v8sf);
  v4df __builtin_ia32_maxpd256 (v4df,v4df);
  v8sf __builtin_ia32_maxps256 (v8sf,v8sf);
  v4df __builtin_ia32_minpd256 (v4df,v4df);
  v8sf __builtin_ia32_minps256 (v8sf,v8sf);
  v4df __builtin_ia32_movddup256 (v4df);
  int __builtin_ia32_movmskpd256 (v4df);
  int __builtin_ia32_movmskps256 (v8sf);
  v8sf __builtin_ia32_movshdup256 (v8sf);
  v8sf __builtin_ia32_movsldup256 (v8sf);
  v4df __builtin_ia32_mulpd256 (v4df,v4df);
  v8sf __builtin_ia32_mulps256 (v8sf,v8sf);
  v4df __builtin_ia32_orpd256 (v4df,v4df);
  v8sf __builtin_ia32_orps256 (v8sf,v8sf);
  v2df __builtin_ia32_pd_pd256 (v4df);
  v4df __builtin_ia32_pd256_pd (v2df);
  v4sf __builtin_ia32_ps_ps256 (v8sf);
  v8sf __builtin_ia32_ps256_ps (v4sf);
  int __builtin_ia32_ptestc256 (v4di,v4di,ptest);
  int __builtin_ia32_ptestnzc256 (v4di,v4di,ptest);
  int __builtin_ia32_ptestz256 (v4di,v4di,ptest);
  v8sf __builtin_ia32_rcpps256 (v8sf);
  v4df __builtin_ia32_roundpd256 (v4df,int);
  v8sf __builtin_ia32_roundps256 (v8sf,int);
  v8sf __builtin_ia32_rsqrtps_nr256 (v8sf);
  v8sf __builtin_ia32_rsqrtps256 (v8sf);
  v4df __builtin_ia32_shufpd256 (v4df,v4df,int);
  v8sf __builtin_ia32_shufps256 (v8sf,v8sf,int);
  v4si __builtin_ia32_si_si256 (v8si);
  v8si __builtin_ia32_si256_si (v4si);
  v4df __builtin_ia32_sqrtpd256 (v4df);
  v8sf __builtin_ia32_sqrtps_nr256 (v8sf);
  v8sf __builtin_ia32_sqrtps256 (v8sf);
  void __builtin_ia32_storedqu256 (pchar,v32qi);
  void __builtin_ia32_storeupd256 (pdouble,v4df);
  void __builtin_ia32_storeups256 (pfloat,v8sf);
  v4df __builtin_ia32_subpd256 (v4df,v4df);
  v8sf __builtin_ia32_subps256 (v8sf,v8sf);
  v4df __builtin_ia32_unpckhpd256 (v4df,v4df);
  v8sf __builtin_ia32_unpckhps256 (v8sf,v8sf);
  v4df __builtin_ia32_unpcklpd256 (v4df,v4df);
  v8sf __builtin_ia32_unpcklps256 (v8sf,v8sf);
  v4df __builtin_ia32_vbroadcastf128_pd256 (pcv2df);
  v8sf __builtin_ia32_vbroadcastf128_ps256 (pcv4sf);
  v4df __builtin_ia32_vbroadcastsd256 (pcdouble);
  v4sf __builtin_ia32_vbroadcastss (pcfloat);
  v8sf __builtin_ia32_vbroadcastss256 (pcfloat);
  v2df __builtin_ia32_vextractf128_pd256 (v4df,int);
  v4sf __builtin_ia32_vextractf128_ps256 (v8sf,int);
  v4si __builtin_ia32_vextractf128_si256 (v8si,int);
  v4df __builtin_ia32_vinsertf128_pd256 (v4df,v2df,int);
  v8sf __builtin_ia32_vinsertf128_ps256 (v8sf,v4sf,int);
  v8si __builtin_ia32_vinsertf128_si256 (v8si,v4si,int);
  v4df __builtin_ia32_vperm2f128_pd256 (v4df,v4df,int);
  v8sf __builtin_ia32_vperm2f128_ps256 (v8sf,v8sf,int);
  v8si __builtin_ia32_vperm2f128_si256 (v8si,v8si,int);
  v2df __builtin_ia32_vpermil2pd (v2df,v2df,v2di,int);
  v4df __builtin_ia32_vpermil2pd256 (v4df,v4df,v4di,int);
  v4sf __builtin_ia32_vpermil2ps (v4sf,v4sf,v4si,int);
  v8sf __builtin_ia32_vpermil2ps256 (v8sf,v8sf,v8si,int);
  v2df __builtin_ia32_vpermilpd (v2df,int);
  v4df __builtin_ia32_vpermilpd256 (v4df,int);
  v4sf __builtin_ia32_vpermilps (v4sf,int);
  v8sf __builtin_ia32_vpermilps256 (v8sf,int);
  v2df __builtin_ia32_vpermilvarpd (v2df,v2di);
  v4df __builtin_ia32_vpermilvarpd256 (v4df,v4di);
  v4sf __builtin_ia32_vpermilvarps (v4sf,v4si);
  v8sf __builtin_ia32_vpermilvarps256 (v8sf,v8si);
  int __builtin_ia32_vtestcpd (v2df,v2df,ptest);
  int __builtin_ia32_vtestcpd256 (v4df,v4df,ptest);
  int __builtin_ia32_vtestcps (v4sf,v4sf,ptest);
  int __builtin_ia32_vtestcps256 (v8sf,v8sf,ptest);
  int __builtin_ia32_vtestnzcpd (v2df,v2df,ptest);
  int __builtin_ia32_vtestnzcpd256 (v4df,v4df,ptest);
  int __builtin_ia32_vtestnzcps (v4sf,v4sf,ptest);
  int __builtin_ia32_vtestnzcps256 (v8sf,v8sf,ptest);
  int __builtin_ia32_vtestzpd (v2df,v2df,ptest);
  int __builtin_ia32_vtestzpd256 (v4df,v4df,ptest);
  int __builtin_ia32_vtestzps (v4sf,v4sf,ptest);
  int __builtin_ia32_vtestzps256 (v8sf,v8sf,ptest);
  void __builtin_ia32_vzeroall (void);
  void __builtin_ia32_vzeroupper (void);
  v4df __builtin_ia32_xorpd256 (v4df,v4df);
  v8sf __builtin_ia32_xorps256 (v8sf,v8sf);

The following built-in functions are available when :option:`-mavx2` is
used. All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v32qi __builtin_ia32_mpsadbw256 (v32qi,v32qi,int);
  v32qi __builtin_ia32_pabsb256 (v32qi);
  v16hi __builtin_ia32_pabsw256 (v16hi);
  v8si __builtin_ia32_pabsd256 (v8si);
  v16hi __builtin_ia32_packssdw256 (v8si,v8si);
  v32qi __builtin_ia32_packsswb256 (v16hi,v16hi);
  v16hi __builtin_ia32_packusdw256 (v8si,v8si);
  v32qi __builtin_ia32_packuswb256 (v16hi,v16hi);
  v32qi __builtin_ia32_paddb256 (v32qi,v32qi);
  v16hi __builtin_ia32_paddw256 (v16hi,v16hi);
  v8si __builtin_ia32_paddd256 (v8si,v8si);
  v4di __builtin_ia32_paddq256 (v4di,v4di);
  v32qi __builtin_ia32_paddsb256 (v32qi,v32qi);
  v16hi __builtin_ia32_paddsw256 (v16hi,v16hi);
  v32qi __builtin_ia32_paddusb256 (v32qi,v32qi);
  v16hi __builtin_ia32_paddusw256 (v16hi,v16hi);
  v4di __builtin_ia32_palignr256 (v4di,v4di,int);
  v4di __builtin_ia32_andsi256 (v4di,v4di);
  v4di __builtin_ia32_andnotsi256 (v4di,v4di);
  v32qi __builtin_ia32_pavgb256 (v32qi,v32qi);
  v16hi __builtin_ia32_pavgw256 (v16hi,v16hi);
  v32qi __builtin_ia32_pblendvb256 (v32qi,v32qi,v32qi);
  v16hi __builtin_ia32_pblendw256 (v16hi,v16hi,int);
  v32qi __builtin_ia32_pcmpeqb256 (v32qi,v32qi);
  v16hi __builtin_ia32_pcmpeqw256 (v16hi,v16hi);
  v8si __builtin_ia32_pcmpeqd256 (c8si,v8si);
  v4di __builtin_ia32_pcmpeqq256 (v4di,v4di);
  v32qi __builtin_ia32_pcmpgtb256 (v32qi,v32qi);
  v16hi __builtin_ia32_pcmpgtw256 (16hi,v16hi);
  v8si __builtin_ia32_pcmpgtd256 (v8si,v8si);
  v4di __builtin_ia32_pcmpgtq256 (v4di,v4di);
  v16hi __builtin_ia32_phaddw256 (v16hi,v16hi);
  v8si __builtin_ia32_phaddd256 (v8si,v8si);
  v16hi __builtin_ia32_phaddsw256 (v16hi,v16hi);
  v16hi __builtin_ia32_phsubw256 (v16hi,v16hi);
  v8si __builtin_ia32_phsubd256 (v8si,v8si);
  v16hi __builtin_ia32_phsubsw256 (v16hi,v16hi);
  v32qi __builtin_ia32_pmaddubsw256 (v32qi,v32qi);
  v16hi __builtin_ia32_pmaddwd256 (v16hi,v16hi);
  v32qi __builtin_ia32_pmaxsb256 (v32qi,v32qi);
  v16hi __builtin_ia32_pmaxsw256 (v16hi,v16hi);
  v8si __builtin_ia32_pmaxsd256 (v8si,v8si);
  v32qi __builtin_ia32_pmaxub256 (v32qi,v32qi);
  v16hi __builtin_ia32_pmaxuw256 (v16hi,v16hi);
  v8si __builtin_ia32_pmaxud256 (v8si,v8si);
  v32qi __builtin_ia32_pminsb256 (v32qi,v32qi);
  v16hi __builtin_ia32_pminsw256 (v16hi,v16hi);
  v8si __builtin_ia32_pminsd256 (v8si,v8si);
  v32qi __builtin_ia32_pminub256 (v32qi,v32qi);
  v16hi __builtin_ia32_pminuw256 (v16hi,v16hi);
  v8si __builtin_ia32_pminud256 (v8si,v8si);
  int __builtin_ia32_pmovmskb256 (v32qi);
  v16hi __builtin_ia32_pmovsxbw256 (v16qi);
  v8si __builtin_ia32_pmovsxbd256 (v16qi);
  v4di __builtin_ia32_pmovsxbq256 (v16qi);
  v8si __builtin_ia32_pmovsxwd256 (v8hi);
  v4di __builtin_ia32_pmovsxwq256 (v8hi);
  v4di __builtin_ia32_pmovsxdq256 (v4si);
  v16hi __builtin_ia32_pmovzxbw256 (v16qi);
  v8si __builtin_ia32_pmovzxbd256 (v16qi);
  v4di __builtin_ia32_pmovzxbq256 (v16qi);
  v8si __builtin_ia32_pmovzxwd256 (v8hi);
  v4di __builtin_ia32_pmovzxwq256 (v8hi);
  v4di __builtin_ia32_pmovzxdq256 (v4si);
  v4di __builtin_ia32_pmuldq256 (v8si,v8si);
  v16hi __builtin_ia32_pmulhrsw256 (v16hi, v16hi);
  v16hi __builtin_ia32_pmulhuw256 (v16hi,v16hi);
  v16hi __builtin_ia32_pmulhw256 (v16hi,v16hi);
  v16hi __builtin_ia32_pmullw256 (v16hi,v16hi);
  v8si __builtin_ia32_pmulld256 (v8si,v8si);
  v4di __builtin_ia32_pmuludq256 (v8si,v8si);
  v4di __builtin_ia32_por256 (v4di,v4di);
  v16hi __builtin_ia32_psadbw256 (v32qi,v32qi);
  v32qi __builtin_ia32_pshufb256 (v32qi,v32qi);
  v8si __builtin_ia32_pshufd256 (v8si,int);
  v16hi __builtin_ia32_pshufhw256 (v16hi,int);
  v16hi __builtin_ia32_pshuflw256 (v16hi,int);
  v32qi __builtin_ia32_psignb256 (v32qi,v32qi);
  v16hi __builtin_ia32_psignw256 (v16hi,v16hi);
  v8si __builtin_ia32_psignd256 (v8si,v8si);
  v4di __builtin_ia32_pslldqi256 (v4di,int);
  v16hi __builtin_ia32_psllwi256 (16hi,int);
  v16hi __builtin_ia32_psllw256(v16hi,v8hi);
  v8si __builtin_ia32_pslldi256 (v8si,int);
  v8si __builtin_ia32_pslld256(v8si,v4si);
  v4di __builtin_ia32_psllqi256 (v4di,int);
  v4di __builtin_ia32_psllq256(v4di,v2di);
  v16hi __builtin_ia32_psrawi256 (v16hi,int);
  v16hi __builtin_ia32_psraw256 (v16hi,v8hi);
  v8si __builtin_ia32_psradi256 (v8si,int);
  v8si __builtin_ia32_psrad256 (v8si,v4si);
  v4di __builtin_ia32_psrldqi256 (v4di, int);
  v16hi __builtin_ia32_psrlwi256 (v16hi,int);
  v16hi __builtin_ia32_psrlw256 (v16hi,v8hi);
  v8si __builtin_ia32_psrldi256 (v8si,int);
  v8si __builtin_ia32_psrld256 (v8si,v4si);
  v4di __builtin_ia32_psrlqi256 (v4di,int);
  v4di __builtin_ia32_psrlq256(v4di,v2di);
  v32qi __builtin_ia32_psubb256 (v32qi,v32qi);
  v32hi __builtin_ia32_psubw256 (v16hi,v16hi);
  v8si __builtin_ia32_psubd256 (v8si,v8si);
  v4di __builtin_ia32_psubq256 (v4di,v4di);
  v32qi __builtin_ia32_psubsb256 (v32qi,v32qi);
  v16hi __builtin_ia32_psubsw256 (v16hi,v16hi);
  v32qi __builtin_ia32_psubusb256 (v32qi,v32qi);
  v16hi __builtin_ia32_psubusw256 (v16hi,v16hi);
  v32qi __builtin_ia32_punpckhbw256 (v32qi,v32qi);
  v16hi __builtin_ia32_punpckhwd256 (v16hi,v16hi);
  v8si __builtin_ia32_punpckhdq256 (v8si,v8si);
  v4di __builtin_ia32_punpckhqdq256 (v4di,v4di);
  v32qi __builtin_ia32_punpcklbw256 (v32qi,v32qi);
  v16hi __builtin_ia32_punpcklwd256 (v16hi,v16hi);
  v8si __builtin_ia32_punpckldq256 (v8si,v8si);
  v4di __builtin_ia32_punpcklqdq256 (v4di,v4di);
  v4di __builtin_ia32_pxor256 (v4di,v4di);
  v4di __builtin_ia32_movntdqa256 (pv4di);
  v4sf __builtin_ia32_vbroadcastss_ps (v4sf);
  v8sf __builtin_ia32_vbroadcastss_ps256 (v4sf);
  v4df __builtin_ia32_vbroadcastsd_pd256 (v2df);
  v4di __builtin_ia32_vbroadcastsi256 (v2di);
  v4si __builtin_ia32_pblendd128 (v4si,v4si);
  v8si __builtin_ia32_pblendd256 (v8si,v8si);
  v32qi __builtin_ia32_pbroadcastb256 (v16qi);
  v16hi __builtin_ia32_pbroadcastw256 (v8hi);
  v8si __builtin_ia32_pbroadcastd256 (v4si);
  v4di __builtin_ia32_pbroadcastq256 (v2di);
  v16qi __builtin_ia32_pbroadcastb128 (v16qi);
  v8hi __builtin_ia32_pbroadcastw128 (v8hi);
  v4si __builtin_ia32_pbroadcastd128 (v4si);
  v2di __builtin_ia32_pbroadcastq128 (v2di);
  v8si __builtin_ia32_permvarsi256 (v8si,v8si);
  v4df __builtin_ia32_permdf256 (v4df,int);
  v8sf __builtin_ia32_permvarsf256 (v8sf,v8sf);
  v4di __builtin_ia32_permdi256 (v4di,int);
  v4di __builtin_ia32_permti256 (v4di,v4di,int);
  v4di __builtin_ia32_extract128i256 (v4di,int);
  v4di __builtin_ia32_insert128i256 (v4di,v2di,int);
  v8si __builtin_ia32_maskloadd256 (pcv8si,v8si);
  v4di __builtin_ia32_maskloadq256 (pcv4di,v4di);
  v4si __builtin_ia32_maskloadd (pcv4si,v4si);
  v2di __builtin_ia32_maskloadq (pcv2di,v2di);
  void __builtin_ia32_maskstored256 (pv8si,v8si,v8si);
  void __builtin_ia32_maskstoreq256 (pv4di,v4di,v4di);
  void __builtin_ia32_maskstored (pv4si,v4si,v4si);
  void __builtin_ia32_maskstoreq (pv2di,v2di,v2di);
  v8si __builtin_ia32_psllv8si (v8si,v8si);
  v4si __builtin_ia32_psllv4si (v4si,v4si);
  v4di __builtin_ia32_psllv4di (v4di,v4di);
  v2di __builtin_ia32_psllv2di (v2di,v2di);
  v8si __builtin_ia32_psrav8si (v8si,v8si);
  v4si __builtin_ia32_psrav4si (v4si,v4si);
  v8si __builtin_ia32_psrlv8si (v8si,v8si);
  v4si __builtin_ia32_psrlv4si (v4si,v4si);
  v4di __builtin_ia32_psrlv4di (v4di,v4di);
  v2di __builtin_ia32_psrlv2di (v2di,v2di);
  v2df __builtin_ia32_gathersiv2df (v2df, pcdouble,v4si,v2df,int);
  v4df __builtin_ia32_gathersiv4df (v4df, pcdouble,v4si,v4df,int);
  v2df __builtin_ia32_gatherdiv2df (v2df, pcdouble,v2di,v2df,int);
  v4df __builtin_ia32_gatherdiv4df (v4df, pcdouble,v4di,v4df,int);
  v4sf __builtin_ia32_gathersiv4sf (v4sf, pcfloat,v4si,v4sf,int);
  v8sf __builtin_ia32_gathersiv8sf (v8sf, pcfloat,v8si,v8sf,int);
  v4sf __builtin_ia32_gatherdiv4sf (v4sf, pcfloat,v2di,v4sf,int);
  v4sf __builtin_ia32_gatherdiv4sf256 (v4sf, pcfloat,v4di,v4sf,int);
  v2di __builtin_ia32_gathersiv2di (v2di, pcint64,v4si,v2di,int);
  v4di __builtin_ia32_gathersiv4di (v4di, pcint64,v4si,v4di,int);
  v2di __builtin_ia32_gatherdiv2di (v2di, pcint64,v2di,v2di,int);
  v4di __builtin_ia32_gatherdiv4di (v4di, pcint64,v4di,v4di,int);
  v4si __builtin_ia32_gathersiv4si (v4si, pcint,v4si,v4si,int);
  v8si __builtin_ia32_gathersiv8si (v8si, pcint,v8si,v8si,int);
  v4si __builtin_ia32_gatherdiv4si (v4si, pcint,v2di,v4si,int);
  v4si __builtin_ia32_gatherdiv4si256 (v4si, pcint,v4di,v4si,int);

The following built-in functions are available when :option:`-maes` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v2di __builtin_ia32_aesenc128 (v2di, v2di);
  v2di __builtin_ia32_aesenclast128 (v2di, v2di);
  v2di __builtin_ia32_aesdec128 (v2di, v2di);
  v2di __builtin_ia32_aesdeclast128 (v2di, v2di);
  v2di __builtin_ia32_aeskeygenassist128 (v2di, const int);
  v2di __builtin_ia32_aesimc128 (v2di);

The following built-in function is available when :option:`-mpclmul` is
used.

.. function:: v2di __builtin_ia32_pclmulqdq128 (v2di, v2di, const int)

  Generates the ``pclmulqdq`` machine instruction.

The following built-in function is available when :option:`-mfsgsbase` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  unsigned int __builtin_ia32_rdfsbase32 (void);
  unsigned long long __builtin_ia32_rdfsbase64 (void);
  unsigned int __builtin_ia32_rdgsbase32 (void);
  unsigned long long __builtin_ia32_rdgsbase64 (void);
  void _writefsbase_u32 (unsigned int);
  void _writefsbase_u64 (unsigned long long);
  void _writegsbase_u32 (unsigned int);
  void _writegsbase_u64 (unsigned long long);

The following built-in function is available when :option:`-mrdrnd` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  unsigned int __builtin_ia32_rdrand16_step (unsigned short *);
  unsigned int __builtin_ia32_rdrand32_step (unsigned int *);
  unsigned int __builtin_ia32_rdrand64_step (unsigned long long *);

The following built-in function is available when :option:`-mptwrite` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  void __builtin_ia32_ptwrite32 (unsigned);
  void __builtin_ia32_ptwrite64 (unsigned long long);

The following built-in functions are available when :option:`-msse4a` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_movntsd (double *, v2df);
  void __builtin_ia32_movntss (float *, v4sf);
  v2di __builtin_ia32_extrq  (v2di, v16qi);
  v2di __builtin_ia32_extrqi (v2di, const unsigned int, const unsigned int);
  v2di __builtin_ia32_insertq (v2di, v2di);
  v2di __builtin_ia32_insertqi (v2di, v2di, const unsigned int, const unsigned int);

The following built-in functions are available when :option:`-mxop` is used.

.. code-block:: c++

  v2df __builtin_ia32_vfrczpd (v2df);
  v4sf __builtin_ia32_vfrczps (v4sf);
  v2df __builtin_ia32_vfrczsd (v2df);
  v4sf __builtin_ia32_vfrczss (v4sf);
  v4df __builtin_ia32_vfrczpd256 (v4df);
  v8sf __builtin_ia32_vfrczps256 (v8sf);
  v2di __builtin_ia32_vpcmov (v2di, v2di, v2di);
  v2di __builtin_ia32_vpcmov_v2di (v2di, v2di, v2di);
  v4si __builtin_ia32_vpcmov_v4si (v4si, v4si, v4si);
  v8hi __builtin_ia32_vpcmov_v8hi (v8hi, v8hi, v8hi);
  v16qi __builtin_ia32_vpcmov_v16qi (v16qi, v16qi, v16qi);
  v2df __builtin_ia32_vpcmov_v2df (v2df, v2df, v2df);
  v4sf __builtin_ia32_vpcmov_v4sf (v4sf, v4sf, v4sf);
  v4di __builtin_ia32_vpcmov_v4di256 (v4di, v4di, v4di);
  v8si __builtin_ia32_vpcmov_v8si256 (v8si, v8si, v8si);
  v16hi __builtin_ia32_vpcmov_v16hi256 (v16hi, v16hi, v16hi);
  v32qi __builtin_ia32_vpcmov_v32qi256 (v32qi, v32qi, v32qi);
  v4df __builtin_ia32_vpcmov_v4df256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vpcmov_v8sf256 (v8sf, v8sf, v8sf);
  v16qi __builtin_ia32_vpcomeqb (v16qi, v16qi);
  v8hi __builtin_ia32_vpcomeqw (v8hi, v8hi);
  v4si __builtin_ia32_vpcomeqd (v4si, v4si);
  v2di __builtin_ia32_vpcomeqq (v2di, v2di);
  v16qi __builtin_ia32_vpcomequb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomequd (v4si, v4si);
  v2di __builtin_ia32_vpcomequq (v2di, v2di);
  v8hi __builtin_ia32_vpcomequw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomeqw (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomfalseb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomfalsed (v4si, v4si);
  v2di __builtin_ia32_vpcomfalseq (v2di, v2di);
  v16qi __builtin_ia32_vpcomfalseub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomfalseud (v4si, v4si);
  v2di __builtin_ia32_vpcomfalseuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomfalseuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomfalsew (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomgeb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomged (v4si, v4si);
  v2di __builtin_ia32_vpcomgeq (v2di, v2di);
  v16qi __builtin_ia32_vpcomgeub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomgeud (v4si, v4si);
  v2di __builtin_ia32_vpcomgeuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomgeuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomgew (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomgtb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomgtd (v4si, v4si);
  v2di __builtin_ia32_vpcomgtq (v2di, v2di);
  v16qi __builtin_ia32_vpcomgtub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomgtud (v4si, v4si);
  v2di __builtin_ia32_vpcomgtuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomgtuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomgtw (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomleb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomled (v4si, v4si);
  v2di __builtin_ia32_vpcomleq (v2di, v2di);
  v16qi __builtin_ia32_vpcomleub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomleud (v4si, v4si);
  v2di __builtin_ia32_vpcomleuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomleuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomlew (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomltb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomltd (v4si, v4si);
  v2di __builtin_ia32_vpcomltq (v2di, v2di);
  v16qi __builtin_ia32_vpcomltub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomltud (v4si, v4si);
  v2di __builtin_ia32_vpcomltuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomltuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomltw (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomneb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomned (v4si, v4si);
  v2di __builtin_ia32_vpcomneq (v2di, v2di);
  v16qi __builtin_ia32_vpcomneub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomneud (v4si, v4si);
  v2di __builtin_ia32_vpcomneuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomneuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomnew (v8hi, v8hi);
  v16qi __builtin_ia32_vpcomtrueb (v16qi, v16qi);
  v4si __builtin_ia32_vpcomtrued (v4si, v4si);
  v2di __builtin_ia32_vpcomtrueq (v2di, v2di);
  v16qi __builtin_ia32_vpcomtrueub (v16qi, v16qi);
  v4si __builtin_ia32_vpcomtrueud (v4si, v4si);
  v2di __builtin_ia32_vpcomtrueuq (v2di, v2di);
  v8hi __builtin_ia32_vpcomtrueuw (v8hi, v8hi);
  v8hi __builtin_ia32_vpcomtruew (v8hi, v8hi);
  v4si __builtin_ia32_vphaddbd (v16qi);
  v2di __builtin_ia32_vphaddbq (v16qi);
  v8hi __builtin_ia32_vphaddbw (v16qi);
  v2di __builtin_ia32_vphadddq (v4si);
  v4si __builtin_ia32_vphaddubd (v16qi);
  v2di __builtin_ia32_vphaddubq (v16qi);
  v8hi __builtin_ia32_vphaddubw (v16qi);
  v2di __builtin_ia32_vphaddudq (v4si);
  v4si __builtin_ia32_vphadduwd (v8hi);
  v2di __builtin_ia32_vphadduwq (v8hi);
  v4si __builtin_ia32_vphaddwd (v8hi);
  v2di __builtin_ia32_vphaddwq (v8hi);
  v8hi __builtin_ia32_vphsubbw (v16qi);
  v2di __builtin_ia32_vphsubdq (v4si);
  v4si __builtin_ia32_vphsubwd (v8hi);
  v4si __builtin_ia32_vpmacsdd (v4si, v4si, v4si);
  v2di __builtin_ia32_vpmacsdqh (v4si, v4si, v2di);
  v2di __builtin_ia32_vpmacsdql (v4si, v4si, v2di);
  v4si __builtin_ia32_vpmacssdd (v4si, v4si, v4si);
  v2di __builtin_ia32_vpmacssdqh (v4si, v4si, v2di);
  v2di __builtin_ia32_vpmacssdql (v4si, v4si, v2di);
  v4si __builtin_ia32_vpmacsswd (v8hi, v8hi, v4si);
  v8hi __builtin_ia32_vpmacssww (v8hi, v8hi, v8hi);
  v4si __builtin_ia32_vpmacswd (v8hi, v8hi, v4si);
  v8hi __builtin_ia32_vpmacsww (v8hi, v8hi, v8hi);
  v4si __builtin_ia32_vpmadcsswd (v8hi, v8hi, v4si);
  v4si __builtin_ia32_vpmadcswd (v8hi, v8hi, v4si);
  v16qi __builtin_ia32_vpperm (v16qi, v16qi, v16qi);
  v16qi __builtin_ia32_vprotb (v16qi, v16qi);
  v4si __builtin_ia32_vprotd (v4si, v4si);
  v2di __builtin_ia32_vprotq (v2di, v2di);
  v8hi __builtin_ia32_vprotw (v8hi, v8hi);
  v16qi __builtin_ia32_vpshab (v16qi, v16qi);
  v4si __builtin_ia32_vpshad (v4si, v4si);
  v2di __builtin_ia32_vpshaq (v2di, v2di);
  v8hi __builtin_ia32_vpshaw (v8hi, v8hi);
  v16qi __builtin_ia32_vpshlb (v16qi, v16qi);
  v4si __builtin_ia32_vpshld (v4si, v4si);
  v2di __builtin_ia32_vpshlq (v2di, v2di);
  v8hi __builtin_ia32_vpshlw (v8hi, v8hi);

The following built-in functions are available when :option:`-mfma4` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2df __builtin_ia32_vfmaddpd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmaddps (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfmaddsd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmaddss (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfmsubpd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmsubps (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfmsubsd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmsubss (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfnmaddpd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfnmaddps (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfnmaddsd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfnmaddss (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfnmsubpd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfnmsubps (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfnmsubsd (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfnmsubss (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfmaddsubpd  (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmaddsubps  (v4sf, v4sf, v4sf);
  v2df __builtin_ia32_vfmsubaddpd  (v2df, v2df, v2df);
  v4sf __builtin_ia32_vfmsubaddps  (v4sf, v4sf, v4sf);
  v4df __builtin_ia32_vfmaddpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfmaddps256 (v8sf, v8sf, v8sf);
  v4df __builtin_ia32_vfmsubpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfmsubps256 (v8sf, v8sf, v8sf);
  v4df __builtin_ia32_vfnmaddpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfnmaddps256 (v8sf, v8sf, v8sf);
  v4df __builtin_ia32_vfnmsubpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfnmsubps256 (v8sf, v8sf, v8sf);
  v4df __builtin_ia32_vfmaddsubpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfmaddsubps256 (v8sf, v8sf, v8sf);
  v4df __builtin_ia32_vfmsubaddpd256 (v4df, v4df, v4df);
  v8sf __builtin_ia32_vfmsubaddps256 (v8sf, v8sf, v8sf);

The following built-in functions are available when :option:`-mlwp` is used.

.. code-block:: c++

  void __builtin_ia32_llwpcb16 (void *);
  void __builtin_ia32_llwpcb32 (void *);
  void __builtin_ia32_llwpcb64 (void *);
  void * __builtin_ia32_llwpcb16 (void);
  void * __builtin_ia32_llwpcb32 (void);
  void * __builtin_ia32_llwpcb64 (void);
  void __builtin_ia32_lwpval16 (unsigned short, unsigned int, unsigned short);
  void __builtin_ia32_lwpval32 (unsigned int, unsigned int, unsigned int);
  void __builtin_ia32_lwpval64 (unsigned __int64, unsigned int, unsigned int);
  unsigned char __builtin_ia32_lwpins16 (unsigned short, unsigned int, unsigned short);
  unsigned char __builtin_ia32_lwpins32 (unsigned int, unsigned int, unsigned int);
  unsigned char __builtin_ia32_lwpins64 (unsigned __int64, unsigned int, unsigned int);

The following built-in functions are available when :option:`-mbmi` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned int __builtin_ia32_bextr_u32(unsigned int, unsigned int);
  unsigned long long __builtin_ia32_bextr_u64 (unsigned long long, unsigned long long);

The following built-in functions are available when :option:`-mbmi2` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned int _bzhi_u32 (unsigned int, unsigned int);
  unsigned int _pdep_u32 (unsigned int, unsigned int);
  unsigned int _pext_u32 (unsigned int, unsigned int);
  unsigned long long _bzhi_u64 (unsigned long long, unsigned long long);
  unsigned long long _pdep_u64 (unsigned long long, unsigned long long);
  unsigned long long _pext_u64 (unsigned long long, unsigned long long);

The following built-in functions are available when :option:`-mlzcnt` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned short __builtin_ia32_lzcnt_u16(unsigned short);
  unsigned int __builtin_ia32_lzcnt_u32(unsigned int);
  unsigned long long __builtin_ia32_lzcnt_u64 (unsigned long long);

The following built-in functions are available when :option:`-mfxsr` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_fxsave (void *);
  void __builtin_ia32_fxrstor (void *);
  void __builtin_ia32_fxsave64 (void *);
  void __builtin_ia32_fxrstor64 (void *);

The following built-in functions are available when :option:`-mxsave` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_xsave (void *, long long);
  void __builtin_ia32_xrstor (void *, long long);
  void __builtin_ia32_xsave64 (void *, long long);
  void __builtin_ia32_xrstor64 (void *, long long);

The following built-in functions are available when :option:`-mxsaveopt` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_xsaveopt (void *, long long);
  void __builtin_ia32_xsaveopt64 (void *, long long);

The following built-in functions are available when :option:`-mtbm` is used.
Both of them generate the immediate form of the bextr machine instruction.

.. code-block:: c++

  unsigned int __builtin_ia32_bextri_u32 (unsigned int,
                                          const unsigned int);
  unsigned long long __builtin_ia32_bextri_u64 (unsigned long long,
                                                const unsigned long long);

The following built-in functions are available when :option:`-m3dnow` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_femms (void);
  v8qi __builtin_ia32_pavgusb (v8qi, v8qi);
  v2si __builtin_ia32_pf2id (v2sf);
  v2sf __builtin_ia32_pfacc (v2sf, v2sf);
  v2sf __builtin_ia32_pfadd (v2sf, v2sf);
  v2si __builtin_ia32_pfcmpeq (v2sf, v2sf);
  v2si __builtin_ia32_pfcmpge (v2sf, v2sf);
  v2si __builtin_ia32_pfcmpgt (v2sf, v2sf);
  v2sf __builtin_ia32_pfmax (v2sf, v2sf);
  v2sf __builtin_ia32_pfmin (v2sf, v2sf);
  v2sf __builtin_ia32_pfmul (v2sf, v2sf);
  v2sf __builtin_ia32_pfrcp (v2sf);
  v2sf __builtin_ia32_pfrcpit1 (v2sf, v2sf);
  v2sf __builtin_ia32_pfrcpit2 (v2sf, v2sf);
  v2sf __builtin_ia32_pfrsqrt (v2sf);
  v2sf __builtin_ia32_pfsub (v2sf, v2sf);
  v2sf __builtin_ia32_pfsubr (v2sf, v2sf);
  v2sf __builtin_ia32_pi2fd (v2si);
  v4hi __builtin_ia32_pmulhrw (v4hi, v4hi);

The following built-in functions are available when :option:`-m3dnowa` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2si __builtin_ia32_pf2iw (v2sf);
  v2sf __builtin_ia32_pfnacc (v2sf, v2sf);
  v2sf __builtin_ia32_pfpnacc (v2sf, v2sf);
  v2sf __builtin_ia32_pi2fw (v2si);
  v2sf __builtin_ia32_pswapdsf (v2sf);
  v2si __builtin_ia32_pswapdsi (v2si);

The following built-in functions are available when :option:`-mrtm` is used
They are used for restricted transactional memory. These are the internal
low level functions. Normally the functions in
:ref:`x86-transactional-memory-intrinsics` should be used instead.

.. code-block:: c++

  int __builtin_ia32_xbegin ();
  void __builtin_ia32_xend ();
  void __builtin_ia32_xabort (status);
  int __builtin_ia32_xtest ();

The following built-in functions are available when :option:`-mmwaitx` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_monitorx (void *, unsigned int, unsigned int);
  void __builtin_ia32_mwaitx (unsigned int, unsigned int, unsigned int);

The following built-in functions are available when :option:`-mclzero` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_i32_clzero (void *);

The following built-in functions are available when :option:`-mpku` is used.
They generate reads and writes to PKRU.

.. code-block:: c++

  void __builtin_ia32_wrpkru (unsigned int);
  unsigned int __builtin_ia32_rdpkru ();

The following built-in functions are available when
:option:`-mshstk` option is used.  They support shadow stack
machine instructions from Intel Control-flow Enforcement Technology (CET).
Each built-in function generates the  machine instruction that is part
of the function's name.  These are the internal low-level functions.
Normally the functions in :ref:`x86-control-flow-protection-intrinsics`
should be used instead.

.. code-block:: c++

  unsigned int __builtin_ia32_rdsspd (void);
  unsigned long long __builtin_ia32_rdsspq (void);
  void __builtin_ia32_incsspd (unsigned int);
  void __builtin_ia32_incsspq (unsigned long long);
  void __builtin_ia32_saveprevssp(void);
  void __builtin_ia32_rstorssp(void *);
  void __builtin_ia32_wrssd(unsigned int, void *);
  void __builtin_ia32_wrssq(unsigned long long, void *);
  void __builtin_ia32_wrussd(unsigned int, void *);
  void __builtin_ia32_wrussq(unsigned long long, void *);
  void __builtin_ia32_setssbsy(void);
  void __builtin_ia32_clrssbsy(void *);
