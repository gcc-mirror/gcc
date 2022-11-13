..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _powerpc-altivec-vsx-built-in-functions:

PowerPC AltiVec/VSX Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides an interface for the PowerPC family of processors to access
the AltiVec operations described in Motorola's AltiVec Programming
Interface Manual.  The interface is made available by including
``<altivec.h>`` and using :option:`-maltivec` and
:option:`-mabi=altivec`.  The interface supports the following vector
types.

.. code-block:: c++

  vector unsigned char
  vector signed char
  vector bool char

  vector unsigned short
  vector signed short
  vector bool short
  vector pixel

  vector unsigned int
  vector signed int
  vector bool int
  vector float

GCC's implementation of the high-level language interface available from
C and C++ code differs from Motorola's documentation in several ways.

* A vector constant is a list of constant expressions within curly braces.

* A vector initializer requires no cast if the vector constant is of the
  same type as the variable it is initializing.

* If ``signed`` or ``unsigned`` is omitted, the signedness of the
  vector type is the default signedness of the base type.  The default
  varies depending on the operating system, so a portable program should
  always specify the signedness.

* Compiling with :option:`-maltivec` adds keywords ``__vector``,
  ``vector``, ``__pixel``, ``pixel``, ``__bool`` and
  ``bool``.  When compiling ISO C, the context-sensitive substitution
  of the keywords ``vector``, ``pixel`` and ``bool`` is
  disabled.  To use them, you must include ``<altivec.h>`` instead.

* GCC allows using a ``typedef`` name as the type specifier for a
  vector type, but only under the following circumstances:

  * When using ``__vector`` instead of ``vector`` ; for example,

    .. code-block:: c++

      typedef signed short int16;
      __vector int16 data;

  * When using ``vector`` in keyword-and-predefine mode; for example,

    .. code-block:: c++

      typedef signed short int16;
      vector int16 data;

    Note that keyword-and-predefine mode is enabled by disabling GNU
    extensions (e.g., by using ``-std=c11``) and including
    ``<altivec.h>``.

* For C, overloaded functions are implemented with macros so the following
  does not work:

  .. code-block:: c++

      vec_add ((vector signed int){1, 2, 3, 4}, foo);

  Since ``vec_add`` is a macro, the vector constant in the example
  is treated as four separate arguments.  Wrap the entire argument in
  parentheses for this to work.

.. note::

  Only the ``<altivec.h>`` interface is supported.
  Internally, GCC uses built-in functions to achieve the functionality in
  the aforementioned header file, but they are not supported and are
  subject to change without notice.

GCC complies with the Power Vector Intrinsic Programming Reference (PVIPR),
which may be found at
https://openpowerfoundation.org/?resource_lib=power-vector-intrinsic-programming-reference.
Chapter 4 of this document fully documents the vector API interfaces
that must be
provided by compliant compilers.  Programmers should preferentially use
the interfaces described therein.  However, historically GCC has provided
additional interfaces for access to vector instructions.  These are
briefly described below.  Where the PVIPR provides a portable interface,
other functions in GCC that provide the same capabilities should be
considered deprecated.

The PVIPR documents the following overloaded functions:

.. list-table::

   * - ``vec_abs``
     - ``vec_absd``
     - ``vec_abss``
   * - ``vec_add``
     - ``vec_addc``
     - ``vec_adde``
   * - ``vec_addec``
     - ``vec_adds``
     - ``vec_all_eq``
   * - ``vec_all_ge``
     - ``vec_all_gt``
     - ``vec_all_in``
   * - ``vec_all_le``
     - ``vec_all_lt``
     - ``vec_all_nan``
   * - ``vec_all_ne``
     - ``vec_all_nge``
     - ``vec_all_ngt``
   * - ``vec_all_nle``
     - ``vec_all_nlt``
     - ``vec_all_numeric``
   * - ``vec_and``
     - ``vec_andc``
     - ``vec_any_eq``
   * - ``vec_any_ge``
     - ``vec_any_gt``
     - ``vec_any_le``
   * - ``vec_any_lt``
     - ``vec_any_nan``
     - ``vec_any_ne``
   * - ``vec_any_nge``
     - ``vec_any_ngt``
     - ``vec_any_nle``
   * - ``vec_any_nlt``
     - ``vec_any_numeric``
     - ``vec_any_out``
   * - ``vec_avg``
     - ``vec_bperm``
     - ``vec_ceil``
   * - ``vec_cipher_be``
     - ``vec_cipherlast_be``
     - ``vec_cmpb``
   * - ``vec_cmpeq``
     - ``vec_cmpge``
     - ``vec_cmpgt``
   * - ``vec_cmple``
     - ``vec_cmplt``
     - ``vec_cmpne``
   * - ``vec_cmpnez``
     - ``vec_cntlz``
     - ``vec_cntlz_lsbb``
   * - ``vec_cnttz``
     - ``vec_cnttz_lsbb``
     - ``vec_cpsgn``
   * - ``vec_ctf``
     - ``vec_cts``
     - ``vec_ctu``
   * - ``vec_div``
     - ``vec_double``
     - ``vec_doublee``
   * - ``vec_doubleh``
     - ``vec_doublel``
     - ``vec_doubleo``
   * - ``vec_eqv``
     - ``vec_expte``
     - ``vec_extract``
   * - ``vec_extract_exp``
     - ``vec_extract_fp32_from_shorth``
     - ``vec_extract_fp32_from_shortl``
   * - ``vec_extract_sig``
     - ``vec_extract_4b``
     - ``vec_first_match_index``
   * - ``vec_first_match_or_eos_index``
     - ``vec_first_mismatch_index``
     - ``vec_first_mismatch_or_eos_index``
   * - ``vec_float``
     - ``vec_float2``
     - ``vec_floate``
   * - ``vec_floato``
     - ``vec_floor``
     - ``vec_gb``
   * - ``vec_insert``
     - ``vec_insert_exp``
     - ``vec_insert4b``
   * - ``vec_ld``
     - ``vec_lde``
     - ``vec_ldl``
   * - ``vec_loge``
     - ``vec_madd``
     - ``vec_madds``
   * - ``vec_max``
     - ``vec_mergee``
     - ``vec_mergeh``
   * - ``vec_mergel``
     - ``vec_mergeo``
     - ``vec_mfvscr``
   * - ``vec_min``
     - ``vec_mradds``
     - ``vec_msub``
   * - ``vec_msum``
     - ``vec_msums``
     - ``vec_mtvscr``
   * - ``vec_mul``
     - ``vec_mule``
     - ``vec_mulo``
   * - ``vec_nabs``
     - ``vec_nand``
     - ``vec_ncipher_be``
   * - ``vec_ncipherlast_be``
     - ``vec_nearbyint``
     - ``vec_neg``
   * - ``vec_nmadd``
     - ``vec_nmsub``
     - ``vec_nor``
   * - ``vec_or``
     - ``vec_orc``
     - ``vec_pack``
   * - ``vec_pack_to_short_fp32``
     - ``vec_packpx``
     - ``vec_packs``
   * - ``vec_packsu``
     - ``vec_parity_lsbb``
     - ``vec_perm``
   * - ``vec_permxor``
     - ``vec_pmsum_be``
     - ``vec_popcnt``
   * - ``vec_re``
     - ``vec_recipdiv``
     - ``vec_revb``
   * - ``vec_reve``
     - ``vec_rint``
     - ``vec_rl``
   * - ``vec_rlmi``
     - ``vec_rlnm``
     - ``vec_round``
   * - ``vec_rsqrt``
     - ``vec_rsqrte``
     - ``vec_sbox_be``
   * - ``vec_sel``
     - ``vec_shasigma_be``
     - ``vec_signed``
   * - ``vec_signed2``
     - ``vec_signede``
     - ``vec_signedo``
   * - ``vec_sl``
     - ``vec_sld``
     - ``vec_sldw``
   * - ``vec_sll``
     - ``vec_slo``
     - ``vec_slv``
   * - ``vec_splat``
     - ``vec_splat_s8``
     - ``vec_splat_s16``
   * - ``vec_splat_s32``
     - ``vec_splat_u8``
     - ``vec_splat_u16``
   * - ``vec_splat_u32``
     - ``vec_splats``
     - ``vec_sqrt``
   * - ``vec_sr``
     - ``vec_sra``
     - ``vec_srl``
   * - ``vec_sro``
     - ``vec_srv``
     - ``vec_st``
   * - ``vec_ste``
     - ``vec_stl``
     - ``vec_sub``
   * - ``vec_subc``
     - ``vec_sube``
     - ``vec_subec``
   * - ``vec_subs``
     - ``vec_sum2s``
     - ``vec_sum4s``
   * - ``vec_sums``
     - ``vec_test_data_class``
     - ``vec_trunc``
   * - ``vec_unpackh``
     - ``vec_unpackl``
     - ``vec_unsigned``
   * - ``vec_unsigned2``
     - ``vec_unsignede``
     - ``vec_unsignedo``
   * - ``vec_xl``
     - ``vec_xl_be``
     - ``vec_xl_len``
   * - ``vec_xl_len_r``
     - ``vec_xor``
     - ``vec_xst``
   * - ``vec_xst_be``
     - ``vec_xst_len``
     - ``vec_xst_len_r``

.. toctree::
  :maxdepth: 2


.. _powerpc-altivec-built-in-functions-on-isa-2.05:

PowerPC AltiVec Built-in Functions on ISA 2.05
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following interfaces are supported for the generic and specific
AltiVec operations and the AltiVec predicates.  In cases where there
is a direct mapping between generic and specific operations, only the
generic names are shown here, although the specific operations can also
be used.

Arguments that are documented as ``const int`` require literal
integral values within the range required for that operation.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  void vec_dss (const int);

  void vec_dssall (void);

  void vec_dst (const vector unsigned char *, int, const int);
  void vec_dst (const vector signed char *, int, const int);
  void vec_dst (const vector bool char *, int, const int);
  void vec_dst (const vector unsigned short *, int, const int);
  void vec_dst (const vector signed short *, int, const int);
  void vec_dst (const vector bool short *, int, const int);
  void vec_dst (const vector pixel *, int, const int);
  void vec_dst (const vector unsigned int *, int, const int);
  void vec_dst (const vector signed int *, int, const int);
  void vec_dst (const vector bool int *, int, const int);
  void vec_dst (const vector float *, int, const int);
  void vec_dst (const unsigned char *, int, const int);
  void vec_dst (const signed char *, int, const int);
  void vec_dst (const unsigned short *, int, const int);
  void vec_dst (const short *, int, const int);
  void vec_dst (const unsigned int *, int, const int);
  void vec_dst (const int *, int, const int);
  void vec_dst (const float *, int, const int);

  void vec_dstst (const vector unsigned char *, int, const int);
  void vec_dstst (const vector signed char *, int, const int);
  void vec_dstst (const vector bool char *, int, const int);
  void vec_dstst (const vector unsigned short *, int, const int);
  void vec_dstst (const vector signed short *, int, const int);
  void vec_dstst (const vector bool short *, int, const int);
  void vec_dstst (const vector pixel *, int, const int);
  void vec_dstst (const vector unsigned int *, int, const int);
  void vec_dstst (const vector signed int *, int, const int);
  void vec_dstst (const vector bool int *, int, const int);
  void vec_dstst (const vector float *, int, const int);
  void vec_dstst (const unsigned char *, int, const int);
  void vec_dstst (const signed char *, int, const int);
  void vec_dstst (const unsigned short *, int, const int);
  void vec_dstst (const short *, int, const int);
  void vec_dstst (const unsigned int *, int, const int);
  void vec_dstst (const int *, int, const int);
  void vec_dstst (const unsigned long *, int, const int);
  void vec_dstst (const long *, int, const int);
  void vec_dstst (const float *, int, const int);

  void vec_dststt (const vector unsigned char *, int, const int);
  void vec_dststt (const vector signed char *, int, const int);
  void vec_dststt (const vector bool char *, int, const int);
  void vec_dststt (const vector unsigned short *, int, const int);
  void vec_dststt (const vector signed short *, int, const int);
  void vec_dststt (const vector bool short *, int, const int);
  void vec_dststt (const vector pixel *, int, const int);
  void vec_dststt (const vector unsigned int *, int, const int);
  void vec_dststt (const vector signed int *, int, const int);
  void vec_dststt (const vector bool int *, int, const int);
  void vec_dststt (const vector float *, int, const int);
  void vec_dststt (const unsigned char *, int, const int);
  void vec_dststt (const signed char *, int, const int);
  void vec_dststt (const unsigned short *, int, const int);
  void vec_dststt (const short *, int, const int);
  void vec_dststt (const unsigned int *, int, const int);
  void vec_dststt (const int *, int, const int);
  void vec_dststt (const float *, int, const int);

  void vec_dstt (const vector unsigned char *, int, const int);
  void vec_dstt (const vector signed char *, int, const int);
  void vec_dstt (const vector bool char *, int, const int);
  void vec_dstt (const vector unsigned short *, int, const int);
  void vec_dstt (const vector signed short *, int, const int);
  void vec_dstt (const vector bool short *, int, const int);
  void vec_dstt (const vector pixel *, int, const int);
  void vec_dstt (const vector unsigned int *, int, const int);
  void vec_dstt (const vector signed int *, int, const int);
  void vec_dstt (const vector bool int *, int, const int);
  void vec_dstt (const vector float *, int, const int);
  void vec_dstt (const unsigned char *, int, const int);
  void vec_dstt (const signed char *, int, const int);
  void vec_dstt (const unsigned short *, int, const int);
  void vec_dstt (const short *, int, const int);
  void vec_dstt (const unsigned int *, int, const int);
  void vec_dstt (const int *, int, const int);
  void vec_dstt (const float *, int, const int);

  vector signed char vec_lvebx (int, char *);
  vector unsigned char vec_lvebx (int, unsigned char *);

  vector signed short vec_lvehx (int, short *);
  vector unsigned short vec_lvehx (int, unsigned short *);

  vector float vec_lvewx (int, float *);
  vector signed int vec_lvewx (int, int *);
  vector unsigned int vec_lvewx (int, unsigned int *);

  vector unsigned char vec_lvsl (int, const unsigned char *);
  vector unsigned char vec_lvsl (int, const signed char *);
  vector unsigned char vec_lvsl (int, const unsigned short *);
  vector unsigned char vec_lvsl (int, const short *);
  vector unsigned char vec_lvsl (int, const unsigned int *);
  vector unsigned char vec_lvsl (int, const int *);
  vector unsigned char vec_lvsl (int, const float *);

  vector unsigned char vec_lvsr (int, const unsigned char *);
  vector unsigned char vec_lvsr (int, const signed char *);
  vector unsigned char vec_lvsr (int, const unsigned short *);
  vector unsigned char vec_lvsr (int, const short *);
  vector unsigned char vec_lvsr (int, const unsigned int *);
  vector unsigned char vec_lvsr (int, const int *);
  vector unsigned char vec_lvsr (int, const float *);

  void vec_stvebx (vector signed char, int, signed char *);
  void vec_stvebx (vector unsigned char, int, unsigned char *);
  void vec_stvebx (vector bool char, int, signed char *);
  void vec_stvebx (vector bool char, int, unsigned char *);

  void vec_stvehx (vector signed short, int, short *);
  void vec_stvehx (vector unsigned short, int, unsigned short *);
  void vec_stvehx (vector bool short, int, short *);
  void vec_stvehx (vector bool short, int, unsigned short *);

  void vec_stvewx (vector float, int, float *);
  void vec_stvewx (vector signed int, int, int *);
  void vec_stvewx (vector unsigned int, int, unsigned int *);
  void vec_stvewx (vector bool int, int, int *);
  void vec_stvewx (vector bool int, int, unsigned int *);

  vector float vec_vaddfp (vector float, vector float);

  vector signed char vec_vaddsbs (vector bool char, vector signed char);
  vector signed char vec_vaddsbs (vector signed char, vector bool char);
  vector signed char vec_vaddsbs (vector signed char, vector signed char);

  vector signed short vec_vaddshs (vector bool short, vector signed short);
  vector signed short vec_vaddshs (vector signed short, vector bool short);
  vector signed short vec_vaddshs (vector signed short, vector signed short);

  vector signed int vec_vaddsws (vector bool int, vector signed int);
  vector signed int vec_vaddsws (vector signed int, vector bool int);
  vector signed int vec_vaddsws (vector signed int, vector signed int);

  vector signed char vec_vaddubm (vector bool char, vector signed char);
  vector signed char vec_vaddubm (vector signed char, vector bool char);
  vector signed char vec_vaddubm (vector signed char, vector signed char);
  vector unsigned char vec_vaddubm (vector bool char, vector unsigned char);
  vector unsigned char vec_vaddubm (vector unsigned char, vector bool char);
  vector unsigned char vec_vaddubm (vector unsigned char, vector unsigned char);

  vector unsigned char vec_vaddubs (vector bool char, vector unsigned char);
  vector unsigned char vec_vaddubs (vector unsigned char, vector bool char);
  vector unsigned char vec_vaddubs (vector unsigned char, vector unsigned char);

  vector signed short vec_vadduhm (vector bool short, vector signed short);
  vector signed short vec_vadduhm (vector signed short, vector bool short);
  vector signed short vec_vadduhm (vector signed short, vector signed short);
  vector unsigned short vec_vadduhm (vector bool short, vector unsigned short);
  vector unsigned short vec_vadduhm (vector unsigned short, vector bool short);
  vector unsigned short vec_vadduhm (vector unsigned short, vector unsigned short);

  vector unsigned short vec_vadduhs (vector bool short, vector unsigned short);
  vector unsigned short vec_vadduhs (vector unsigned short, vector bool short);
  vector unsigned short vec_vadduhs (vector unsigned short, vector unsigned short);

  vector signed int vec_vadduwm (vector bool int, vector signed int);
  vector signed int vec_vadduwm (vector signed int, vector bool int);
  vector signed int vec_vadduwm (vector signed int, vector signed int);
  vector unsigned int vec_vadduwm (vector bool int, vector unsigned int);
  vector unsigned int vec_vadduwm (vector unsigned int, vector bool int);
  vector unsigned int vec_vadduwm (vector unsigned int, vector unsigned int);

  vector unsigned int vec_vadduws (vector bool int, vector unsigned int);
  vector unsigned int vec_vadduws (vector unsigned int, vector bool int);
  vector unsigned int vec_vadduws (vector unsigned int, vector unsigned int);

  vector signed char vec_vavgsb (vector signed char, vector signed char);

  vector signed short vec_vavgsh (vector signed short, vector signed short);

  vector signed int vec_vavgsw (vector signed int, vector signed int);

  vector unsigned char vec_vavgub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vavguh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vavguw (vector unsigned int, vector unsigned int);

  vector float vec_vcfsx (vector signed int, const int);

  vector float vec_vcfux (vector unsigned int, const int);

  vector bool int vec_vcmpeqfp (vector float, vector float);

  vector bool char vec_vcmpequb (vector signed char, vector signed char);
  vector bool char vec_vcmpequb (vector unsigned char, vector unsigned char);

  vector bool short vec_vcmpequh (vector signed short, vector signed short);
  vector bool short vec_vcmpequh (vector unsigned short, vector unsigned short);

  vector bool int vec_vcmpequw (vector signed int, vector signed int);
  vector bool int vec_vcmpequw (vector unsigned int, vector unsigned int);

  vector bool int vec_vcmpgtfp (vector float, vector float);

  vector bool char vec_vcmpgtsb (vector signed char, vector signed char);

  vector bool short vec_vcmpgtsh (vector signed short, vector signed short);

  vector bool int vec_vcmpgtsw (vector signed int, vector signed int);

  vector bool char vec_vcmpgtub (vector unsigned char, vector unsigned char);

  vector bool short vec_vcmpgtuh (vector unsigned short, vector unsigned short);

  vector bool int vec_vcmpgtuw (vector unsigned int, vector unsigned int);

  vector float vec_vmaxfp (vector float, vector float);

  vector signed char vec_vmaxsb (vector bool char, vector signed char);
  vector signed char vec_vmaxsb (vector signed char, vector bool char);
  vector signed char vec_vmaxsb (vector signed char, vector signed char);

  vector signed short vec_vmaxsh (vector bool short, vector signed short);
  vector signed short vec_vmaxsh (vector signed short, vector bool short);
  vector signed short vec_vmaxsh (vector signed short, vector signed short);

  vector signed int vec_vmaxsw (vector bool int, vector signed int);
  vector signed int vec_vmaxsw (vector signed int, vector bool int);
  vector signed int vec_vmaxsw (vector signed int, vector signed int);

  vector unsigned char vec_vmaxub (vector bool char, vector unsigned char);
  vector unsigned char vec_vmaxub (vector unsigned char, vector bool char);
  vector unsigned char vec_vmaxub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vmaxuh (vector bool short, vector unsigned short);
  vector unsigned short vec_vmaxuh (vector unsigned short, vector bool short);
  vector unsigned short vec_vmaxuh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vmaxuw (vector bool int, vector unsigned int);
  vector unsigned int vec_vmaxuw (vector unsigned int, vector bool int);
  vector unsigned int vec_vmaxuw (vector unsigned int, vector unsigned int);

  vector float vec_vminfp (vector float, vector float);

  vector signed char vec_vminsb (vector bool char, vector signed char);
  vector signed char vec_vminsb (vector signed char, vector bool char);
  vector signed char vec_vminsb (vector signed char, vector signed char);

  vector signed short vec_vminsh (vector bool short, vector signed short);
  vector signed short vec_vminsh (vector signed short, vector bool short);
  vector signed short vec_vminsh (vector signed short, vector signed short);

  vector signed int vec_vminsw (vector bool int, vector signed int);
  vector signed int vec_vminsw (vector signed int, vector bool int);
  vector signed int vec_vminsw (vector signed int, vector signed int);

  vector unsigned char vec_vminub (vector bool char, vector unsigned char);
  vector unsigned char vec_vminub (vector unsigned char, vector bool char);
  vector unsigned char vec_vminub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vminuh (vector bool short, vector unsigned short);
  vector unsigned short vec_vminuh (vector unsigned short, vector bool short);
  vector unsigned short vec_vminuh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vminuw (vector bool int, vector unsigned int);
  vector unsigned int vec_vminuw (vector unsigned int, vector bool int);
  vector unsigned int vec_vminuw (vector unsigned int, vector unsigned int);

  vector bool char vec_vmrghb (vector bool char, vector bool char);
  vector signed char vec_vmrghb (vector signed char, vector signed char);
  vector unsigned char vec_vmrghb (vector unsigned char, vector unsigned char);

  vector bool short vec_vmrghh (vector bool short, vector bool short);
  vector signed short vec_vmrghh (vector signed short, vector signed short);
  vector unsigned short vec_vmrghh (vector unsigned short, vector unsigned short);
  vector pixel vec_vmrghh (vector pixel, vector pixel);

  vector float vec_vmrghw (vector float, vector float);
  vector bool int vec_vmrghw (vector bool int, vector bool int);
  vector signed int vec_vmrghw (vector signed int, vector signed int);
  vector unsigned int vec_vmrghw (vector unsigned int, vector unsigned int);

  vector bool char vec_vmrglb (vector bool char, vector bool char);
  vector signed char vec_vmrglb (vector signed char, vector signed char);
  vector unsigned char vec_vmrglb (vector unsigned char, vector unsigned char);

  vector bool short vec_vmrglh (vector bool short, vector bool short);
  vector signed short vec_vmrglh (vector signed short, vector signed short);
  vector unsigned short vec_vmrglh (vector unsigned short, vector unsigned short);
  vector pixel vec_vmrglh (vector pixel, vector pixel);

  vector float vec_vmrglw (vector float, vector float);
  vector signed int vec_vmrglw (vector signed int, vector signed int);
  vector unsigned int vec_vmrglw (vector unsigned int, vector unsigned int);
  vector bool int vec_vmrglw (vector bool int, vector bool int);

  vector signed int vec_vmsummbm (vector signed char, vector unsigned char,
                                  vector signed int);

  vector signed int vec_vmsumshm (vector signed short, vector signed short,
                                  vector signed int);

  vector signed int vec_vmsumshs (vector signed short, vector signed short,
                                  vector signed int);

  vector unsigned int vec_vmsumubm (vector unsigned char, vector unsigned char,
                                    vector unsigned int);

  vector unsigned int vec_vmsumuhm (vector unsigned short, vector unsigned short,
                                    vector unsigned int);

  vector unsigned int vec_vmsumuhs (vector unsigned short, vector unsigned short,
                                    vector unsigned int);

  vector signed short vec_vmulesb (vector signed char, vector signed char);

  vector signed int vec_vmulesh (vector signed short, vector signed short);

  vector unsigned short vec_vmuleub (vector unsigned char, vector unsigned char);

  vector unsigned int vec_vmuleuh (vector unsigned short, vector unsigned short);

  vector signed short vec_vmulosb (vector signed char, vector signed char);

  vector signed int vec_vmulosh (vector signed short, vector signed short);

  vector unsigned short vec_vmuloub (vector unsigned char, vector unsigned char);

  vector unsigned int vec_vmulouh (vector unsigned short, vector unsigned short);

  vector signed char vec_vpkshss (vector signed short, vector signed short);

  vector unsigned char vec_vpkshus (vector signed short, vector signed short);

  vector signed short vec_vpkswss (vector signed int, vector signed int);

  vector unsigned short vec_vpkswus (vector signed int, vector signed int);

  vector bool char vec_vpkuhum (vector bool short, vector bool short);
  vector signed char vec_vpkuhum (vector signed short, vector signed short);
  vector unsigned char vec_vpkuhum (vector unsigned short, vector unsigned short);

  vector unsigned char vec_vpkuhus (vector unsigned short, vector unsigned short);

  vector bool short vec_vpkuwum (vector bool int, vector bool int);
  vector signed short vec_vpkuwum (vector signed int, vector signed int);
  vector unsigned short vec_vpkuwum (vector unsigned int, vector unsigned int);

  vector unsigned short vec_vpkuwus (vector unsigned int, vector unsigned int);

  vector signed char vec_vrlb (vector signed char, vector unsigned char);
  vector unsigned char vec_vrlb (vector unsigned char, vector unsigned char);

  vector signed short vec_vrlh (vector signed short, vector unsigned short);
  vector unsigned short vec_vrlh (vector unsigned short, vector unsigned short);

  vector signed int vec_vrlw (vector signed int, vector unsigned int);
  vector unsigned int vec_vrlw (vector unsigned int, vector unsigned int);

  vector signed char vec_vslb (vector signed char, vector unsigned char);
  vector unsigned char vec_vslb (vector unsigned char, vector unsigned char);

  vector signed short vec_vslh (vector signed short, vector unsigned short);
  vector unsigned short vec_vslh (vector unsigned short, vector unsigned short);

  vector signed int vec_vslw (vector signed int, vector unsigned int);
  vector unsigned int vec_vslw (vector unsigned int, vector unsigned int);

  vector signed char vec_vspltb (vector signed char, const int);
  vector unsigned char vec_vspltb (vector unsigned char, const int);
  vector bool char vec_vspltb (vector bool char, const int);

  vector bool short vec_vsplth (vector bool short, const int);
  vector signed short vec_vsplth (vector signed short, const int);
  vector unsigned short vec_vsplth (vector unsigned short, const int);
  vector pixel vec_vsplth (vector pixel, const int);

  vector float vec_vspltw (vector float, const int);
  vector signed int vec_vspltw (vector signed int, const int);
  vector unsigned int vec_vspltw (vector unsigned int, const int);
  vector bool int vec_vspltw (vector bool int, const int);

  vector signed char vec_vsrab (vector signed char, vector unsigned char);
  vector unsigned char vec_vsrab (vector unsigned char, vector unsigned char);

  vector signed short vec_vsrah (vector signed short, vector unsigned short);
  vector unsigned short vec_vsrah (vector unsigned short, vector unsigned short);

  vector signed int vec_vsraw (vector signed int, vector unsigned int);
  vector unsigned int vec_vsraw (vector unsigned int, vector unsigned int);

  vector signed char vec_vsrb (vector signed char, vector unsigned char);
  vector unsigned char vec_vsrb (vector unsigned char, vector unsigned char);

  vector signed short vec_vsrh (vector signed short, vector unsigned short);
  vector unsigned short vec_vsrh (vector unsigned short, vector unsigned short);

  vector signed int vec_vsrw (vector signed int, vector unsigned int);
  vector unsigned int vec_vsrw (vector unsigned int, vector unsigned int);

  vector float vec_vsubfp (vector float, vector float);

  vector signed char vec_vsubsbs (vector bool char, vector signed char);
  vector signed char vec_vsubsbs (vector signed char, vector bool char);
  vector signed char vec_vsubsbs (vector signed char, vector signed char);

  vector signed short vec_vsubshs (vector bool short, vector signed short);
  vector signed short vec_vsubshs (vector signed short, vector bool short);
  vector signed short vec_vsubshs (vector signed short, vector signed short);

  vector signed int vec_vsubsws (vector bool int, vector signed int);
  vector signed int vec_vsubsws (vector signed int, vector bool int);
  vector signed int vec_vsubsws (vector signed int, vector signed int);

  vector signed char vec_vsububm (vector bool char, vector signed char);
  vector signed char vec_vsububm (vector signed char, vector bool char);
  vector signed char vec_vsububm (vector signed char, vector signed char);
  vector unsigned char vec_vsububm (vector bool char, vector unsigned char);
  vector unsigned char vec_vsububm (vector unsigned char, vector bool char);
  vector unsigned char vec_vsububm (vector unsigned char, vector unsigned char);

  vector unsigned char vec_vsububs (vector bool char, vector unsigned char);
  vector unsigned char vec_vsububs (vector unsigned char, vector bool char);
  vector unsigned char vec_vsububs (vector unsigned char, vector unsigned char);

  vector signed short vec_vsubuhm (vector bool short, vector signed short);
  vector signed short vec_vsubuhm (vector signed short, vector bool short);
  vector signed short vec_vsubuhm (vector signed short, vector signed short);
  vector unsigned short vec_vsubuhm (vector bool short, vector unsigned short);
  vector unsigned short vec_vsubuhm (vector unsigned short, vector bool short);
  vector unsigned short vec_vsubuhm (vector unsigned short, vector unsigned short);

  vector unsigned short vec_vsubuhs (vector bool short, vector unsigned short);
  vector unsigned short vec_vsubuhs (vector unsigned short, vector bool short);
  vector unsigned short vec_vsubuhs (vector unsigned short, vector unsigned short);

  vector signed int vec_vsubuwm (vector bool int, vector signed int);
  vector signed int vec_vsubuwm (vector signed int, vector bool int);
  vector signed int vec_vsubuwm (vector signed int, vector signed int);
  vector unsigned int vec_vsubuwm (vector bool int, vector unsigned int);
  vector unsigned int vec_vsubuwm (vector unsigned int, vector bool int);
  vector unsigned int vec_vsubuwm (vector unsigned int, vector unsigned int);

  vector unsigned int vec_vsubuws (vector bool int, vector unsigned int);
  vector unsigned int vec_vsubuws (vector unsigned int, vector bool int);
  vector unsigned int vec_vsubuws (vector unsigned int, vector unsigned int);

  vector signed int vec_vsum4sbs (vector signed char, vector signed int);

  vector signed int vec_vsum4shs (vector signed short, vector signed int);

  vector unsigned int vec_vsum4ubs (vector unsigned char, vector unsigned int);

  vector unsigned int vec_vupkhpx (vector pixel);

  vector bool short vec_vupkhsb (vector bool char);
  vector signed short vec_vupkhsb (vector signed char);

  vector bool int vec_vupkhsh (vector bool short);
  vector signed int vec_vupkhsh (vector signed short);

  vector unsigned int vec_vupklpx (vector pixel);

  vector bool short vec_vupklsb (vector bool char);
  vector signed short vec_vupklsb (vector signed char);

  vector bool int vec_vupklsh (vector bool short);
  vector signed int vec_vupklsh (vector signed short);

.. _powerpc-altivec-built-in-functions-available-on-isa-2.06:

PowerPC AltiVec Built-in Functions Available on ISA 2.06
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The AltiVec built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.06
or later.  These are normally enabled by adding :option:`-mvsx` to the
command line.

When :option:`-mvsx` is used, the following additional vector types are
implemented.

.. code-block:: c++

  vector unsigned __int128
  vector signed __int128
  vector unsigned long long int
  vector signed long long int
  vector double

The long long types are only implemented for 64-bit code generation.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  void vec_dst (const unsigned long *, int, const int);
  void vec_dst (const long *, int, const int);

  void vec_dststt (const unsigned long *, int, const int);
  void vec_dststt (const long *, int, const int);

  void vec_dstt (const unsigned long *, int, const int);
  void vec_dstt (const long *, int, const int);

  vector unsigned char vec_lvsl (int, const unsigned long *);
  vector unsigned char vec_lvsl (int, const long *);

  vector unsigned char vec_lvsr (int, const unsigned long *);
  vector unsigned char vec_lvsr (int, const long *);

  vector unsigned char vec_lvsl (int, const double *);
  vector unsigned char vec_lvsr (int, const double *);

  vector double vec_vsx_ld (int, const vector double *);
  vector double vec_vsx_ld (int, const double *);
  vector float vec_vsx_ld (int, const vector float *);
  vector float vec_vsx_ld (int, const float *);
  vector bool int vec_vsx_ld (int, const vector bool int *);
  vector signed int vec_vsx_ld (int, const vector signed int *);
  vector signed int vec_vsx_ld (int, const int *);
  vector signed int vec_vsx_ld (int, const long *);
  vector unsigned int vec_vsx_ld (int, const vector unsigned int *);
  vector unsigned int vec_vsx_ld (int, const unsigned int *);
  vector unsigned int vec_vsx_ld (int, const unsigned long *);
  vector bool short vec_vsx_ld (int, const vector bool short *);
  vector pixel vec_vsx_ld (int, const vector pixel *);
  vector signed short vec_vsx_ld (int, const vector signed short *);
  vector signed short vec_vsx_ld (int, const short *);
  vector unsigned short vec_vsx_ld (int, const vector unsigned short *);
  vector unsigned short vec_vsx_ld (int, const unsigned short *);
  vector bool char vec_vsx_ld (int, const vector bool char *);
  vector signed char vec_vsx_ld (int, const vector signed char *);
  vector signed char vec_vsx_ld (int, const signed char *);
  vector unsigned char vec_vsx_ld (int, const vector unsigned char *);
  vector unsigned char vec_vsx_ld (int, const unsigned char *);

  void vec_vsx_st (vector double, int, vector double *);
  void vec_vsx_st (vector double, int, double *);
  void vec_vsx_st (vector float, int, vector float *);
  void vec_vsx_st (vector float, int, float *);
  void vec_vsx_st (vector signed int, int, vector signed int *);
  void vec_vsx_st (vector signed int, int, int *);
  void vec_vsx_st (vector unsigned int, int, vector unsigned int *);
  void vec_vsx_st (vector unsigned int, int, unsigned int *);
  void vec_vsx_st (vector bool int, int, vector bool int *);
  void vec_vsx_st (vector bool int, int, unsigned int *);
  void vec_vsx_st (vector bool int, int, int *);
  void vec_vsx_st (vector signed short, int, vector signed short *);
  void vec_vsx_st (vector signed short, int, short *);
  void vec_vsx_st (vector unsigned short, int, vector unsigned short *);
  void vec_vsx_st (vector unsigned short, int, unsigned short *);
  void vec_vsx_st (vector bool short, int, vector bool short *);
  void vec_vsx_st (vector bool short, int, unsigned short *);
  void vec_vsx_st (vector pixel, int, vector pixel *);
  void vec_vsx_st (vector pixel, int, unsigned short *);
  void vec_vsx_st (vector pixel, int, short *);
  void vec_vsx_st (vector bool short, int, short *);
  void vec_vsx_st (vector signed char, int, vector signed char *);
  void vec_vsx_st (vector signed char, int, signed char *);
  void vec_vsx_st (vector unsigned char, int, vector unsigned char *);
  void vec_vsx_st (vector unsigned char, int, unsigned char *);
  void vec_vsx_st (vector bool char, int, vector bool char *);
  void vec_vsx_st (vector bool char, int, unsigned char *);
  void vec_vsx_st (vector bool char, int, signed char *);

  vector double vec_xxpermdi (vector double, vector double, const int);
  vector float vec_xxpermdi (vector float, vector float, const int);
  vector long long vec_xxpermdi (vector long long, vector long long, const int);
  vector unsigned long long vec_xxpermdi (vector unsigned long long,
                                          vector unsigned long long, const int);
  vector int vec_xxpermdi (vector int, vector int, const int);
  vector unsigned int vec_xxpermdi (vector unsigned int,
                                    vector unsigned int, const int);
  vector short vec_xxpermdi (vector short, vector short, const int);
  vector unsigned short vec_xxpermdi (vector unsigned short,
                                      vector unsigned short, const int);
  vector signed char vec_xxpermdi (vector signed char, vector signed char,
                                   const int);
  vector unsigned char vec_xxpermdi (vector unsigned char,
                                     vector unsigned char, const int);

  vector double vec_xxsldi (vector double, vector double, int);
  vector float vec_xxsldi (vector float, vector float, int);
  vector long long vec_xxsldi (vector long long, vector long long, int);
  vector unsigned long long vec_xxsldi (vector unsigned long long,
                                        vector unsigned long long, int);
  vector int vec_xxsldi (vector int, vector int, int);
  vector unsigned int vec_xxsldi (vector unsigned int, vector unsigned int, int);
  vector short vec_xxsldi (vector short, vector short, int);
  vector unsigned short vec_xxsldi (vector unsigned short,
                                    vector unsigned short, int);
  vector signed char vec_xxsldi (vector signed char, vector signed char, int);
  vector unsigned char vec_xxsldi (vector unsigned char,
                                   vector unsigned char, int);

Note that the :samp:`vec_ld` and :samp:`vec_st` built-in functions always
generate the AltiVec :samp:`LVX` and :samp:`STVX` instructions even
if the VSX instruction set is available.  The :samp:`vec_vsx_ld` and
:samp:`vec_vsx_st` built-in functions always generate the VSX :samp:`LXVD2X`,
:samp:`LXVW4X`, :samp:`STXVD2X`, and :samp:`STXVW4X` instructions.

.. _powerpc-altivec-built-in-functions-available-on-isa-2.07:

PowerPC AltiVec Built-in Functions Available on ISA 2.07
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the ISA 2.07 additions to the vector/scalar (power8-vector)
instruction set are available, the following additional functions are
available for both 32-bit and 64-bit targets.  For 64-bit targets, you
can use :samp:`{vector long}` instead of :samp:`{vector long long}`,
:samp:`{vector bool long}` instead of :samp:`{vector bool long long}`, and
:samp:`{vector unsigned long}` instead of :samp:`{vector unsigned long long}`.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector long long vec_vaddudm (vector long long, vector long long);
  vector long long vec_vaddudm (vector bool long long, vector long long);
  vector long long vec_vaddudm (vector long long, vector bool long long);
  vector unsigned long long vec_vaddudm (vector unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vaddudm (vector bool unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vaddudm (vector unsigned long long,
                                         vector bool unsigned long long);

  vector long long vec_vclz (vector long long);
  vector unsigned long long vec_vclz (vector unsigned long long);
  vector int vec_vclz (vector int);
  vector unsigned int vec_vclz (vector int);
  vector short vec_vclz (vector short);
  vector unsigned short vec_vclz (vector unsigned short);
  vector signed char vec_vclz (vector signed char);
  vector unsigned char vec_vclz (vector unsigned char);

  vector signed char vec_vclzb (vector signed char);
  vector unsigned char vec_vclzb (vector unsigned char);

  vector long long vec_vclzd (vector long long);
  vector unsigned long long vec_vclzd (vector unsigned long long);

  vector short vec_vclzh (vector short);
  vector unsigned short vec_vclzh (vector unsigned short);

  vector int vec_vclzw (vector int);
  vector unsigned int vec_vclzw (vector int);

  vector signed char vec_vgbbd (vector signed char);
  vector unsigned char vec_vgbbd (vector unsigned char);

  vector long long vec_vmaxsd (vector long long, vector long long);

  vector unsigned long long vec_vmaxud (vector unsigned long long,
                                        unsigned vector long long);

  vector long long vec_vminsd (vector long long, vector long long);

  vector unsigned long long vec_vminud (vector long long, vector long long);

  vector int vec_vpksdss (vector long long, vector long long);
  vector unsigned int vec_vpksdss (vector long long, vector long long);

  vector unsigned int vec_vpkudus (vector unsigned long long,
                                   vector unsigned long long);

  vector int vec_vpkudum (vector long long, vector long long);
  vector unsigned int vec_vpkudum (vector unsigned long long,
                                   vector unsigned long long);
  vector bool int vec_vpkudum (vector bool long long, vector bool long long);

  vector long long vec_vpopcnt (vector long long);
  vector unsigned long long vec_vpopcnt (vector unsigned long long);
  vector int vec_vpopcnt (vector int);
  vector unsigned int vec_vpopcnt (vector int);
  vector short vec_vpopcnt (vector short);
  vector unsigned short vec_vpopcnt (vector unsigned short);
  vector signed char vec_vpopcnt (vector signed char);
  vector unsigned char vec_vpopcnt (vector unsigned char);

  vector signed char vec_vpopcntb (vector signed char);
  vector unsigned char vec_vpopcntb (vector unsigned char);

  vector long long vec_vpopcntd (vector long long);
  vector unsigned long long vec_vpopcntd (vector unsigned long long);

  vector short vec_vpopcnth (vector short);
  vector unsigned short vec_vpopcnth (vector unsigned short);

  vector int vec_vpopcntw (vector int);
  vector unsigned int vec_vpopcntw (vector int);

  vector long long vec_vrld (vector long long, vector unsigned long long);
  vector unsigned long long vec_vrld (vector unsigned long long,
                                      vector unsigned long long);

  vector long long vec_vsld (vector long long, vector unsigned long long);
  vector long long vec_vsld (vector unsigned long long,
                             vector unsigned long long);

  vector long long vec_vsrad (vector long long, vector unsigned long long);
  vector unsigned long long vec_vsrad (vector unsigned long long,
                                       vector unsigned long long);

  vector long long vec_vsrd (vector long long, vector unsigned long long);
  vector unsigned long long char vec_vsrd (vector unsigned long long,
                                           vector unsigned long long);

  vector long long vec_vsubudm (vector long long, vector long long);
  vector long long vec_vsubudm (vector bool long long, vector long long);
  vector long long vec_vsubudm (vector long long, vector bool long long);
  vector unsigned long long vec_vsubudm (vector unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vsubudm (vector bool long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vsubudm (vector unsigned long long,
                                         vector bool long long);

  vector long long vec_vupkhsw (vector int);
  vector unsigned long long vec_vupkhsw (vector unsigned int);

  vector long long vec_vupklsw (vector int);
  vector unsigned long long vec_vupklsw (vector int);

If the ISA 2.07 additions to the vector/scalar (power8-vector)
instruction set are available, the following additional functions are
available for 64-bit targets.  New vector types
(:samp:`{vector __int128}` and :samp:`{vector __uint128}`) are available
to hold the :samp:`{__int128}` and :samp:`{__uint128}` types to use these
builtins.

The normal vector extract, and set operations work on
:samp:`{vector __int128}` and :samp:`{vector __uint128}` types,
but the index value must be 0.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector __int128 vec_vaddcuq (vector __int128, vector __int128);
  vector __uint128 vec_vaddcuq (vector __uint128, vector __uint128);

  vector __int128 vec_vadduqm (vector __int128, vector __int128);
  vector __uint128 vec_vadduqm (vector __uint128, vector __uint128);

  vector __int128 vec_vaddecuq (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vaddecuq (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vaddeuqm (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vaddeuqm (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubecuq (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vsubecuq (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubeuqm (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vsubeuqm (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubcuq (vector __int128, vector __int128);
  vector __uint128 vec_vsubcuq (vector __uint128, vector __uint128);

  __int128 vec_vsubuqm (__int128, __int128);
  __uint128 vec_vsubuqm (__uint128, __uint128);

  vector __int128 __builtin_bcdadd (vector __int128, vector __int128, const int);
  vector unsigned char __builtin_bcdadd (vector unsigned char, vector unsigned char,
                                         const int);
  int __builtin_bcdadd_lt (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_lt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_eq (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_eq (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_gt (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_gt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_ov (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_ov (vector unsigned char, vector unsigned char, const int);

  vector __int128 __builtin_bcdsub (vector __int128, vector __int128, const int);
  vector unsigned char __builtin_bcdsub (vector unsigned char, vector unsigned char,
                                         const int);
  int __builtin_bcdsub_lt (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_lt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_eq (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_eq (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_gt (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_gt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_ov (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_ov (vector unsigned char, vector unsigned char, const int);

.. _powerpc-altivec-built-in-functions-available-on-isa-3.0:

PowerPC AltiVec Built-in Functions Available on ISA 3.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0
(:option:`-mcpu=power9`) or later.

Only instructions excluded from the PVIPR are listed here.

.. code-block:: c++

  unsigned int scalar_extract_exp (double source);
  unsigned long long int scalar_extract_exp (__ieee128 source);

  unsigned long long int scalar_extract_sig (double source);
  unsigned __int128 scalar_extract_sig (__ieee128 source);

  double scalar_insert_exp (unsigned long long int significand,
                            unsigned long long int exponent);
  double scalar_insert_exp (double significand, unsigned long long int exponent);

  ieee_128 scalar_insert_exp (unsigned __int128 significand,
                              unsigned long long int exponent);
  ieee_128 scalar_insert_exp (ieee_128 significand, unsigned long long int exponent);

  int scalar_cmp_exp_gt (double arg1, double arg2);
  int scalar_cmp_exp_lt (double arg1, double arg2);
  int scalar_cmp_exp_eq (double arg1, double arg2);
  int scalar_cmp_exp_unordered (double arg1, double arg2);

  bool scalar_test_data_class (float source, const int condition);
  bool scalar_test_data_class (double source, const int condition);
  bool scalar_test_data_class (__ieee128 source, const int condition);

  bool scalar_test_neg (float source);
  bool scalar_test_neg (double source);
  bool scalar_test_neg (__ieee128 source);

The ``scalar_extract_exp`` and ``scalar_extract_sig``
functions require a 64-bit environment supporting ISA 3.0 or later.
The ``scalar_extract_exp`` and ``scalar_extract_sig`` built-in
functions return the significand and the biased exponent value
respectively of their ``source`` arguments.
When supplied with a 64-bit ``source`` argument, the
result returned by ``scalar_extract_sig`` has
the ``0x0010000000000000`` bit set if the
function's ``source`` argument is in normalized form.
Otherwise, this bit is set to 0.
When supplied with a 128-bit ``source`` argument, the
``0x00010000000000000000000000000000`` bit of the result is
treated similarly.
Note that the sign of the significand is not represented in the result
returned from the ``scalar_extract_sig`` function.  Use the
``scalar_test_neg`` function to test the sign of its ``double``
argument.

The ``scalar_insert_exp``
functions require a 64-bit environment supporting ISA 3.0 or later.
When supplied with a 64-bit first argument, the
``scalar_insert_exp`` built-in function returns a double-precision
floating point value that is constructed by assembling the values of its
``significand`` and ``exponent`` arguments.  The sign of the
result is copied from the most significant bit of the
``significand`` argument.  The significand and exponent components
of the result are composed of the least significant 11 bits of the
``exponent`` argument and the least significant 52 bits of the
``significand`` argument respectively.

When supplied with a 128-bit first argument, the
``scalar_insert_exp`` built-in function returns a quad-precision
ieee floating point value.  The sign bit of the result is copied from
the most significant bit of the ``significand`` argument.
The significand and exponent components of the result are composed of
the least significant 15 bits of the ``exponent`` argument and the
least significant 112 bits of the ``significand`` argument respectively.

The ``scalar_cmp_exp_gt``, ``scalar_cmp_exp_lt``,
``scalar_cmp_exp_eq``, and ``scalar_cmp_exp_unordered`` built-in
functions return a non-zero value if ``arg1`` is greater than, less
than, equal to, or not comparable to ``arg2`` respectively.  The
arguments are not comparable if one or the other equals NaN (not a
number).

The ``scalar_test_data_class`` built-in function returns 1
if any of the condition tests enabled by the value of the
``condition`` variable are true, and 0 otherwise.  The
``condition`` argument must be a compile-time constant integer with
value not exceeding 127.  The
``condition`` argument is encoded as a bitmask with each bit
enabling the testing of a different condition, as characterized by the
following:

.. code-block:: c++

  0x40    Test for NaN
  0x20    Test for +Infinity
  0x10    Test for -Infinity
  0x08    Test for +Zero
  0x04    Test for -Zero
  0x02    Test for +Denormal
  0x01    Test for -Denormal

The ``scalar_test_neg`` built-in function returns 1 if its
``source`` argument holds a negative value, 0 otherwise.

The following built-in functions are also available for the PowerPC family
of processors, starting with ISA 3.0 or later
(:option:`-mcpu=power9`).  These string functions are described
separately in order to group the descriptions closer to the function
prototypes.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  int vec_all_nez (vector signed char, vector signed char);
  int vec_all_nez (vector unsigned char, vector unsigned char);
  int vec_all_nez (vector signed short, vector signed short);
  int vec_all_nez (vector unsigned short, vector unsigned short);
  int vec_all_nez (vector signed int, vector signed int);
  int vec_all_nez (vector unsigned int, vector unsigned int);

  int vec_any_eqz (vector signed char, vector signed char);
  int vec_any_eqz (vector unsigned char, vector unsigned char);
  int vec_any_eqz (vector signed short, vector signed short);
  int vec_any_eqz (vector unsigned short, vector unsigned short);
  int vec_any_eqz (vector signed int, vector signed int);
  int vec_any_eqz (vector unsigned int, vector unsigned int);

  signed char vec_xlx (unsigned int index, vector signed char data);
  unsigned char vec_xlx (unsigned int index, vector unsigned char data);
  signed short vec_xlx (unsigned int index, vector signed short data);
  unsigned short vec_xlx (unsigned int index, vector unsigned short data);
  signed int vec_xlx (unsigned int index, vector signed int data);
  unsigned int vec_xlx (unsigned int index, vector unsigned int data);
  float vec_xlx (unsigned int index, vector float data);

  signed char vec_xrx (unsigned int index, vector signed char data);
  unsigned char vec_xrx (unsigned int index, vector unsigned char data);
  signed short vec_xrx (unsigned int index, vector signed short data);
  unsigned short vec_xrx (unsigned int index, vector unsigned short data);
  signed int vec_xrx (unsigned int index, vector signed int data);
  unsigned int vec_xrx (unsigned int index, vector unsigned int data);
  float vec_xrx (unsigned int index, vector float data);

The ``vec_all_nez``, ``vec_any_eqz``, and ``vec_cmpnez``
perform pairwise comparisons between the elements at the same
positions within their two vector arguments.
The ``vec_all_nez`` function returns a
non-zero value if and only if all pairwise comparisons are not
equal and no element of either vector argument contains a zero.
The ``vec_any_eqz`` function returns a
non-zero value if and only if at least one pairwise comparison is equal
or if at least one element of either vector argument contains a zero.
The ``vec_cmpnez`` function returns a vector of the same type as
its two arguments, within which each element consists of all ones to
denote that either the corresponding elements of the incoming arguments are
not equal or that at least one of the corresponding elements contains
zero.  Otherwise, the element of the returned vector contains all zeros.

The ``vec_xlx`` and ``vec_xrx`` functions extract the single
element selected by the ``index`` argument from the vector
represented by the ``data`` argument.  The ``index`` argument
always specifies a byte offset, regardless of the size of the vector
element.  With ``vec_xlx``, ``index`` is the offset of the first
byte of the element to be extracted.  With ``vec_xrx``, ``index``
represents the last byte of the element to be extracted, measured
from the right end of the vector.  In other words, the last byte of
the element to be extracted is found at position ``(15 - index)``.
There is no requirement that ``index`` be a multiple of the vector
element size.  However, if the size of the vector element added to
``index`` is greater than 15, the content of the returned value is
undefined.

The following functions are also available if the ISA 3.0 instruction
set additions (:option:`-mcpu=power9`) are available.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector long long vec_vctz (vector long long);
  vector unsigned long long vec_vctz (vector unsigned long long);
  vector int vec_vctz (vector int);
  vector unsigned int vec_vctz (vector int);
  vector short vec_vctz (vector short);
  vector unsigned short vec_vctz (vector unsigned short);
  vector signed char vec_vctz (vector signed char);
  vector unsigned char vec_vctz (vector unsigned char);

  vector signed char vec_vctzb (vector signed char);
  vector unsigned char vec_vctzb (vector unsigned char);

  vector long long vec_vctzd (vector long long);
  vector unsigned long long vec_vctzd (vector unsigned long long);

  vector short vec_vctzh (vector short);
  vector unsigned short vec_vctzh (vector unsigned short);

  vector int vec_vctzw (vector int);
  vector unsigned int vec_vctzw (vector int);

  vector int vec_vprtyb (vector int);
  vector unsigned int vec_vprtyb (vector unsigned int);
  vector long long vec_vprtyb (vector long long);
  vector unsigned long long vec_vprtyb (vector unsigned long long);

  vector int vec_vprtybw (vector int);
  vector unsigned int vec_vprtybw (vector unsigned int);

  vector long long vec_vprtybd (vector long long);
  vector unsigned long long vec_vprtybd (vector unsigned long long);

On 64-bit targets, if the ISA 3.0 additions (:option:`-mcpu=power9`)
are available:

.. code-block:: c++

  vector long vec_vprtyb (vector long);
  vector unsigned long vec_vprtyb (vector unsigned long);
  vector __int128 vec_vprtyb (vector __int128);
  vector __uint128 vec_vprtyb (vector __uint128);

  vector long vec_vprtybd (vector long);
  vector unsigned long vec_vprtybd (vector unsigned long);

  vector __int128 vec_vprtybq (vector __int128);
  vector __uint128 vec_vprtybd (vector __uint128);

The following built-in functions are available for the PowerPC family
of processors, starting with ISA 3.0 or later (:option:`-mcpu=power9`).

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  __vector unsigned char
  vec_absdb (__vector unsigned char arg1, __vector unsigned char arg2);
  __vector unsigned short
  vec_absdh (__vector unsigned short arg1, __vector unsigned short arg2);
  __vector unsigned int
  vec_absdw (__vector unsigned int arg1, __vector unsigned int arg2);

The ``vec_absd``, ``vec_absdb``, ``vec_absdh``, and
``vec_absdw`` built-in functions each computes the absolute
differences of the pairs of vector elements supplied in its two vector
arguments, placing the absolute differences into the corresponding
elements of the vector result.

The following built-in functions are available for the PowerPC family
of processors, starting with ISA 3.0 or later (:option:`-mcpu=power9`):

.. code-block:: c++

  vector unsigned int vec_vrlnm (vector unsigned int, vector unsigned int);
  vector unsigned long long vec_vrlnm (vector unsigned long long,
                                       vector unsigned long long);

The result of ``vec_vrlnm`` is obtained by rotating each element
of the first argument vector left and ANDing it with a mask.  The
second argument vector contains the mask  beginning in bits 11:15,
the mask end in bits 19:23, and the shift count in bits 27:31,
of each element.

If the cryptographic instructions are enabled (:option:`-mcrypto` or
:option:`-mcpu=power8`), the following builtins are enabled.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector unsigned long long __builtin_crypto_vsbox (vector unsigned long long);

  vector unsigned long long __builtin_crypto_vcipher (vector unsigned long long,
                                                      vector unsigned long long);

  vector unsigned long long __builtin_crypto_vcipherlast
                                       (vector unsigned long long,
                                        vector unsigned long long);

  vector unsigned long long __builtin_crypto_vncipher (vector unsigned long long,
                                                       vector unsigned long long);

  vector unsigned long long __builtin_crypto_vncipherlast (vector unsigned long long,
                                                           vector unsigned long long);

  vector unsigned char __builtin_crypto_vpermxor (vector unsigned char,
                                                  vector unsigned char,
                                                  vector unsigned char);

  vector unsigned short __builtin_crypto_vpermxor (vector unsigned short,
                                                   vector unsigned short,
                                                   vector unsigned short);

  vector unsigned int __builtin_crypto_vpermxor (vector unsigned int,
                                                 vector unsigned int,
                                                 vector unsigned int);

  vector unsigned long long __builtin_crypto_vpermxor (vector unsigned long long,
                                                       vector unsigned long long,
                                                       vector unsigned long long);

  vector unsigned char __builtin_crypto_vpmsumb (vector unsigned char,
                                                 vector unsigned char);

  vector unsigned short __builtin_crypto_vpmsumh (vector unsigned short,
                                                  vector unsigned short);

  vector unsigned int __builtin_crypto_vpmsumw (vector unsigned int,
                                                vector unsigned int);

  vector unsigned long long __builtin_crypto_vpmsumd (vector unsigned long long,
                                                      vector unsigned long long);

  vector unsigned long long __builtin_crypto_vshasigmad (vector unsigned long long,
                                                         int, int);

  vector unsigned int __builtin_crypto_vshasigmaw (vector unsigned int, int, int);

The second argument to :samp:`{__builtin_crypto_vshasigmad}` and
:samp:`{__builtin_crypto_vshasigmaw}` must be a constant
integer that is 0 or 1.  The third argument to these built-in functions
must be a constant integer in the range of 0 to 15.

The following sign extension builtins are provided:

.. code-block:: c++

  vector signed int vec_signexti (vector signed char a);
  vector signed long long vec_signextll (vector signed char a);
  vector signed int vec_signexti (vector signed short a);
  vector signed long long vec_signextll (vector signed short a);
  vector signed long long vec_signextll (vector signed int a);
  vector signed long long vec_signextq (vector signed long long a);

Each element of the result is produced by sign-extending the element of the
input vector that would fall in the least significant portion of the result
element. For example, a sign-extension of a vector signed char to a vector
signed long long will sign extend the rightmost byte of each doubleword.

.. _powerpc-altivec-built-in-functions-available-on-isa-3.1:

PowerPC AltiVec Built-in Functions Available on ISA 3.1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.1 (:option:`-mcpu=power10`):

.. code-block:: c++

  vector unsigned long long int
  vec_cfuge (vector unsigned long long int, vector unsigned long long int);

Perform a vector centrifuge operation, as if implemented by the
``vcfuged`` instruction.

.. index:: vec_cfuge

.. code-block:: c++

  vector unsigned long long int
  vec_cntlzm (vector unsigned long long int, vector unsigned long long int);

Perform a vector count leading zeros under bit mask operation, as if
implemented by the ``vclzdm`` instruction.

.. index:: vec_cntlzm

.. code-block:: c++

  vector unsigned long long int
  vec_cnttzm (vector unsigned long long int, vector unsigned long long int);

Perform a vector count trailing zeros under bit mask operation, as if
implemented by the ``vctzdm`` instruction.

.. index:: vec_cnttzm

.. code-block:: c++

  vector signed char
  vec_clrl (vector signed char a, unsigned int n);
  vector unsigned char
  vec_clrl (vector unsigned char a, unsigned int n);

Clear the left-most ``(16 - n)`` bytes of vector argument ``a``, as if
implemented by the ``vclrlb`` instruction on a big-endian target
and by the ``vclrrb`` instruction on a little-endian target.  A
value of ``n`` that is greater than 16 is treated as if it equaled 16.

.. index:: vec_clrl

.. code-block:: c++

  vector signed char
  vec_clrr (vector signed char a, unsigned int n);
  vector unsigned char
  vec_clrr (vector unsigned char a, unsigned int n);

Clear the right-most ``(16 - n)`` bytes of vector argument ``a``, as if
implemented by the ``vclrrb`` instruction on a big-endian target
and by the ``vclrlb`` instruction on a little-endian target.  A
value of ``n`` that is greater than 16 is treated as if it equaled 16.

.. index:: vec_clrr

.. code-block:: c++

  vector unsigned long long int
  vec_gnb (vector unsigned __int128, const unsigned char);

Perform a 128-bit vector gather  operation, as if implemented by the
``vgnb`` instruction.  The second argument must be a literal
integer value between 2 and 7 inclusive.

.. index:: vec_gnb

Vector Extract

.. code-block:: c++

  vector unsigned long long int
  vec_extractl (vector unsigned char, vector unsigned char, unsigned int);
  vector unsigned long long int
  vec_extractl (vector unsigned short, vector unsigned short, unsigned int);
  vector unsigned long long int
  vec_extractl (vector unsigned int, vector unsigned int, unsigned int);
  vector unsigned long long int
  vec_extractl (vector unsigned long long, vector unsigned long long, unsigned int);

Extract an element from two concatenated vectors starting at the given byte index
in natural-endian order, and place it zero-extended in doubleword 1 of the result
according to natural element order.  If the byte index is out of range for the
data type, the intrinsic will be rejected.
For little-endian, this output will match the placement by the hardware
instruction, i.e., dword[0] in RTL notation.  For big-endian, an additional
instruction is needed to move it from the "left" doubleword to the  "right" one.
For little-endian, semantics matching the ``vextdubvrx``,
``vextduhvrx``, ``vextduwvrx`` instruction will be generated, while for
big-endian, semantics matching the ``vextdubvlx``, ``vextduhvlx``,
``vextduwvlx`` instructions
will be generated.  Note that some fairly anomalous results can be generated if
the byte index is not aligned on an element boundary for the element being
extracted.  This is a limitation of the bi-endian vector programming model is
consistent with the limitation on ``vec_perm``.

.. index:: vec_extractl

.. code-block:: c++

  vector unsigned long long intvec_extracth (vector unsigned char, vector unsigned char, unsigned int);
  vector unsigned long long intvec_extracth (vector unsigned short, vector unsigned short,unsigned int);
  vector unsigned long long intvec_extracth (vector unsigned int, vector unsigned int, unsigned int);
  vector unsigned long long intvec_extracth (vector unsigned long long, vector unsigned long long,unsigned int);

Extract an element from two concatenated vectors starting at the given byte
index.  The index is based on big endian order for a little endian system.
Similarly, the index is based on little endian order for a big endian system.
The extraced elements are zero-extended and put in doubleword 1
according to natural element order.  If the byte index is out of range for the
data type, the intrinsic will be rejected.  For little-endian, this output
will match the placement by the hardware instruction (vextdubvrx, vextduhvrx,
vextduwvrx, vextddvrx) i.e., dword[0] in RTL
notation.  For big-endian, an additional instruction is needed to move it
from the "left" doubleword to the "right" one.  For little-endian, semantics
matching the ``vextdubvlx``, ``vextduhvlx``, ``vextduwvlx``
instructions will be generated, while for big-endian, semantics matching the
``vextdubvrx``, ``vextduhvrx``, ``vextduwvrx`` instructions will
be generated.  Note that some fairly anomalous
results can be generated if the byte index is not aligned on the
element boundary for the element being extracted.  This is a
limitation of the bi-endian vector programming model consistent with the
limitation on ``vec_perm``.

.. index:: vec_extracth

.. code-block:: c++

  vector unsigned long long int
  vec_pdep (vector unsigned long long int, vector unsigned long long int);

Perform a vector parallel bits deposit operation, as if implemented by
the ``vpdepd`` instruction.

.. index:: vec_pdep

Vector Insert

.. code-block:: c++

  vector unsigned charvec_insertl (unsigned char, vector unsigned char, unsigned int);
  vector unsigned shortvec_insertl (unsigned short, vector unsigned short, unsigned int);
  vector unsigned intvec_insertl (unsigned int, vector unsigned int, unsigned int);
  vector unsigned long longvec_insertl (unsigned long long, vector unsigned long long,unsigned int);
  vector unsigned charvec_insertl (vector unsigned char, vector unsigned char, unsigned int;
  vector unsigned shortvec_insertl (vector unsigned short, vector unsigned short,unsigned int);
  vector unsigned intvec_insertl (vector unsigned int, vector unsigned int, unsigned int);

Let src be the first argument, when the first argument is a scalar, or the
rightmost element of the left doubleword of the first argument, when the first
argument is a vector.  Insert the source into the destination at the position
given by the third argument, using natural element order in the second
argument.  The rest of the second argument is unchanged.  If the byte
index is greater than 14 for halfwords, greater than 12 for words, or
greater than 8 for doublewords the result is undefined.   For little-endian,
the generated code will be semantically equivalent to ``vins[bhwd]rx``
instructions.  Similarly for big-endian it will be semantically equivalent
to ``vins[bhwd]lx``.  Note that some fairly anomalous results can be
generated if the byte index is not aligned on an element boundary for the
type of element being inserted.

.. index:: vec_insertl

.. code-block:: c++

  vector unsigned charvec_inserth (unsigned char, vector unsigned char, unsigned int);
  vector unsigned shortvec_inserth (unsigned short, vector unsigned short, unsigned int);
  vector unsigned intvec_inserth (unsigned int, vector unsigned int, unsigned int);
  vector unsigned long longvec_inserth (unsigned long long, vector unsigned long long,unsigned int);
  vector unsigned charvec_inserth (vector unsigned char, vector unsigned char, unsigned int);
  vector unsigned shortvec_inserth (vector unsigned short, vector unsigned short,unsigned int);
  vector unsigned intvec_inserth (vector unsigned int, vector unsigned int, unsigned int);

Let src be the first argument, when the first argument is a scalar, or the
rightmost element of the first argument, when the first argument is a vector.
Insert src into the second argument at the position identified by the third
argument, using opposite element order in the second argument, and leaving the
rest of the second argument unchanged.  If the byte index is greater than 14
for halfwords, 12 for words, or 8 for doublewords, the intrinsic will be
rejected. Note that the underlying hardware instruction uses the same register
for the second argument and the result.
For little-endian, the code generation will be semantically equivalent to
``vins[bhwd]lx``, while for big-endian it will be semantically equivalent to
``vins[bhwd]rx``.
Note that some fairly anomalous results can be generated if the byte index is
not aligned on an element boundary for the sort of element being inserted.

.. index:: vec_inserth

Vector Replace Element

.. code-block:: c++

  vector signed int vec_replace_elt (vector signed int, signed int,const int);
  vector unsigned int vec_replace_elt (vector unsigned int,unsigned int, const int);
  vector float vec_replace_elt (vector float, float, const int);
  vector signed long long vec_replace_elt (vector signed long long,signed long long, const int);
  vector unsigned long long vec_replace_elt (vector unsigned long long,unsigned long long, const int);
  vector double rec_replace_elt (vector double, double, const int);

The third argument (constrained to [0,3]) identifies the natural-endian
element number of the first argument that will be replaced by the second
argument to produce the result.  The other elements of the first argument will
remain unchanged in the result.

If it's desirable to insert a word at an unaligned position, use
vec_replace_unaligned instead.

.. index:: vec_replace_element

Vector Replace Unaligned

.. code-block:: c++

  vector unsigned char vec_replace_unaligned (vector unsigned char,signed int, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,unsigned int, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,float, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,signed long long, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,unsigned long long, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,double, const int);

The second argument replaces a portion of the first argument to produce the
result, with the rest of the first argument unchanged in the result.  The
third argument identifies the byte index (using left-to-right, or big-endian
order) where the high-order byte of the second argument will be placed, with
the remaining bytes of the second argument placed naturally "to the right"
of the high-order byte.

The programmer is responsible for understanding the endianness issues involved
with the first argument and the result.

.. index:: vec_replace_unaligned

Vector Shift Left Double Bit Immediate

.. code-block:: c++

  vector signed char vec_sldb (vector signed char, vector signed char,const unsigned int);
  vector unsigned char vec_sldb (vector unsigned char,vector unsigned char, const unsigned int);
  vector signed short vec_sldb (vector signed short, vector signed short,const unsigned int);
  vector unsigned short vec_sldb (vector unsigned short,vector unsigned short, const unsigned int);
  vector signed int vec_sldb (vector signed int, vector signed int,const unsigned int);
  vector unsigned int vec_sldb (vector unsigned int, vector unsigned int,const unsigned int);
  vector signed long long vec_sldb (vector signed long long,vector signed long long, const unsigned int);
  vector unsigned long long vec_sldb (vector unsigned long long,vector unsigned long long, const unsigned int);

Shift the combined input vectors left by the amount specified by the low-order
three bits of the third argument, and return the leftmost remaining 128 bits.
Code using this instruction must be endian-aware.

.. index:: vec_sldb

Vector Shift Right Double Bit Immediate

.. code-block:: c++

  vector signed char vec_srdb (vector signed char, vector signed char,const unsigned int);
  vector unsigned char vec_srdb (vector unsigned char, vector unsigned char,const unsigned int);
  vector signed short vec_srdb (vector signed short, vector signed short,const unsigned int);
  vector unsigned short vec_srdb (vector unsigned short, vector unsigned short,const unsigned int);
  vector signed int vec_srdb (vector signed int, vector signed int,const unsigned int);
  vector unsigned int vec_srdb (vector unsigned int, vector unsigned int,const unsigned int);
  vector signed long long vec_srdb (vector signed long long,vector signed long long, const unsigned int);
  vector unsigned long long vec_srdb (vector unsigned long long,vector unsigned long long, const unsigned int);

Shift the combined input vectors right by the amount specified by the low-order
three bits of the third argument, and return the remaining 128 bits.  Code
using this built-in must be endian-aware.

.. index:: vec_srdb

Vector Splat

.. code-block:: c++

  vector signed int vec_splati (const signed int);
  vector float vec_splati (const float);

Splat a 32-bit immediate into a vector of words.

.. index:: vec_splati

.. code-block:: c++

  vector double vec_splatid (const float);

Convert a single precision floating-point value to double-precision and splat
the result to a vector of double-precision floats.

.. index:: vec_splatid

.. code-block:: c++

  vector signed int vec_splati_ins (vector signed int,const unsigned int, const signed int);
  vector unsigned int vec_splati_ins (vector unsigned int,const unsigned int, const unsigned int);
  vector float vec_splati_ins (vector float, const unsigned int,const float);

Argument 2 must be either 0 or 1.  Splat the value of argument 3 into the word
identified by argument 2 of each doubleword of argument 1 and return the
result.  The other words of argument 1 are unchanged.

.. index:: vec_splati_ins

Vector Blend Variable

.. code-block:: c++

  vector signed char vec_blendv (vector signed char, vector signed char,vector unsigned char);
  vector unsigned char vec_blendv (vector unsigned char,vector unsigned char, vector unsigned char);
  vector signed short vec_blendv (vector signed short,vector signed short, vector unsigned short);
  vector unsigned short vec_blendv (vector unsigned short,vector unsigned short, vector unsigned short);
  vector signed int vec_blendv (vector signed int, vector signed int,vector unsigned int);
  vector unsigned int vec_blendv (vector unsigned int,vector unsigned int, vector unsigned int);
  vector signed long long vec_blendv (vector signed long long,vector signed long long, vector unsigned long long);
  vector unsigned long long vec_blendv (vector unsigned long long,vector unsigned long long, vector unsigned long long);
  vector float vec_blendv (vector float, vector float,vector unsigned int);
  vector double vec_blendv (vector double, vector double,vector unsigned long long);

Blend the first and second argument vectors according to the sign bits of the
corresponding elements of the third argument vector.  This is similar to the
``vsel`` and ``xxsel`` instructions but for bigger elements.

.. index:: vec_blendv

Vector Permute Extended

.. code-block:: c++

  vector signed char vec_permx (vector signed char, vector signed char,vector unsigned char, const int);
  vector unsigned char vec_permx (vector unsigned char,vector unsigned char, vector unsigned char, const int);
  vector signed short vec_permx (vector signed short,vector signed short, vector unsigned char, const int);
  vector unsigned short vec_permx (vector unsigned short,vector unsigned short, vector unsigned char, const int);
  vector signed int vec_permx (vector signed int, vector signed int,vector unsigned char, const int);
  vector unsigned int vec_permx (vector unsigned int,vector unsigned int, vector unsigned char, const int);
  vector signed long long vec_permx (vector signed long long,vector signed long long, vector unsigned char, const int);
  vector unsigned long long vec_permx (vector unsigned long long,vector unsigned long long, vector unsigned char, const int);
  vector float (vector float, vector float, vector unsigned char,const int);
  vector double (vector double, vector double, vector unsigned char,const int);

Perform a partial permute of the first two arguments, which form a 32-byte
section of an emulated vector up to 256 bytes wide, using the partial permute
control vector in the third argument.  The fourth argument (constrained to
values of 0-7) identifies which 32-byte section of the emulated vector is
contained in the first two arguments.

.. index:: vec_permx

.. code-block:: c++

  vector unsigned long long int
  vec_pext (vector unsigned long long int, vector unsigned long long int);

Perform a vector parallel bit extract operation, as if implemented by
the ``vpextd`` instruction.

.. index:: vec_pext

.. code-block:: c++

  vector unsigned char vec_stril (vector unsigned char);
  vector signed char vec_stril (vector signed char);
  vector unsigned short vec_stril (vector unsigned short);
  vector signed short vec_stril (vector signed short);

Isolate the left-most non-zero elements of the incoming vector argument,
replacing all elements to the right of the left-most zero element
found within the argument with zero.  The typical implementation uses
the ``vstribl`` or ``vstrihl`` instruction on big-endian targets
and uses the ``vstribr`` or ``vstrihr`` instruction on
little-endian targets.

.. index:: vec_stril

.. code-block:: c++

  int vec_stril_p (vector unsigned char);
  int vec_stril_p (vector signed char);
  int short vec_stril_p (vector unsigned short);
  int vec_stril_p (vector signed short);

Return a non-zero value if and only if the argument contains a zero
element.  The typical implementation uses
the ``vstribl.`` or ``vstrihl.`` instruction on big-endian targets
and uses the ``vstribr.`` or ``vstrihr.`` instruction on
little-endian targets.  Choose this built-in to check for presence of
zero element if the same argument is also passed to ``vec_stril``.

.. index:: vec_stril_p

.. code-block:: c++

  vector unsigned char vec_strir (vector unsigned char);
  vector signed char vec_strir (vector signed char);
  vector unsigned short vec_strir (vector unsigned short);
  vector signed short vec_strir (vector signed short);

Isolate the right-most non-zero elements of the incoming vector argument,
replacing all elements to the left of the right-most zero element
found within the argument with zero.  The typical implementation uses
the ``vstribr`` or ``vstrihr`` instruction on big-endian targets
and uses the ``vstribl`` or ``vstrihl`` instruction on
little-endian targets.

.. index:: vec_strir

.. code-block:: c++

  int vec_strir_p (vector unsigned char);
  int vec_strir_p (vector signed char);
  int short vec_strir_p (vector unsigned short);
  int vec_strir_p (vector signed short);

Return a non-zero value if and only if the argument contains a zero
element.  The typical implementation uses
the ``vstribr.`` or ``vstrihr.`` instruction on big-endian targets
and uses the ``vstribl.`` or ``vstrihl.`` instruction on
little-endian targets.  Choose this built-in to check for presence of
zero element if the same argument is also passed to ``vec_strir``.

.. index:: vec_strir_p

.. code-block:: c++

  vector unsigned charvec_ternarylogic (vector unsigned char, vector unsigned char, vector unsigned char, const unsigned int);
  vector unsigned shortvec_ternarylogic (vector unsigned short, vector unsigned short, vector unsigned short, const unsigned int);
  vector unsigned intvec_ternarylogic (vector unsigned int, vector unsigned int, vector unsigned int, const unsigned int);
  vector unsigned long long intvec_ternarylogic (vector unsigned long long int, vector unsigned long long int, vector unsigned long long int, const unsigned int);
  vector unsigned __int128vec_ternarylogic (vector unsigned __int128, vector unsigned __int128, vector unsigned __int128, const unsigned int);

Perform a 128-bit vector evaluate operation, as if implemented by the
``xxeval`` instruction.  The fourth argument must be a literal
integer value between 0 and 255 inclusive.

.. index:: vec_ternarylogic

.. code-block:: c++

  vector unsigned char vec_genpcvm (vector unsigned char, const int);
  vector unsigned short vec_genpcvm (vector unsigned short, const int);
  vector unsigned int vec_genpcvm (vector unsigned int, const int);
  vector unsigned int vec_genpcvm (vector unsigned long long int, const int);

Vector Integer Multiply/Divide/Modulo

.. code-block:: c++

  vector signed int
  vec_mulh (vector signed int a, vector signed int b);
  vector unsigned int
  vec_mulh (vector unsigned int a, vector unsigned int b);

For each integer value ``i`` from 0 to 3, do the following. The integer
value in word element ``i`` of a is multiplied by the integer value in word
element ``i`` of b. The high-order 32 bits of the 64-bit product are placed
into word element ``i`` of the vector returned.

.. code-block:: c++

  vector signed long long
  vec_mulh (vector signed long long a, vector signed long long b);
  vector unsigned long long
  vec_mulh (vector unsigned long long a, vector unsigned long long b);

For each integer value ``i`` from 0 to 1, do the following. The integer
value in doubleword element ``i`` of a is multiplied by the integer value in
doubleword element ``i`` of b. The high-order 64 bits of the 128-bit product
are placed into doubleword element ``i`` of the vector returned.

.. code-block:: c++

  vector unsigned long long
  vec_mul (vector unsigned long long a, vector unsigned long long b);
  vector signed long long
  vec_mul (vector signed long long a, vector signed long long b);

For each integer value ``i`` from 0 to 1, do the following. The integer
value in doubleword element ``i`` of a is multiplied by the integer value in
doubleword element ``i`` of b. The low-order 64 bits of the 128-bit product
are placed into doubleword element ``i`` of the vector returned.

.. code-block:: c++

  vector signed int
  vec_div (vector signed int a, vector signed int b);
  vector unsigned int
  vec_div (vector unsigned int a, vector unsigned int b);

For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is divided by the integer in word element ``i``
of b. The unique integer quotient is placed into the word element ``i`` of
the vector returned. If an attempt is made to perform any of the divisions
<anything> ÷ 0 then the quotient is undefined.

.. code-block:: c++

  vector signed long long
  vec_div (vector signed long long a, vector signed long long b);
  vector unsigned long long
  vec_div (vector unsigned long long a, vector unsigned long long b);

For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is divided by the integer in doubleword
element ``i`` of b. The unique integer quotient is placed into the
doubleword element ``i`` of the vector returned. If an attempt is made to
perform any of the divisions 0x8000_0000_0000_0000 ÷ -1 or <anything> ÷ 0 then
the quotient is undefined.

.. code-block:: c++

  vector signed int
  vec_dive (vector signed int a, vector signed int b);
  vector unsigned int
  vec_dive (vector unsigned int a, vector unsigned int b);

For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is shifted left by 32 bits, then divided by the
integer in word element ``i`` of b. The unique integer quotient is placed
into the word element ``i`` of the vector returned. If the quotient cannot
be represented in 32 bits, or if an attempt is made to perform any of the
divisions <anything> ÷ 0 then the quotient is undefined.

.. code-block:: c++

  vector signed long long
  vec_dive (vector signed long long a, vector signed long long b);
  vector unsigned long long
  vec_dive (vector unsigned long long a, vector unsigned long long b);

For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is shifted left by 64 bits, then divided by
the integer in doubleword element ``i`` of b. The unique integer quotient is
placed into the doubleword element ``i`` of the vector returned. If the
quotient cannot be represented in 64 bits, or if an attempt is made to perform
<anything> ÷ 0 then the quotient is undefined.

.. code-block:: c++

  vector signed int
  vec_mod (vector signed int a, vector signed int b);
  vector unsigned int
  vec_mod (vector unsigned int a, vector unsigned int b);

For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is divided by the integer in word element ``i``
of b. The unique integer remainder is placed into the word element ``i`` of
the vector returned.  If an attempt is made to perform any of the divisions
0x8000_0000 ÷ -1 or <anything> ÷ 0 then the remainder is undefined.

.. code-block:: c++

  vector signed long long
  vec_mod (vector signed long long a, vector signed long long b);
  vector unsigned long long
  vec_mod (vector unsigned long long a, vector unsigned long long b);

For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is divided by the integer in doubleword
element ``i`` of b. The unique integer remainder is placed into the
doubleword element ``i`` of the vector returned. If an attempt is made to
perform <anything> ÷ 0 then the remainder is undefined.

Generate PCV from specified Mask size, as if implemented by the
``xxgenpcvbm``, ``xxgenpcvhm``, ``xxgenpcvwm`` instructions, where
immediate value is either 0, 1, 2 or 3.

.. index:: vec_genpcvm

.. code-block:: c++

  vector unsigned __int128 vec_rl (vector unsigned __int128 A, vector unsigned __int128 B);
  vector signed __int128 vec_rl (vector signed __int128 A, vector unsigned __int128 B);

Result value: Each element of R is obtained by rotating the corresponding element
of A left by the number of bits specified by the corresponding element of B.

.. code-block:: c++

  vector unsigned __int128 vec_rlmi (vector unsigned __int128, vector unsigned __int128, vector unsigned __int128);
  vector signed __int128 vec_rlmi (vector signed __int128, vector signed __int128, vector unsigned __int128);

Returns the result of rotating the first input and inserting it under mask
into the second input.  The first bit in the mask, the last bit in the mask are
obtained from the two 7-bit fields bits [108:115] and bits [117:123]
respectively of the second input.  The shift is obtained from the third input
in the 7-bit field [125:131] where all bits counted from zero at the left.

.. code-block:: c++

  vector unsigned __int128 vec_rlnm (vector unsigned __int128, vector unsigned __int128, vector unsigned __int128);
  vector signed __int128 vec_rlnm (vector signed __int128, vector unsigned __int128, vector unsigned __int128);

Returns the result of rotating the first input and ANDing it with a mask.  The
first bit in the mask and the last bit in the mask are obtained from the two
7-bit fields bits [117:123] and bits [125:131] respectively of the second
input.  The shift is obtained from the third input in the 7-bit field bits
[125:131] where all bits counted from zero at the left.

.. code-block:: c++

  vector unsigned __int128 vec_sl(vector unsigned __int128 A, vector unsigned __int128 B);
  vector signed __int128 vec_sl(vector signed __int128 A, vector unsigned __int128 B);

Result value: Each element of R is obtained by shifting the corresponding element of
A left by the number of bits specified by the corresponding element of B.

.. code-block:: c++

  vector unsigned __int128 vec_sr(vector unsigned __int128 A, vector unsigned __int128 B);
  vector signed __int128 vec_sr(vector signed __int128 A, vector unsigned __int128 B);

Result value: Each element of R is obtained by shifting the corresponding element of
A right by the number of bits specified by the corresponding element of B.

.. code-block:: c++

  vector unsigned __int128 vec_sra(vector unsigned __int128 A, vector unsigned __int128 B);
  vector signed __int128 vec_sra(vector signed __int128 A, vector unsigned __int128 B);

Result value: Each element of R is obtained by arithmetic shifting the corresponding
element of A right by the number of bits specified by the corresponding element of B.

.. code-block:: c++

  vector unsigned __int128 vec_mule (vector unsigned long long, vector unsigned long long);
  vector signed __int128 vec_mule (vector signed long long, vector signed long long);

Returns a vector containing a 128-bit integer result of multiplying the even
doubleword elements of the two inputs.

.. code-block:: c++

  vector unsigned __int128 vec_mulo (vector unsigned long long, vector unsigned long long);
  vector signed __int128 vec_mulo (vector signed long long, vector signed long long);

Returns a vector containing a 128-bit integer result of multiplying the odd
doubleword elements of the two inputs.

.. code-block:: c++

  vector unsigned __int128 vec_div (vector unsigned __int128, vector unsigned __int128);
  vector signed __int128 vec_div (vector signed __int128, vector signed __int128);

Returns the result of dividing the first operand by the second operand. An
attempt to divide any value by zero or to divide the most negative signed
128-bit integer by negative one results in an undefined value.

.. code-block:: c++

  vector unsigned __int128 vec_dive (vector unsigned __int128, vector unsigned __int128);
  vector signed __int128 vec_dive (vector signed __int128, vector signed __int128);

The result is produced by shifting the first input left by 128 bits and
dividing by the second.  If an attempt is made to divide by zero or the result
is larger than 128 bits, the result is undefined.

.. code-block:: c++

  vector unsigned __int128 vec_mod (vector unsigned __int128, vector unsigned __int128);
  vector signed __int128 vec_mod (vector signed __int128, vector signed __int128);

The result is the modulo result of dividing the first input  by the second
input.

The following builtins perform 128-bit vector comparisons.  The
``vec_all_xx``, ``vec_any_xx``, and ``vec_cmpxx``, where ``xx`` is
one of the operations ``eq, ne, gt, lt, ge, le`` perform pairwise
comparisons between the elements at the same positions within their two vector
arguments.  The ``vec_all_xx`` function returns a non-zero value if and only
if all pairwise comparisons are true.  The ``vec_any_xx`` function returns
a non-zero value if and only if at least one pairwise comparison is true.  The
``vec_cmpxx`` function returns a vector of the same type as its two
arguments, within which each element consists of all ones to denote that
specified logical comparison of the corresponding elements was true.
Otherwise, the element of the returned vector contains all zeros.

.. code-block:: c++

  vector bool __int128 vec_cmpeq (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmpeq (vector unsigned __int128, vector unsigned __int128);
  vector bool __int128 vec_cmpne (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmpne (vector unsigned __int128, vector unsigned __int128);
  vector bool __int128 vec_cmpgt (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmpgt (vector unsigned __int128, vector unsigned __int128);
  vector bool __int128 vec_cmplt (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmplt (vector unsigned __int128, vector unsigned __int128);
  vector bool __int128 vec_cmpge (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmpge (vector unsigned __int128, vector unsigned __int128);
  vector bool __int128 vec_cmple (vector signed __int128, vector signed __int128);
  vector bool __int128 vec_cmple (vector unsigned __int128, vector unsigned __int128);

  int vec_all_eq (vector signed __int128, vector signed __int128);
  int vec_all_eq (vector unsigned __int128, vector unsigned __int128);
  int vec_all_ne (vector signed __int128, vector signed __int128);
  int vec_all_ne (vector unsigned __int128, vector unsigned __int128);
  int vec_all_gt (vector signed __int128, vector signed __int128);
  int vec_all_gt (vector unsigned __int128, vector unsigned __int128);
  int vec_all_lt (vector signed __int128, vector signed __int128);
  int vec_all_lt (vector unsigned __int128, vector unsigned __int128);
  int vec_all_ge (vector signed __int128, vector signed __int128);
  int vec_all_ge (vector unsigned __int128, vector unsigned __int128);
  int vec_all_le (vector signed __int128, vector signed __int128);
  int vec_all_le (vector unsigned __int128, vector unsigned __int128);

  int vec_any_eq (vector signed __int128, vector signed __int128);
  int vec_any_eq (vector unsigned __int128, vector unsigned __int128);
  int vec_any_ne (vector signed __int128, vector signed __int128);
  int vec_any_ne (vector unsigned __int128, vector unsigned __int128);
  int vec_any_gt (vector signed __int128, vector signed __int128);
  int vec_any_gt (vector unsigned __int128, vector unsigned __int128);
  int vec_any_lt (vector signed __int128, vector signed __int128);
  int vec_any_lt (vector unsigned __int128, vector unsigned __int128);
  int vec_any_ge (vector signed __int128, vector signed __int128);
  int vec_any_ge (vector unsigned __int128, vector unsigned __int128);
  int vec_any_le (vector signed __int128, vector signed __int128);
  int vec_any_le (vector unsigned __int128, vector unsigned __int128);