..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _mips-dsp-built-in-functions:

MIPS DSP Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The MIPS DSP Application-Specific Extension (ASE) includes new
instructions that are designed to improve the performance of DSP and
media applications.  It provides instructions that operate on packed
8-bit/16-bit integer data, Q7, Q15 and Q31 fractional data.

GCC supports MIPS DSP operations using both the generic
vector extensions (see :ref:`vector-extensions`) and a collection of
MIPS-specific built-in functions.  Both kinds of support are
enabled by the :option:`-mdsp` command-line option.

Revision 2 of the ASE was introduced in the second half of 2006.
This revision adds extra instructions to the original ASE, but is
otherwise backwards-compatible with it.  You can select revision 2
using the command-line option :option:`-mdspr2` ; this option implies
:option:`-mdsp`.

The SCOUNT and POS bits of the DSP control register are global.  The
WRDSP, EXTPDP, EXTPDPV and MTHLIP instructions modify the SCOUNT and
POS bits.  During optimization, the compiler does not delete these
instructions and it does not delete calls to functions containing
these instructions.

At present, GCC only provides support for operations on 32-bit
vectors.  The vector type associated with 8-bit integer data is
usually called ``v4i8``, the vector type associated with Q7
is usually called ``v4q7``, the vector type associated with 16-bit
integer data is usually called ``v2i16``, and the vector type
associated with Q15 is usually called ``v2q15``.  They can be
defined in C as follows:

.. code-block:: c++

  typedef signed char v4i8 __attribute__ ((vector_size(4)));
  typedef signed char v4q7 __attribute__ ((vector_size(4)));
  typedef short v2i16 __attribute__ ((vector_size(4)));
  typedef short v2q15 __attribute__ ((vector_size(4)));

``v4i8``, ``v4q7``, ``v2i16`` and ``v2q15`` values are
initialized in the same way as aggregates.  For example:

.. code-block:: c++

  v4i8 a = {1, 2, 3, 4};
  v4i8 b;
  b = (v4i8) {5, 6, 7, 8};

  v2q15 c = {0x0fcb, 0x3a75};
  v2q15 d;
  d = (v2q15) {0.1234 * 0x1.0p15, 0.4567 * 0x1.0p15};

.. note::

  The CPU's endianness determines the order in which values
  are packed.  On little-endian targets, the first value is the least
  significant and the last value is the most significant.  The opposite
  order applies to big-endian targets.  For example, the code above
  sets the lowest byte of ``a`` to ``1`` on little-endian targets
  and ``4`` on big-endian targets.

.. note::

  Q7, Q15 and Q31 values must be initialized with their integer
  representation.  As shown in this example, the integer representation
  of a Q7 value can be obtained by multiplying the fractional value by
  ``0x1.0p7``.  The equivalent for Q15 values is to multiply by
  ``0x1.0p15``.  The equivalent for Q31 values is to multiply by
  ``0x1.0p31``.

The table below lists the ``v4i8`` and ``v2q15`` operations for which
hardware support exists.  ``a`` and ``b`` are ``v4i8`` values,
and ``c`` and ``d`` are ``v2q15`` values.

.. list-table::
   :header-rows: 1

   * - C code
     - MIPS instruction

   * - ``a + b``
     - ``addu.qb``
   * - ``c + d``
     - ``addq.ph``
   * - ``a - b``
     - ``subu.qb``
   * - ``c - d``
     - ``subq.ph``

The table below lists the ``v2i16`` operation for which
hardware support exists for the DSP ASE REV 2.  ``e`` and ``f`` are
``v2i16`` values.

.. list-table::
   :header-rows: 1

   * - C code
     - MIPS instruction

   * - ``e * f``
     - ``mul.ph``

It is easier to describe the DSP built-in functions if we first define
the following types:

.. code-block:: c++

  typedef int q31;
  typedef int i32;
  typedef unsigned int ui32;
  typedef long long a64;

``q31`` and ``i32`` are actually the same as ``int``, but we
use ``q31`` to indicate a Q31 fractional value and ``i32`` to
indicate a 32-bit integer value.  Similarly, ``a64`` is the same as
``long long``, but we use ``a64`` to indicate values that are
placed in one of the four DSP accumulators (``$ac0``,
``$ac1``, ``$ac2`` or ``$ac3``).

Also, some built-in functions prefer or require immediate numbers as
parameters, because the corresponding DSP instructions accept both immediate
numbers and register operands, or accept immediate numbers only.  The
immediate parameters are listed as follows.

.. code-block:: c++

  imm0_3: 0 to 3.
  imm0_7: 0 to 7.
  imm0_15: 0 to 15.
  imm0_31: 0 to 31.
  imm0_63: 0 to 63.
  imm0_255: 0 to 255.
  imm_n32_31: -32 to 31.
  imm_n512_511: -512 to 511.

The following built-in functions map directly to a particular MIPS DSP
instruction.  Please refer to the architecture specification
for details on what each instruction does.

.. code-block:: c++

  v2q15 __builtin_mips_addq_ph (v2q15, v2q15);
  v2q15 __builtin_mips_addq_s_ph (v2q15, v2q15);
  q31 __builtin_mips_addq_s_w (q31, q31);
  v4i8 __builtin_mips_addu_qb (v4i8, v4i8);
  v4i8 __builtin_mips_addu_s_qb (v4i8, v4i8);
  v2q15 __builtin_mips_subq_ph (v2q15, v2q15);
  v2q15 __builtin_mips_subq_s_ph (v2q15, v2q15);
  q31 __builtin_mips_subq_s_w (q31, q31);
  v4i8 __builtin_mips_subu_qb (v4i8, v4i8);
  v4i8 __builtin_mips_subu_s_qb (v4i8, v4i8);
  i32 __builtin_mips_addsc (i32, i32);
  i32 __builtin_mips_addwc (i32, i32);
  i32 __builtin_mips_modsub (i32, i32);
  i32 __builtin_mips_raddu_w_qb (v4i8);
  v2q15 __builtin_mips_absq_s_ph (v2q15);
  q31 __builtin_mips_absq_s_w (q31);
  v4i8 __builtin_mips_precrq_qb_ph (v2q15, v2q15);
  v2q15 __builtin_mips_precrq_ph_w (q31, q31);
  v2q15 __builtin_mips_precrq_rs_ph_w (q31, q31);
  v4i8 __builtin_mips_precrqu_s_qb_ph (v2q15, v2q15);
  q31 __builtin_mips_preceq_w_phl (v2q15);
  q31 __builtin_mips_preceq_w_phr (v2q15);
  v2q15 __builtin_mips_precequ_ph_qbl (v4i8);
  v2q15 __builtin_mips_precequ_ph_qbr (v4i8);
  v2q15 __builtin_mips_precequ_ph_qbla (v4i8);
  v2q15 __builtin_mips_precequ_ph_qbra (v4i8);
  v2q15 __builtin_mips_preceu_ph_qbl (v4i8);
  v2q15 __builtin_mips_preceu_ph_qbr (v4i8);
  v2q15 __builtin_mips_preceu_ph_qbla (v4i8);
  v2q15 __builtin_mips_preceu_ph_qbra (v4i8);
  v4i8 __builtin_mips_shll_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shll_qb (v4i8, i32);
  v2q15 __builtin_mips_shll_ph (v2q15, imm0_15);
  v2q15 __builtin_mips_shll_ph (v2q15, i32);
  v2q15 __builtin_mips_shll_s_ph (v2q15, imm0_15);
  v2q15 __builtin_mips_shll_s_ph (v2q15, i32);
  q31 __builtin_mips_shll_s_w (q31, imm0_31);
  q31 __builtin_mips_shll_s_w (q31, i32);
  v4i8 __builtin_mips_shrl_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shrl_qb (v4i8, i32);
  v2q15 __builtin_mips_shra_ph (v2q15, imm0_15);
  v2q15 __builtin_mips_shra_ph (v2q15, i32);
  v2q15 __builtin_mips_shra_r_ph (v2q15, imm0_15);
  v2q15 __builtin_mips_shra_r_ph (v2q15, i32);
  q31 __builtin_mips_shra_r_w (q31, imm0_31);
  q31 __builtin_mips_shra_r_w (q31, i32);
  v2q15 __builtin_mips_muleu_s_ph_qbl (v4i8, v2q15);
  v2q15 __builtin_mips_muleu_s_ph_qbr (v4i8, v2q15);
  v2q15 __builtin_mips_mulq_rs_ph (v2q15, v2q15);
  q31 __builtin_mips_muleq_s_w_phl (v2q15, v2q15);
  q31 __builtin_mips_muleq_s_w_phr (v2q15, v2q15);
  a64 __builtin_mips_dpau_h_qbl (a64, v4i8, v4i8);
  a64 __builtin_mips_dpau_h_qbr (a64, v4i8, v4i8);
  a64 __builtin_mips_dpsu_h_qbl (a64, v4i8, v4i8);
  a64 __builtin_mips_dpsu_h_qbr (a64, v4i8, v4i8);
  a64 __builtin_mips_dpaq_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpaq_sa_l_w (a64, q31, q31);
  a64 __builtin_mips_dpsq_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpsq_sa_l_w (a64, q31, q31);
  a64 __builtin_mips_mulsaq_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_maq_s_w_phl (a64, v2q15, v2q15);
  a64 __builtin_mips_maq_s_w_phr (a64, v2q15, v2q15);
  a64 __builtin_mips_maq_sa_w_phl (a64, v2q15, v2q15);
  a64 __builtin_mips_maq_sa_w_phr (a64, v2q15, v2q15);
  i32 __builtin_mips_bitrev (i32);
  i32 __builtin_mips_insv (i32, i32);
  v4i8 __builtin_mips_repl_qb (imm0_255);
  v4i8 __builtin_mips_repl_qb (i32);
  v2q15 __builtin_mips_repl_ph (imm_n512_511);
  v2q15 __builtin_mips_repl_ph (i32);
  void __builtin_mips_cmpu_eq_qb (v4i8, v4i8);
  void __builtin_mips_cmpu_lt_qb (v4i8, v4i8);
  void __builtin_mips_cmpu_le_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgu_eq_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgu_lt_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgu_le_qb (v4i8, v4i8);
  void __builtin_mips_cmp_eq_ph (v2q15, v2q15);
  void __builtin_mips_cmp_lt_ph (v2q15, v2q15);
  void __builtin_mips_cmp_le_ph (v2q15, v2q15);
  v4i8 __builtin_mips_pick_qb (v4i8, v4i8);
  v2q15 __builtin_mips_pick_ph (v2q15, v2q15);
  v2q15 __builtin_mips_packrl_ph (v2q15, v2q15);
  i32 __builtin_mips_extr_w (a64, imm0_31);
  i32 __builtin_mips_extr_w (a64, i32);
  i32 __builtin_mips_extr_r_w (a64, imm0_31);
  i32 __builtin_mips_extr_s_h (a64, i32);
  i32 __builtin_mips_extr_rs_w (a64, imm0_31);
  i32 __builtin_mips_extr_rs_w (a64, i32);
  i32 __builtin_mips_extr_s_h (a64, imm0_31);
  i32 __builtin_mips_extr_r_w (a64, i32);
  i32 __builtin_mips_extp (a64, imm0_31);
  i32 __builtin_mips_extp (a64, i32);
  i32 __builtin_mips_extpdp (a64, imm0_31);
  i32 __builtin_mips_extpdp (a64, i32);
  a64 __builtin_mips_shilo (a64, imm_n32_31);
  a64 __builtin_mips_shilo (a64, i32);
  a64 __builtin_mips_mthlip (a64, i32);
  void __builtin_mips_wrdsp (i32, imm0_63);
  i32 __builtin_mips_rddsp (imm0_63);
  i32 __builtin_mips_lbux (void *, i32);
  i32 __builtin_mips_lhx (void *, i32);
  i32 __builtin_mips_lwx (void *, i32);
  a64 __builtin_mips_ldx (void *, i32); /* MIPS64 only */
  i32 __builtin_mips_bposge32 (void);
  a64 __builtin_mips_madd (a64, i32, i32);
  a64 __builtin_mips_maddu (a64, ui32, ui32);
  a64 __builtin_mips_msub (a64, i32, i32);
  a64 __builtin_mips_msubu (a64, ui32, ui32);
  a64 __builtin_mips_mult (i32, i32);
  a64 __builtin_mips_multu (ui32, ui32);

The following built-in functions map directly to a particular MIPS DSP REV 2
instruction.  Please refer to the architecture specification
for details on what each instruction does.

.. code-block:: c++

  v4q7 __builtin_mips_absq_s_qb (v4q7);
  v2i16 __builtin_mips_addu_ph (v2i16, v2i16);
  v2i16 __builtin_mips_addu_s_ph (v2i16, v2i16);
  v4i8 __builtin_mips_adduh_qb (v4i8, v4i8);
  v4i8 __builtin_mips_adduh_r_qb (v4i8, v4i8);
  i32 __builtin_mips_append (i32, i32, imm0_31);
  i32 __builtin_mips_balign (i32, i32, imm0_3);
  i32 __builtin_mips_cmpgdu_eq_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgdu_lt_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgdu_le_qb (v4i8, v4i8);
  a64 __builtin_mips_dpa_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dps_w_ph (a64, v2i16, v2i16);
  v2i16 __builtin_mips_mul_ph (v2i16, v2i16);
  v2i16 __builtin_mips_mul_s_ph (v2i16, v2i16);
  q31 __builtin_mips_mulq_rs_w (q31, q31);
  v2q15 __builtin_mips_mulq_s_ph (v2q15, v2q15);
  q31 __builtin_mips_mulq_s_w (q31, q31);
  a64 __builtin_mips_mulsa_w_ph (a64, v2i16, v2i16);
  v4i8 __builtin_mips_precr_qb_ph (v2i16, v2i16);
  v2i16 __builtin_mips_precr_sra_ph_w (i32, i32, imm0_31);
  v2i16 __builtin_mips_precr_sra_r_ph_w (i32, i32, imm0_31);
  i32 __builtin_mips_prepend (i32, i32, imm0_31);
  v4i8 __builtin_mips_shra_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shra_r_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shra_qb (v4i8, i32);
  v4i8 __builtin_mips_shra_r_qb (v4i8, i32);
  v2i16 __builtin_mips_shrl_ph (v2i16, imm0_15);
  v2i16 __builtin_mips_shrl_ph (v2i16, i32);
  v2i16 __builtin_mips_subu_ph (v2i16, v2i16);
  v2i16 __builtin_mips_subu_s_ph (v2i16, v2i16);
  v4i8 __builtin_mips_subuh_qb (v4i8, v4i8);
  v4i8 __builtin_mips_subuh_r_qb (v4i8, v4i8);
  v2q15 __builtin_mips_addqh_ph (v2q15, v2q15);
  v2q15 __builtin_mips_addqh_r_ph (v2q15, v2q15);
  q31 __builtin_mips_addqh_w (q31, q31);
  q31 __builtin_mips_addqh_r_w (q31, q31);
  v2q15 __builtin_mips_subqh_ph (v2q15, v2q15);
  v2q15 __builtin_mips_subqh_r_ph (v2q15, v2q15);
  q31 __builtin_mips_subqh_w (q31, q31);
  q31 __builtin_mips_subqh_r_w (q31, q31);
  a64 __builtin_mips_dpax_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dpsx_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dpaqx_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpaqx_sa_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpsqx_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpsqx_sa_w_ph (a64, v2q15, v2q15);