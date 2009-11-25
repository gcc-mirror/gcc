/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_OPTABS_H
#define GCC_OPTABS_H

#include "insn-codes.h"

/* Optabs are tables saying how to generate insn bodies
   for various machine modes and numbers of operands.
   Each optab applies to one operation.

   For example, add_optab applies to addition.

   The insn_code slot is the enum insn_code that says how to
   generate an insn for this operation on a particular machine mode.
   It is CODE_FOR_nothing if there is no such insn on the target machine.

   The `lib_call' slot is the name of the library function that
   can be used to perform the operation.

   A few optabs, such as move_optab, are used by special code.  */

struct optab_handlers
{
  enum insn_code insn_code;
};

struct optab_d
{
  enum rtx_code code;
  const char *libcall_basename;
  char libcall_suffix;
  void (*libcall_gen)(struct optab_d *, const char *name, char suffix,
		      enum machine_mode);
  struct optab_handlers handlers[NUM_MACHINE_MODES];
};
typedef struct optab_d * optab;

/* A convert_optab is for some sort of conversion operation between
   modes.  The first array index is the destination mode, the second
   is the source mode.  */
struct convert_optab_d
{
  enum rtx_code code;
  const char *libcall_basename;
  void (*libcall_gen)(struct convert_optab_d *, const char *name,
		      enum machine_mode,
		      enum machine_mode);
  struct optab_handlers handlers[NUM_MACHINE_MODES][NUM_MACHINE_MODES];
};
typedef struct convert_optab_d *convert_optab;

/* Given an enum insn_code, access the function to construct
   the body of that kind of insn.  */
#define GEN_FCN(CODE) (insn_data[CODE].genfun)

/* Enumeration of valid indexes into optab_table.  */
enum optab_index
{
  /* Fixed-point operators with signed/unsigned saturation */
  OTI_ssadd,
  OTI_usadd,
  OTI_sssub,
  OTI_ussub,
  OTI_ssmul,
  OTI_usmul,
  OTI_ssdiv,
  OTI_usdiv,
  OTI_ssneg,
  OTI_usneg,
  OTI_ssashl,
  OTI_usashl,

  OTI_add,
  OTI_addv,
  OTI_sub,
  OTI_subv,

  /* Signed and fp multiply */
  OTI_smul,
  OTI_smulv,
  /* Signed multiply, return high word */
  OTI_smul_highpart,
  OTI_umul_highpart,
  /* Signed multiply with result one machine mode wider than args */
  OTI_smul_widen,
  OTI_umul_widen,
  /* Widening multiply of one unsigned and one signed operand.  */
  OTI_usmul_widen,
  /* Signed multiply and add with the result and addend one machine mode
     wider than the multiplicand and multiplier.  */
  OTI_smadd_widen,
  /* Unsigned multiply and add with the result and addend one machine mode
     wider than the multiplicand and multiplier.  */
  OTI_umadd_widen,
  /* Signed multiply and add with the result and addend one machine mode
     wider than the multiplicand and multiplier.
     All involved operations are saturating.  */
  OTI_ssmadd_widen,
  /* Unsigned multiply and add with the result and addend one machine mode
     wider than the multiplicand and multiplier.
     All involved operations are saturating.  */
  OTI_usmadd_widen,
  /* Signed multiply and subtract the result and minuend one machine mode
     wider than the multiplicand and multiplier.  */
  OTI_smsub_widen,
  /* Unsigned multiply and subtract the result and minuend one machine mode
     wider than the multiplicand and multiplier.  */
  OTI_umsub_widen,
  /* Signed multiply and subtract the result and minuend one machine mode
     wider than the multiplicand and multiplier.
     All involved operations are saturating.  */
  OTI_ssmsub_widen,
  /* Unsigned multiply and subtract the result and minuend one machine mode
     wider than the multiplicand and multiplier.
     All involved operations are saturating.  */
  OTI_usmsub_widen,

  /* Signed divide */
  OTI_sdiv,
  OTI_sdivv,
  /* Signed divide-and-remainder in one */
  OTI_sdivmod,
  OTI_udiv,
  OTI_udivmod,
  /* Signed remainder */
  OTI_smod,
  OTI_umod,
  /* Floating point remainder functions */
  OTI_fmod,
  OTI_remainder,
  /* Convert float to integer in float fmt */
  OTI_ftrunc,

  /* Logical and */
  OTI_and,
  /* Logical or */
  OTI_ior,
  /* Logical xor */
  OTI_xor,

  /* Arithmetic shift left */
  OTI_ashl,
  /* Logical shift right */
  OTI_lshr,
  /* Arithmetic shift right */
  OTI_ashr,
  /* Rotate left */
  OTI_rotl,
  /* Rotate right */
  OTI_rotr,

  /* Arithmetic shift left of vector by vector */
  OTI_vashl,
  /* Logical shift right of vector by vector */
  OTI_vlshr,
  /* Arithmetic shift right of vector by vector */
  OTI_vashr,
  /* Rotate left of vector by vector */
  OTI_vrotl,
  /* Rotate right of vector by vector */
  OTI_vrotr,

  /* Signed and floating-point minimum value */
  OTI_smin,
  /* Signed and floating-point maximum value */
  OTI_smax,
  /* Unsigned minimum value */
  OTI_umin,
  /* Unsigned maximum value */
  OTI_umax,
  /* Power */
  OTI_pow,
  /* Arc tangent of y/x */
  OTI_atan2,

  /* Move instruction.  */
  OTI_mov,
  /* Move, preserving high part of register.  */
  OTI_movstrict,
  /* Move, with a misaligned memory.  */
  OTI_movmisalign,
  /* Nontemporal store.  */
  OTI_storent,

  /* Unary operations */
  /* Negation */
  OTI_neg,
  OTI_negv,
  /* Abs value */
  OTI_abs,
  OTI_absv,
  /* Byteswap */
  OTI_bswap,
  /* Bitwise not */
  OTI_one_cmpl,
  /* Bit scanning and counting */
  OTI_ffs,
  OTI_clz,
  OTI_ctz,
  OTI_popcount,
  OTI_parity,
  /* Square root */
  OTI_sqrt,
  /* Sine-Cosine */
  OTI_sincos,
  /* Sine */
  OTI_sin,
  /* Inverse sine */
  OTI_asin,
  /* Cosine */
  OTI_cos,
  /* Inverse cosine */
  OTI_acos,
  /* Exponential */
  OTI_exp,
  /* Base-10 Exponential */
  OTI_exp10,
  /* Base-2 Exponential */
  OTI_exp2,
  /* Exponential - 1*/
  OTI_expm1,
  /* Load exponent of a floating point number */
  OTI_ldexp,
  /* Multiply floating-point number by integral power of radix */
  OTI_scalb,
  /* Mantissa of a floating-point number */
  OTI_significand,
  /* Radix-independent exponent */
  OTI_logb,
  OTI_ilogb,
  /* Natural Logarithm */
  OTI_log,
  /* Base-10 Logarithm */
  OTI_log10,
  /* Base-2 Logarithm */
  OTI_log2,
  /* logarithm of 1 plus argument */
  OTI_log1p,
  /* Rounding functions */
  OTI_floor,
  OTI_ceil,
  OTI_btrunc,
  OTI_round,
  OTI_nearbyint,
  OTI_rint,
  /* Tangent */
  OTI_tan,
  /* Inverse tangent */
  OTI_atan,
  /* Copy sign */
  OTI_copysign,
  /* Signbit */
  OTI_signbit,
  /* Test for infinite value */
  OTI_isinf,

  /* Compare insn; two operands.  Used only for libcalls.  */
  OTI_cmp,
  OTI_ucmp,

  /* Floating point comparison optabs - used primarily for libfuncs */
  OTI_eq,
  OTI_ne,
  OTI_gt,
  OTI_ge,
  OTI_lt,
  OTI_le,
  OTI_unord,

  /* String length */
  OTI_strlen,

  /* Combined compare & jump/move/store flags/trap operations.  */
  OTI_cbranch,
  OTI_cmov,
  OTI_cstore,
  OTI_ctrap,

  /* Push instruction.  */
  OTI_push,

  /* Conditional add instruction.  */
  OTI_addcc,

  /* Reduction operations on a vector operand.  */
  OTI_reduc_smax,
  OTI_reduc_umax,
  OTI_reduc_smin,
  OTI_reduc_umin,
  OTI_reduc_splus,
  OTI_reduc_uplus,

  /* Summation, with result machine mode one or more wider than args.  */
  OTI_ssum_widen,
  OTI_usum_widen,

  /* Dot product, with result machine mode one or more wider than args.  */
  OTI_sdot_prod,
  OTI_udot_prod,

  /* Set specified field of vector operand.  */
  OTI_vec_set,
  /* Extract specified field of vector operand.  */
  OTI_vec_extract,
  /* Extract even/odd fields of vector operands.  */
  OTI_vec_extract_even,
  OTI_vec_extract_odd,
  /* Interleave fields of vector operands.  */
  OTI_vec_interleave_high,
  OTI_vec_interleave_low,
  /* Initialize vector operand.  */
  OTI_vec_init,
  /* Whole vector shift. The shift amount is in bits.  */
  OTI_vec_shl,
  OTI_vec_shr,
  /* Extract specified elements from vectors, for vector load.  */
  OTI_vec_realign_load,
  /* Widening multiplication.
     The high/low part of the resulting vector of products is returned.  */
  OTI_vec_widen_umult_hi,
  OTI_vec_widen_umult_lo,
  OTI_vec_widen_smult_hi,
  OTI_vec_widen_smult_lo,
  /* Extract and widen the high/low part of a vector of signed or
     floating point elements.  */
  OTI_vec_unpacks_hi,
  OTI_vec_unpacks_lo,
  /* Extract and widen the high/low part of a vector of unsigned
     elements.  */
  OTI_vec_unpacku_hi,
  OTI_vec_unpacku_lo,

  /* Extract, convert to floating point and widen the high/low part of
     a vector of signed or unsigned integer elements.  */
  OTI_vec_unpacks_float_hi,
  OTI_vec_unpacks_float_lo,
  OTI_vec_unpacku_float_hi,
  OTI_vec_unpacku_float_lo,

  /* Narrow (demote) and merge the elements of two vectors.  */
  OTI_vec_pack_trunc,
  OTI_vec_pack_usat,
  OTI_vec_pack_ssat,

  /* Convert to signed/unsigned integer, narrow and merge elements
     of two vectors of floating point elements.  */
  OTI_vec_pack_sfix_trunc,
  OTI_vec_pack_ufix_trunc,

  /* Perform a raise to the power of integer.  */
  OTI_powi,

  OTI_MAX
};

extern struct optab_d optab_table[OTI_MAX];

#define ssadd_optab (&optab_table[OTI_ssadd])
#define usadd_optab (&optab_table[OTI_usadd])
#define sssub_optab (&optab_table[OTI_sssub])
#define ussub_optab (&optab_table[OTI_ussub])
#define ssmul_optab (&optab_table[OTI_ssmul])
#define usmul_optab (&optab_table[OTI_usmul])
#define ssdiv_optab (&optab_table[OTI_ssdiv])
#define usdiv_optab (&optab_table[OTI_usdiv])
#define ssneg_optab (&optab_table[OTI_ssneg])
#define usneg_optab (&optab_table[OTI_usneg])
#define ssashl_optab (&optab_table[OTI_ssashl])
#define usashl_optab (&optab_table[OTI_usashl])

#define add_optab (&optab_table[OTI_add])
#define sub_optab (&optab_table[OTI_sub])
#define smul_optab (&optab_table[OTI_smul])
#define addv_optab (&optab_table[OTI_addv])
#define subv_optab (&optab_table[OTI_subv])
#define smul_highpart_optab (&optab_table[OTI_smul_highpart])
#define umul_highpart_optab (&optab_table[OTI_umul_highpart])
#define smul_widen_optab (&optab_table[OTI_smul_widen])
#define umul_widen_optab (&optab_table[OTI_umul_widen])
#define usmul_widen_optab (&optab_table[OTI_usmul_widen])
#define smadd_widen_optab (&optab_table[OTI_smadd_widen])
#define umadd_widen_optab (&optab_table[OTI_umadd_widen])
#define ssmadd_widen_optab (&optab_table[OTI_ssmadd_widen])
#define usmadd_widen_optab (&optab_table[OTI_usmadd_widen])
#define smsub_widen_optab (&optab_table[OTI_smsub_widen])
#define umsub_widen_optab (&optab_table[OTI_umsub_widen])
#define ssmsub_widen_optab (&optab_table[OTI_ssmsub_widen])
#define usmsub_widen_optab (&optab_table[OTI_usmsub_widen])
#define sdiv_optab (&optab_table[OTI_sdiv])
#define smulv_optab (&optab_table[OTI_smulv])
#define sdivv_optab (&optab_table[OTI_sdivv])
#define sdivmod_optab (&optab_table[OTI_sdivmod])
#define udiv_optab (&optab_table[OTI_udiv])
#define udivmod_optab (&optab_table[OTI_udivmod])
#define smod_optab (&optab_table[OTI_smod])
#define umod_optab (&optab_table[OTI_umod])
#define fmod_optab (&optab_table[OTI_fmod])
#define remainder_optab (&optab_table[OTI_remainder])
#define ftrunc_optab (&optab_table[OTI_ftrunc])
#define and_optab (&optab_table[OTI_and])
#define ior_optab (&optab_table[OTI_ior])
#define xor_optab (&optab_table[OTI_xor])
#define ashl_optab (&optab_table[OTI_ashl])
#define lshr_optab (&optab_table[OTI_lshr])
#define ashr_optab (&optab_table[OTI_ashr])
#define rotl_optab (&optab_table[OTI_rotl])
#define rotr_optab (&optab_table[OTI_rotr])
#define vashl_optab (&optab_table[OTI_vashl])
#define vlshr_optab (&optab_table[OTI_vlshr])
#define vashr_optab (&optab_table[OTI_vashr])
#define vrotl_optab (&optab_table[OTI_vrotl])
#define vrotr_optab (&optab_table[OTI_vrotr])
#define smin_optab (&optab_table[OTI_smin])
#define smax_optab (&optab_table[OTI_smax])
#define umin_optab (&optab_table[OTI_umin])
#define umax_optab (&optab_table[OTI_umax])
#define pow_optab (&optab_table[OTI_pow])
#define atan2_optab (&optab_table[OTI_atan2])

#define mov_optab (&optab_table[OTI_mov])
#define movstrict_optab (&optab_table[OTI_movstrict])
#define movmisalign_optab (&optab_table[OTI_movmisalign])
#define storent_optab (&optab_table[OTI_storent])

#define neg_optab (&optab_table[OTI_neg])
#define negv_optab (&optab_table[OTI_negv])
#define abs_optab (&optab_table[OTI_abs])
#define absv_optab (&optab_table[OTI_absv])
#define one_cmpl_optab (&optab_table[OTI_one_cmpl])
#define bswap_optab (&optab_table[OTI_bswap])
#define ffs_optab (&optab_table[OTI_ffs])
#define clz_optab (&optab_table[OTI_clz])
#define ctz_optab (&optab_table[OTI_ctz])
#define popcount_optab (&optab_table[OTI_popcount])
#define parity_optab (&optab_table[OTI_parity])
#define sqrt_optab (&optab_table[OTI_sqrt])
#define sincos_optab (&optab_table[OTI_sincos])
#define sin_optab (&optab_table[OTI_sin])
#define asin_optab (&optab_table[OTI_asin])
#define cos_optab (&optab_table[OTI_cos])
#define acos_optab (&optab_table[OTI_acos])
#define exp_optab (&optab_table[OTI_exp])
#define exp10_optab (&optab_table[OTI_exp10])
#define exp2_optab (&optab_table[OTI_exp2])
#define expm1_optab (&optab_table[OTI_expm1])
#define ldexp_optab (&optab_table[OTI_ldexp])
#define scalb_optab (&optab_table[OTI_scalb])
#define significand_optab (&optab_table[OTI_significand])
#define logb_optab (&optab_table[OTI_logb])
#define ilogb_optab (&optab_table[OTI_ilogb])
#define log_optab (&optab_table[OTI_log])
#define log10_optab (&optab_table[OTI_log10])
#define log2_optab (&optab_table[OTI_log2])
#define log1p_optab (&optab_table[OTI_log1p])
#define floor_optab (&optab_table[OTI_floor])
#define ceil_optab (&optab_table[OTI_ceil])
#define btrunc_optab (&optab_table[OTI_btrunc])
#define round_optab (&optab_table[OTI_round])
#define nearbyint_optab (&optab_table[OTI_nearbyint])
#define rint_optab (&optab_table[OTI_rint])
#define tan_optab (&optab_table[OTI_tan])
#define atan_optab (&optab_table[OTI_atan])
#define copysign_optab (&optab_table[OTI_copysign])
#define signbit_optab (&optab_table[OTI_signbit])
#define isinf_optab (&optab_table[OTI_isinf])

#define cmp_optab (&optab_table[OTI_cmp])
#define ucmp_optab (&optab_table[OTI_ucmp])

#define eq_optab (&optab_table[OTI_eq])
#define ne_optab (&optab_table[OTI_ne])
#define gt_optab (&optab_table[OTI_gt])
#define ge_optab (&optab_table[OTI_ge])
#define lt_optab (&optab_table[OTI_lt])
#define le_optab (&optab_table[OTI_le])
#define unord_optab (&optab_table[OTI_unord])

#define strlen_optab (&optab_table[OTI_strlen])

#define cbranch_optab (&optab_table[OTI_cbranch])
#define cmov_optab (&optab_table[OTI_cmov])
#define cstore_optab (&optab_table[OTI_cstore])
#define ctrap_optab (&optab_table[OTI_ctrap])

#define push_optab (&optab_table[OTI_push])
#define addcc_optab (&optab_table[OTI_addcc])

#define reduc_smax_optab (&optab_table[OTI_reduc_smax])
#define reduc_umax_optab (&optab_table[OTI_reduc_umax])
#define reduc_smin_optab (&optab_table[OTI_reduc_smin])
#define reduc_umin_optab (&optab_table[OTI_reduc_umin])
#define reduc_splus_optab (&optab_table[OTI_reduc_splus])
#define reduc_uplus_optab (&optab_table[OTI_reduc_uplus])

#define ssum_widen_optab (&optab_table[OTI_ssum_widen])
#define usum_widen_optab (&optab_table[OTI_usum_widen])
#define sdot_prod_optab (&optab_table[OTI_sdot_prod])
#define udot_prod_optab (&optab_table[OTI_udot_prod])

#define vec_set_optab (&optab_table[OTI_vec_set])
#define vec_extract_optab (&optab_table[OTI_vec_extract])
#define vec_extract_even_optab (&optab_table[OTI_vec_extract_even])
#define vec_extract_odd_optab (&optab_table[OTI_vec_extract_odd])
#define vec_interleave_high_optab (&optab_table[OTI_vec_interleave_high])
#define vec_interleave_low_optab (&optab_table[OTI_vec_interleave_low])
#define vec_init_optab (&optab_table[OTI_vec_init])
#define vec_shl_optab (&optab_table[OTI_vec_shl])
#define vec_shr_optab (&optab_table[OTI_vec_shr])
#define vec_realign_load_optab (&optab_table[OTI_vec_realign_load])
#define vec_widen_umult_hi_optab (&optab_table[OTI_vec_widen_umult_hi])
#define vec_widen_umult_lo_optab (&optab_table[OTI_vec_widen_umult_lo])
#define vec_widen_smult_hi_optab (&optab_table[OTI_vec_widen_smult_hi])
#define vec_widen_smult_lo_optab (&optab_table[OTI_vec_widen_smult_lo])
#define vec_unpacks_hi_optab (&optab_table[OTI_vec_unpacks_hi])
#define vec_unpacks_lo_optab (&optab_table[OTI_vec_unpacks_lo])
#define vec_unpacku_hi_optab (&optab_table[OTI_vec_unpacku_hi])
#define vec_unpacku_lo_optab (&optab_table[OTI_vec_unpacku_lo])
#define vec_unpacks_float_hi_optab (&optab_table[OTI_vec_unpacks_float_hi])
#define vec_unpacks_float_lo_optab (&optab_table[OTI_vec_unpacks_float_lo])
#define vec_unpacku_float_hi_optab (&optab_table[OTI_vec_unpacku_float_hi])
#define vec_unpacku_float_lo_optab (&optab_table[OTI_vec_unpacku_float_lo])
#define vec_pack_trunc_optab (&optab_table[OTI_vec_pack_trunc])
#define vec_pack_ssat_optab (&optab_table[OTI_vec_pack_ssat])
#define vec_pack_usat_optab (&optab_table[OTI_vec_pack_usat])
#define vec_pack_sfix_trunc_optab (&optab_table[OTI_vec_pack_sfix_trunc])
#define vec_pack_ufix_trunc_optab (&optab_table[OTI_vec_pack_ufix_trunc])

#define powi_optab (&optab_table[OTI_powi])

/* Conversion optabs have their own table and indexes.  */
enum convert_optab_index
{
  COI_sext,
  COI_zext,
  COI_trunc,

  COI_sfix,
  COI_ufix,

  COI_sfixtrunc,
  COI_ufixtrunc,

  COI_sfloat,
  COI_ufloat,

  COI_lrint,
  COI_lround,
  COI_lfloor,
  COI_lceil,

  COI_fract,
  COI_fractuns,
  COI_satfract,
  COI_satfractuns,

  COI_MAX
};

extern struct convert_optab_d convert_optab_table[COI_MAX];

#define sext_optab (&convert_optab_table[COI_sext])
#define zext_optab (&convert_optab_table[COI_zext])
#define trunc_optab (&convert_optab_table[COI_trunc])
#define sfix_optab (&convert_optab_table[COI_sfix])
#define ufix_optab (&convert_optab_table[COI_ufix])
#define sfixtrunc_optab (&convert_optab_table[COI_sfixtrunc])
#define ufixtrunc_optab (&convert_optab_table[COI_ufixtrunc])
#define sfloat_optab (&convert_optab_table[COI_sfloat])
#define ufloat_optab (&convert_optab_table[COI_ufloat])
#define lrint_optab (&convert_optab_table[COI_lrint])
#define lround_optab (&convert_optab_table[COI_lround])
#define lfloor_optab (&convert_optab_table[COI_lfloor])
#define lceil_optab (&convert_optab_table[COI_lceil])
#define fract_optab (&convert_optab_table[COI_fract])
#define fractuns_optab (&convert_optab_table[COI_fractuns])
#define satfract_optab (&convert_optab_table[COI_satfract])
#define satfractuns_optab (&convert_optab_table[COI_satfractuns])

/* These arrays record the insn_code of insns that may be needed to
   perform input and output reloads of special objects.  They provide a
   place to pass a scratch register.  */
extern enum insn_code reload_in_optab[NUM_MACHINE_MODES];
extern enum insn_code reload_out_optab[NUM_MACHINE_MODES];

/* Contains the optab used for each rtx code.  */
extern optab code_to_optab[NUM_RTX_CODE + 1];


typedef rtx (*rtxfun) (rtx);

#ifdef HAVE_conditional_move
/* Indexed by the machine mode, gives the insn code to make a conditional
   move insn.  */

extern enum insn_code movcc_gen_code[NUM_MACHINE_MODES];
#endif

/* Indexed by the machine mode, gives the insn code for vector conditional
   operation.  */

extern enum insn_code vcond_gen_code[NUM_MACHINE_MODES];
extern enum insn_code vcondu_gen_code[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block moves.  */
extern enum insn_code movmem_optab[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block sets.  */
extern enum insn_code setmem_optab[NUM_MACHINE_MODES];

/* These arrays record the insn_code of two different kinds of insns
   to perform block compares.  */
extern enum insn_code cmpstr_optab[NUM_MACHINE_MODES];
extern enum insn_code cmpstrn_optab[NUM_MACHINE_MODES];
extern enum insn_code cmpmem_optab[NUM_MACHINE_MODES];

/* Synchronization primitives.  This first set is atomic operation for
   which we don't care about the resulting value.  */
extern enum insn_code sync_add_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_sub_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_ior_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_and_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_xor_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_nand_optab[NUM_MACHINE_MODES];

/* This second set is atomic operations in which we return the value
   that existed in memory before the operation.  */
extern enum insn_code sync_old_add_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_old_sub_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_old_ior_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_old_and_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_old_xor_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_old_nand_optab[NUM_MACHINE_MODES];

/* This third set is atomic operations in which we return the value
   that resulted after performing the operation.  */
extern enum insn_code sync_new_add_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_new_sub_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_new_ior_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_new_and_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_new_xor_optab[NUM_MACHINE_MODES];
extern enum insn_code sync_new_nand_optab[NUM_MACHINE_MODES];

/* Atomic compare and swap.  */
extern enum insn_code sync_compare_and_swap[NUM_MACHINE_MODES];

/* Atomic exchange with acquire semantics.  */
extern enum insn_code sync_lock_test_and_set[NUM_MACHINE_MODES];

/* Atomic clear with release semantics.  */
extern enum insn_code sync_lock_release[NUM_MACHINE_MODES];

/* Define functions given in optabs.c.  */

extern rtx expand_widen_pattern_expr (sepops ops, rtx op0, rtx op1, rtx wide_op,
                                      rtx target, int unsignedp);

extern rtx expand_ternary_op (enum machine_mode mode, optab ternary_optab,
			      rtx op0, rtx op1, rtx op2, rtx target,
			      int unsignedp);

/* Expand a binary operation given optab and rtx operands.  */
extern rtx expand_binop (enum machine_mode, optab, rtx, rtx, rtx, int,
			 enum optab_methods);

extern bool force_expand_binop (enum machine_mode, optab, rtx, rtx, rtx, int,
				enum optab_methods);

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop (enum machine_mode, optab, optab, rtx, rtx,
			      rtx, int, enum optab_methods);

/* Generate code to perform an operation on one operand with two results.  */
extern int expand_twoval_unop (optab, rtx, rtx, rtx, int);

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop (optab, rtx, rtx, rtx, rtx, int);

/* Generate code to perform an operation on two operands with two
   results, using a library function.  */
extern bool expand_twoval_binop_libfunc (optab, rtx, rtx, rtx, rtx,
					 enum rtx_code);

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop (enum machine_mode, optab, rtx, rtx, int);

/* Expand the absolute value operation.  */
extern rtx expand_abs_nojump (enum machine_mode, rtx, rtx, int);
extern rtx expand_abs (enum machine_mode, rtx, rtx, int, int);

/* Expand the one's complement absolute value operation.  */
extern rtx expand_one_cmpl_abs_nojump (enum machine_mode, rtx, rtx);

/* Expand the copysign operation.  */
extern rtx expand_copysign (rtx, rtx, rtx);

/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern void emit_unop_insn (int, rtx, rtx, enum rtx_code);
extern bool maybe_emit_unop_insn (int, rtx, rtx, enum rtx_code);

/* An extra flag to control optab_for_tree_code's behavior.  This is needed to
   distinguish between machines with a vector shift that takes a scalar for the
   shift amount vs. machines that take a vector for the shift amount.  */
enum optab_subtype
{
  optab_default,
  optab_scalar,
  optab_vector
};

/* Return the optab used for computing the given operation on the type given by
   the second argument.  The third argument distinguishes between the types of
   vector shifts and rotates */
extern optab optab_for_tree_code (enum tree_code, const_tree, enum optab_subtype);

/* The various uses that a comparison can have; used by can_compare_p:
   jumps, conditional moves, store flag operations.  */
enum can_compare_purpose
{
  ccp_jump,
  ccp_cmov,
  ccp_store_flag
};

/* Nonzero if a compare of mode MODE can be done straightforwardly
   (without splitting it into pieces).  */
extern int can_compare_p (enum rtx_code, enum machine_mode,
			  enum can_compare_purpose);

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p (enum machine_mode, enum machine_mode, int);

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn (rtx, rtx, enum machine_mode,
			    enum machine_mode, int);

/* Call this to reset the function entry for one optab.  */
extern void set_optab_libfunc (optab, enum machine_mode, const char *);
extern void set_conv_libfunc (convert_optab, enum machine_mode,
			      enum machine_mode, const char *);

/* Generate code for a FIXED_CONVERT_EXPR.  */
extern void expand_fixed_convert (rtx, rtx, int, int);

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float (rtx, rtx, int);

/* Generate code for a FIX_EXPR.  */
extern void expand_fix (rtx, rtx, int);

/* Generate code for float to integral conversion.  */
extern bool expand_sfix_optab (rtx, rtx, convert_optab);

/* Return tree if target supports vector operations for COND_EXPR.  */
bool expand_vec_cond_expr_p (tree, enum machine_mode);

/* Generate code for VEC_COND_EXPR.  */
extern rtx expand_vec_cond_expr (tree, tree, tree, tree, rtx);
/* Generate code for VEC_LSHIFT_EXPR and VEC_RSHIFT_EXPR.  */
extern rtx expand_vec_shift_expr (sepops, rtx);

#define optab_handler(optab,mode) (&(optab)->handlers[(int) (mode)])
#define convert_optab_handler(optab,mode,mode2) \
	(&(optab)->handlers[(int) (mode)][(int) (mode2)])

extern rtx optab_libfunc (optab optab, enum machine_mode mode);
extern rtx convert_optab_libfunc (convert_optab optab, enum machine_mode mode1,
			          enum machine_mode mode2);
#endif /* GCC_OPTABS_H */
