/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

   A few optabs, such as move_optab and cmp_optab, are used
   by special code.  */

struct optab_handlers GTY(())
{
  enum insn_code insn_code;
  rtx libfunc;
};

struct optab GTY(())
{
  enum rtx_code code;
  struct optab_handlers handlers[NUM_MACHINE_MODES];
};
typedef struct optab * optab;

/* A convert_optab is for some sort of conversion operation between
   modes.  The first array index is the destination mode, the second
   is the source mode.  */
struct convert_optab GTY(())
{
  enum rtx_code code;
  struct optab_handlers handlers[NUM_MACHINE_MODES][NUM_MACHINE_MODES];
};
typedef struct convert_optab *convert_optab;

/* Given an enum insn_code, access the function to construct
   the body of that kind of insn.  */
#define GEN_FCN(CODE) (insn_data[CODE].genfun)

/* Enumeration of valid indexes into optab_table.  */
enum optab_index
{
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

  /* Unary operations */
  /* Negation */
  OTI_neg,
  OTI_negv,
  /* Abs value */
  OTI_abs,
  OTI_absv,
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
  /* Sine */
  OTI_sin,
  /* Cosine */
  OTI_cos,
  /* Exponential */
  OTI_exp,
  /* Natural Logarithm */
  OTI_log,
  /* Rounding functions */
  OTI_floor,
  OTI_ceil,
  OTI_trunc,
  OTI_round,
  OTI_nearbyint,
  /* Tangent */
  OTI_tan,
  /* Inverse tangent */
  OTI_atan,

  /* Compare insn; two operands.  */
  OTI_cmp,
  /* Used only for libcalls for unsigned comparisons.  */
  OTI_ucmp,
  /* tst insn; compare one operand against 0 */
  OTI_tst,

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

  /* Combined compare & jump/store flags/move operations.  */
  OTI_cbranch,
  OTI_cmov,
  OTI_cstore,

  /* Push instruction.  */
  OTI_push,

  /* Conditional add instruction.  */
  OTI_addcc,

  /* Set specified field of vector operand.  */
  OTI_vec_set,
  /* Extract specified field of vector operand.  */
  OTI_vec_extract,
  /* Initialize vector operand.  */
  OTI_vec_init,

  OTI_MAX
};

extern GTY(()) optab optab_table[OTI_MAX];

#define add_optab (optab_table[OTI_add])
#define sub_optab (optab_table[OTI_sub])
#define smul_optab (optab_table[OTI_smul])
#define addv_optab (optab_table[OTI_addv])
#define subv_optab (optab_table[OTI_subv])
#define smul_highpart_optab (optab_table[OTI_smul_highpart])
#define umul_highpart_optab (optab_table[OTI_umul_highpart])
#define smul_widen_optab (optab_table[OTI_smul_widen])
#define umul_widen_optab (optab_table[OTI_umul_widen])
#define sdiv_optab (optab_table[OTI_sdiv])
#define smulv_optab (optab_table[OTI_smulv])
#define sdivv_optab (optab_table[OTI_sdivv])
#define sdivmod_optab (optab_table[OTI_sdivmod])
#define udiv_optab (optab_table[OTI_udiv])
#define udivmod_optab (optab_table[OTI_udivmod])
#define smod_optab (optab_table[OTI_smod])
#define umod_optab (optab_table[OTI_umod])
#define ftrunc_optab (optab_table[OTI_ftrunc])
#define and_optab (optab_table[OTI_and])
#define ior_optab (optab_table[OTI_ior])
#define xor_optab (optab_table[OTI_xor])
#define ashl_optab (optab_table[OTI_ashl])
#define lshr_optab (optab_table[OTI_lshr])
#define ashr_optab (optab_table[OTI_ashr])
#define rotl_optab (optab_table[OTI_rotl])
#define rotr_optab (optab_table[OTI_rotr])
#define smin_optab (optab_table[OTI_smin])
#define smax_optab (optab_table[OTI_smax])
#define umin_optab (optab_table[OTI_umin])
#define umax_optab (optab_table[OTI_umax])
#define pow_optab (optab_table[OTI_pow])
#define atan2_optab (optab_table[OTI_atan2])

#define mov_optab (optab_table[OTI_mov])
#define movstrict_optab (optab_table[OTI_movstrict])

#define neg_optab (optab_table[OTI_neg])
#define negv_optab (optab_table[OTI_negv])
#define abs_optab (optab_table[OTI_abs])
#define absv_optab (optab_table[OTI_absv])
#define one_cmpl_optab (optab_table[OTI_one_cmpl])
#define ffs_optab (optab_table[OTI_ffs])
#define clz_optab (optab_table[OTI_clz])
#define ctz_optab (optab_table[OTI_ctz])
#define popcount_optab (optab_table[OTI_popcount])
#define parity_optab (optab_table[OTI_parity])
#define sqrt_optab (optab_table[OTI_sqrt])
#define sin_optab (optab_table[OTI_sin])
#define cos_optab (optab_table[OTI_cos])
#define exp_optab (optab_table[OTI_exp])
#define log_optab (optab_table[OTI_log])
#define floor_optab (optab_table[OTI_floor])
#define ceil_optab (optab_table[OTI_ceil])
#define btrunc_optab (optab_table[OTI_trunc])
#define round_optab (optab_table[OTI_round])
#define nearbyint_optab (optab_table[OTI_nearbyint])
#define tan_optab (optab_table[OTI_tan])
#define atan_optab (optab_table[OTI_atan])

#define cmp_optab (optab_table[OTI_cmp])
#define ucmp_optab (optab_table[OTI_ucmp])
#define tst_optab (optab_table[OTI_tst])

#define eq_optab (optab_table[OTI_eq])
#define ne_optab (optab_table[OTI_ne])
#define gt_optab (optab_table[OTI_gt])
#define ge_optab (optab_table[OTI_ge])
#define lt_optab (optab_table[OTI_lt])
#define le_optab (optab_table[OTI_le])
#define unord_optab (optab_table[OTI_unord])

#define strlen_optab (optab_table[OTI_strlen])

#define cbranch_optab (optab_table[OTI_cbranch])
#define cmov_optab (optab_table[OTI_cmov])
#define cstore_optab (optab_table[OTI_cstore])
#define push_optab (optab_table[OTI_push])
#define addcc_optab (optab_table[OTI_addcc])

#define vec_set_optab (optab_table[OTI_vec_set])
#define vec_extract_optab (optab_table[OTI_vec_extract])
#define vec_init_optab (optab_table[OTI_vec_init])

/* Conversion optabs have their own table and indexes.  */
enum convert_optab_index
{
  CTI_sext,
  CTI_zext,
  CTI_trunc,

  CTI_sfix,
  CTI_ufix,

  CTI_sfixtrunc,
  CTI_ufixtrunc,

  CTI_sfloat,
  CTI_ufloat,

  CTI_MAX
};

extern GTY(()) convert_optab convert_optab_table[CTI_MAX];

#define sext_optab (convert_optab_table[CTI_sext])
#define zext_optab (convert_optab_table[CTI_zext])
#define trunc_optab (convert_optab_table[CTI_trunc])
#define sfix_optab (convert_optab_table[CTI_sfix])
#define ufix_optab (convert_optab_table[CTI_ufix])
#define sfixtrunc_optab (convert_optab_table[CTI_sfixtrunc])
#define ufixtrunc_optab (convert_optab_table[CTI_ufixtrunc])
#define sfloat_optab (convert_optab_table[CTI_sfloat])
#define ufloat_optab (convert_optab_table[CTI_ufloat])

/* These arrays record the insn_code of insns that may be needed to
   perform input and output reloads of special objects.  They provide a
   place to pass a scratch register.  */
extern enum insn_code reload_in_optab[NUM_MACHINE_MODES];
extern enum insn_code reload_out_optab[NUM_MACHINE_MODES];

/* Contains the optab used for each rtx code.  */
extern GTY(()) optab code_to_optab[NUM_RTX_CODE + 1];


typedef rtx (*rtxfun) (rtx);

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

extern rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the insn code to make a store-condition insn
   to test that condition.  */

extern enum insn_code setcc_gen_code[NUM_RTX_CODE];

#ifdef HAVE_conditional_move
/* Indexed by the machine mode, gives the insn code to make a conditional
   move insn.  */

extern enum insn_code movcc_gen_code[NUM_MACHINE_MODES];
#endif

/* This array records the insn_code of insns to perform block moves.  */
extern enum insn_code movstr_optab[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block clears.  */
extern enum insn_code clrstr_optab[NUM_MACHINE_MODES];

/* These arrays record the insn_code of two different kinds of insns
   to perform block compares.  */
extern enum insn_code cmpstr_optab[NUM_MACHINE_MODES];
extern enum insn_code cmpmem_optab[NUM_MACHINE_MODES];

/* Define functions given in optabs.c.  */

/* Expand a binary operation given optab and rtx operands.  */
extern rtx expand_binop (enum machine_mode, optab, rtx, rtx, rtx, int,
			 enum optab_methods);

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop (enum machine_mode, optab, optab, rtx, rtx,
			      rtx, int, enum optab_methods);

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop (optab, rtx, rtx, rtx, rtx, int);

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop (enum machine_mode, optab, rtx, rtx, int);

/* Expand the absolute value operation.  */
extern rtx expand_abs_nojump (enum machine_mode, rtx, rtx, int);
extern rtx expand_abs (enum machine_mode, rtx, rtx, int, int);

/* Expand the complex absolute value operation.  */
extern rtx expand_complex_abs (enum machine_mode, rtx, rtx, int);

/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern void emit_unop_insn (int, rtx, rtx, enum rtx_code);

/* Emit code to perform a series of operations on a multi-word quantity, one
   word at a time.  */
extern rtx emit_no_conflict_block (rtx, rtx, rtx, rtx, rtx);

/* Emit one rtl instruction to store zero in specified rtx.  */
extern void emit_clr_insn (rtx);

/* Emit one rtl insn to store 1 in specified rtx assuming it contains 0.  */
extern void emit_0_to_1_insn (rtx);

/* Emit one rtl insn to compare two rtx's.  */
extern void emit_cmp_insn (rtx, rtx, enum rtx_code, rtx, enum machine_mode,
			   int);

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

extern rtx prepare_operand (int, rtx, int, enum machine_mode,
			    enum machine_mode, int);

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p (enum machine_mode, enum machine_mode, int);

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn (rtx, rtx, enum machine_mode,
			    enum machine_mode, int);

/* Initialize the tables that control conversion between fixed and
   floating values.  */
extern void init_fixtab (void);
extern void init_floattab (void);

/* Call this to reset the function entry for one optab.  */
extern void set_optab_libfunc (optab, enum machine_mode, const char *);
extern void set_conv_libfunc (convert_optab, enum machine_mode,
			      enum machine_mode, const char *);

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float (rtx, rtx, int);

/* Generate code for a FIX_EXPR.  */
extern void expand_fix (rtx, rtx, int);

#endif /* GCC_OPTABS_H */
