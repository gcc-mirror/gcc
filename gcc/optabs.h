/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

struct optab GTY(())
{
  enum rtx_code code;
  struct optab_handlers {
    enum insn_code insn_code;
    rtx libfunc;
  } handlers [NUM_MACHINE_MODES];
};
typedef struct optab * optab;

/* Given an enum insn_code, access the function to construct
   the body of that kind of insn.  */
#define GEN_FCN(CODE) (*insn_data[(int) (CODE)].genfun)

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
  /* Find first bit set */
  OTI_ffs,
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

  /* Compare insn; two operands.  */
  OTI_cmp,
  /* Used only for libcalls for unsigned comparisons.  */
  OTI_ucmp,
  /* tst insn; compare one operand against 0 */
  OTI_tst,

  /* String length */
  OTI_strlen,

  /* Combined compare & jump/store flags/move operations.  */
  OTI_cbranch,
  OTI_cmov,
  OTI_cstore,
    
  /* Push instruction.  */
  OTI_push,

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

#define mov_optab (optab_table[OTI_mov])
#define movstrict_optab (optab_table[OTI_movstrict])

#define neg_optab (optab_table[OTI_neg])
#define negv_optab (optab_table[OTI_negv])
#define abs_optab (optab_table[OTI_abs])
#define absv_optab (optab_table[OTI_absv])
#define one_cmpl_optab (optab_table[OTI_one_cmpl])
#define ffs_optab (optab_table[OTI_ffs])
#define sqrt_optab (optab_table[OTI_sqrt])
#define sin_optab (optab_table[OTI_sin])
#define cos_optab (optab_table[OTI_cos])
#define exp_optab (optab_table[OTI_exp])
#define log_optab (optab_table[OTI_log])

#define cmp_optab (optab_table[OTI_cmp])
#define ucmp_optab (optab_table[OTI_ucmp])
#define tst_optab (optab_table[OTI_tst])

#define strlen_optab (optab_table[OTI_strlen])

#define cbranch_optab (optab_table[OTI_cbranch])
#define cmov_optab (optab_table[OTI_cmov])
#define cstore_optab (optab_table[OTI_cstore])
#define push_optab (optab_table[OTI_push])

/* Tables of patterns for extending one integer mode to another.  */
extern enum insn_code extendtab[MAX_MACHINE_MODE][MAX_MACHINE_MODE][2];

/* Tables of patterns for converting between fixed and floating point.  */
extern enum insn_code fixtab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
extern enum insn_code fixtrunctab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
extern enum insn_code floattab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];

/* These arrays record the insn_code of insns that may be needed to
   perform input and output reloads of special objects.  They provide a
   place to pass a scratch register.  */
extern enum insn_code reload_in_optab[NUM_MACHINE_MODES];
extern enum insn_code reload_out_optab[NUM_MACHINE_MODES];

/* Contains the optab used for each rtx code.  */
extern optab code_to_optab[NUM_RTX_CODE + 1];


typedef rtx (*rtxfun) PARAMS ((rtx));

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

/* Define functions given in optabs.c.  */

/* Expand a binary operation given optab and rtx operands.  */
extern rtx expand_binop PARAMS ((enum machine_mode, optab, rtx, rtx, rtx,
				 int, enum optab_methods));

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop PARAMS ((enum machine_mode, optab, optab, rtx,
				      rtx, rtx, int, enum optab_methods));

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop PARAMS ((optab, rtx, rtx, rtx, rtx, int));

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop PARAMS ((enum machine_mode, optab, rtx, rtx, int));

/* Expand the absolute value operation.  */
extern rtx expand_abs PARAMS ((enum machine_mode, rtx, rtx, int, int));

/* Expand the complex absolute value operation.  */
extern rtx expand_complex_abs PARAMS ((enum machine_mode, rtx, rtx, int));

/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern void emit_unop_insn PARAMS ((int, rtx, rtx, enum rtx_code));

/* Emit code to perform a series of operations on a multi-word quantity, one
   word at a time.  */
extern rtx emit_no_conflict_block PARAMS ((rtx, rtx, rtx, rtx, rtx));

/* Emit one rtl instruction to store zero in specified rtx.  */
extern void emit_clr_insn PARAMS ((rtx));

/* Emit one rtl insn to store 1 in specified rtx assuming it contains 0.  */
extern void emit_0_to_1_insn PARAMS ((rtx));

/* Emit one rtl insn to compare two rtx's.  */
extern void emit_cmp_insn PARAMS ((rtx, rtx, enum rtx_code, rtx,
				   enum machine_mode, int));

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
extern int can_compare_p PARAMS ((enum rtx_code, enum machine_mode,
				  enum can_compare_purpose));

extern rtx prepare_operand PARAMS ((int, rtx, int, enum machine_mode,
				    enum machine_mode, int));

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p PARAMS ((enum machine_mode,
					    enum machine_mode, int));

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn PARAMS ((rtx, rtx, enum machine_mode,
				    enum machine_mode, int));

/* Initialize the tables that control conversion between fixed and
   floating values.  */
extern void init_fixtab PARAMS ((void));
extern void init_floattab PARAMS ((void));

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float PARAMS ((rtx, rtx, int));

/* Generate code for a FIX_EXPR.  */
extern void expand_fix PARAMS ((rtx, rtx, int));

#endif /* GCC_OPTABS_H */
