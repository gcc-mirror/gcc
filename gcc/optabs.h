/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001-2015 Free Software Foundation, Inc.

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

#include "insn-opinit.h"

/* Generate code for a widening multiply.  */
extern rtx expand_widening_mult (machine_mode, rtx, rtx, rtx, int, optab);

/* Return the insn used to implement mode MODE of OP, or CODE_FOR_nothing
   if the target does not have such an insn.  */

static inline enum insn_code
optab_handler (optab op, machine_mode mode)
{
  unsigned scode = (op << 16) | mode;
  gcc_assert (op > LAST_CONV_OPTAB);
  return raw_optab_handler (scode);
}

/* Return the insn used to perform conversion OP from mode FROM_MODE
   to mode TO_MODE; return CODE_FOR_nothing if the target does not have
   such an insn.  */

static inline enum insn_code
convert_optab_handler (convert_optab op, machine_mode to_mode,
		       machine_mode from_mode)
{
  unsigned scode = (op << 16) | (from_mode << 8) | to_mode;
  gcc_assert (op > unknown_optab && op <= LAST_CONV_OPTAB);
  return raw_optab_handler (scode);
}

/* Return the insn used to implement mode MODE of OP, or CODE_FOR_nothing
   if the target does not have such an insn.  */

static inline enum insn_code
direct_optab_handler (direct_optab op, machine_mode mode)
{
  return optab_handler (op, mode);
}

/* Return true if UNOPTAB is for a trapping-on-overflow operation.  */

static inline bool
trapv_unoptab_p (optab unoptab)
{
  return (unoptab == negv_optab
	  || unoptab == absv_optab); 
}

/* Return true if BINOPTAB is for a trapping-on-overflow operation.  */

static inline bool
trapv_binoptab_p (optab binoptab)
{
  return (binoptab == addv_optab
	  || binoptab == subv_optab
	  || binoptab == smulv_optab);
}



/* Describes an instruction that inserts or extracts a bitfield.  */
struct extraction_insn
{
  /* The code of the instruction.  */
  enum insn_code icode;

  /* The mode that the structure operand should have.  This is byte_mode
     when using the legacy insv, extv and extzv patterns to access memory.  */
  machine_mode struct_mode;

  /* The mode of the field to be inserted or extracted, and by extension
     the mode of the insertion or extraction itself.  */
  machine_mode field_mode;

  /* The mode of the field's bit position.  This is only important
     when the position is variable rather than constant.  */
  machine_mode pos_mode;
};




/* Describes the type of an expand_operand.  Each value is associated
   with a create_*_operand function; see the comments above those
   functions for details.  */
enum expand_operand_type {
  EXPAND_FIXED,
  EXPAND_OUTPUT,
  EXPAND_INPUT,
  EXPAND_CONVERT_TO,
  EXPAND_CONVERT_FROM,
  EXPAND_ADDRESS,
  EXPAND_INTEGER
};

/* Information about an operand for instruction expansion.  */
struct expand_operand {
  /* The type of operand.  */
  ENUM_BITFIELD (expand_operand_type) type : 8;

  /* True if any conversion should treat VALUE as being unsigned
     rather than signed.  Only meaningful for certain types.  */
  unsigned int unsigned_p : 1;

  /* Unused; available for future use.  */
  unsigned int unused : 7;

  /* The mode passed to the convert_*_operand function.  It has a
     type-dependent meaning.  */
  ENUM_BITFIELD (machine_mode) mode : 16;

  /* The value of the operand.  */
  rtx value;
};

/* Initialize OP with the given fields.  Initialise the other fields
   to their default values.  */

static inline void
create_expand_operand (struct expand_operand *op,
		       enum expand_operand_type type,
		       rtx value, machine_mode mode,
		       bool unsigned_p)
{
  op->type = type;
  op->unsigned_p = unsigned_p;
  op->unused = 0;
  op->mode = mode;
  op->value = value;
}

/* Make OP describe an operand that must use rtx X, even if X is volatile.  */

static inline void
create_fixed_operand (struct expand_operand *op, rtx x)
{
  create_expand_operand (op, EXPAND_FIXED, x, VOIDmode, false);
}

/* Make OP describe an output operand that must have mode MODE.
   X, if nonnull, is a suggestion for where the output should be stored.
   It is OK for VALUE to be inconsistent with MODE, although it will just
   be ignored in that case.  */

static inline void
create_output_operand (struct expand_operand *op, rtx x,
		       machine_mode mode)
{
  create_expand_operand (op, EXPAND_OUTPUT, x, mode, false);
}

/* Make OP describe an input operand that must have mode MODE and
   value VALUE; MODE cannot be VOIDmode.  The backend may request that
   VALUE be copied into a different kind of rtx before being passed
   as an operand.  */

static inline void
create_input_operand (struct expand_operand *op, rtx value,
		      machine_mode mode)
{
  create_expand_operand (op, EXPAND_INPUT, value, mode, false);
}

/* Like create_input_operand, except that VALUE must first be converted
   to mode MODE.  UNSIGNED_P says whether VALUE is unsigned.  */

static inline void
create_convert_operand_to (struct expand_operand *op, rtx value,
			   machine_mode mode, bool unsigned_p)
{
  create_expand_operand (op, EXPAND_CONVERT_TO, value, mode, unsigned_p);
}

/* Make OP describe an input operand that should have the same value
   as VALUE, after any mode conversion that the backend might request.
   If VALUE is a CONST_INT, it should be treated as having mode MODE.
   UNSIGNED_P says whether VALUE is unsigned.  */

static inline void
create_convert_operand_from (struct expand_operand *op, rtx value,
			     machine_mode mode, bool unsigned_p)
{
  create_expand_operand (op, EXPAND_CONVERT_FROM, value, mode, unsigned_p);
}


/* Make OP describe an input Pmode address operand.  VALUE is the value
   of the address, but it may need to be converted to Pmode first.  */

static inline void
create_address_operand (struct expand_operand *op, rtx value)
{
  create_expand_operand (op, EXPAND_ADDRESS, value, Pmode, false);
}

/* Make OP describe an input operand that has value INTVAL and that has
   no inherent mode.  This function should only be used for operands that
   are always expand-time constants.  The backend may request that INTVAL
   be copied into a different kind of rtx, but it must specify the mode
   of that rtx if so.  */

static inline void
create_integer_operand (struct expand_operand *op, HOST_WIDE_INT intval)
{
  create_expand_operand (op, EXPAND_INTEGER, GEN_INT (intval), VOIDmode, false);
}


extern rtx convert_optab_libfunc (convert_optab optab, machine_mode mode1,
			          machine_mode mode2);
extern rtx optab_libfunc (optab optab, machine_mode mode);
extern enum insn_code widening_optab_handler (optab, machine_mode,
					      machine_mode);
/* Find a widening optab even if it doesn't widen as much as we want.  */
#define find_widening_optab_handler(A,B,C,D) \
  find_widening_optab_handler_and_mode (A, B, C, D, NULL)
extern enum insn_code find_widening_optab_handler_and_mode (optab,
							    machine_mode,
							    machine_mode,
							    int,
							    machine_mode *);

/* An extra flag to control optab_for_tree_code's behavior.  This is needed to
   distinguish between machines with a vector shift that takes a scalar for the
   shift amount vs. machines that take a vector for the shift amount.  */
enum optab_subtype
{
  optab_default,
  optab_scalar,
  optab_vector
};

/* Passed to expand_simple_binop and expand_binop to say which options
   to try to use if the requested operation can't be open-coded on the
   requisite mode.  Either OPTAB_LIB or OPTAB_LIB_WIDEN says try using
   a library call.  Either OPTAB_WIDEN or OPTAB_LIB_WIDEN says try
   using a wider mode.  OPTAB_MUST_WIDEN says try widening and don't
   try anything else.  */

enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};

/* Return the optab used for computing the given operation on the type given by
   the second argument.  The third argument distinguishes between the types of
   vector shifts and rotates */
extern optab optab_for_tree_code (enum tree_code, const_tree, enum optab_subtype);

/* Given an optab that reduces a vector to a scalar, find instead the old
   optab that produces a vector with the reduction result in one element,
   for a tree with the specified type.  */
extern optab scalar_reduc_to_vector (optab, const_tree type);

extern rtx expand_widen_pattern_expr (struct separate_ops *, rtx , rtx , rtx,
                                      rtx, int);
extern rtx expand_ternary_op (machine_mode mode, optab ternary_optab,
			      rtx op0, rtx op1, rtx op2, rtx target,
			      int unsignedp);
extern rtx simplify_expand_binop (machine_mode mode, optab binoptab,
				  rtx op0, rtx op1, rtx target, int unsignedp,
				  enum optab_methods methods);
extern bool force_expand_binop (machine_mode, optab, rtx, rtx, rtx, int,
				enum optab_methods);

/* Generate code for a simple binary or unary operation.  "Simple" in
   this case means "can be unambiguously described by a (mode, code)
   pair and mapped to a single optab."  */
extern rtx expand_simple_binop (machine_mode, enum rtx_code, rtx,
				rtx, rtx, int, enum optab_methods);

/* Expand a binary operation given optab and rtx operands.  */
extern rtx expand_binop (machine_mode, optab, rtx, rtx, rtx, int,
			 enum optab_methods);

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop (machine_mode, optab, optab, rtx, rtx,
			      rtx, int, enum optab_methods);

/* Generate code to perform an operation on one operand with two results.  */
extern int expand_twoval_unop (optab, rtx, rtx, rtx, int);

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop (optab, rtx, rtx, rtx, rtx, int);

/* Generate code to perform an operation on two operands with two
   results, using a library function.  */
extern bool expand_twoval_binop_libfunc (optab, rtx, rtx, rtx, rtx,
					 enum rtx_code);
extern rtx expand_simple_unop (machine_mode, enum rtx_code, rtx, rtx,
			       int);

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop (machine_mode, optab, rtx, rtx, int);

/* Expand the absolute value operation.  */
extern rtx expand_abs_nojump (machine_mode, rtx, rtx, int);
extern rtx expand_abs (machine_mode, rtx, rtx, int, int);

/* Expand the one's complement absolute value operation.  */
extern rtx expand_one_cmpl_abs_nojump (machine_mode, rtx, rtx);

/* Expand the copysign operation.  */
extern rtx expand_copysign (rtx, rtx, rtx);
/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern bool maybe_emit_unop_insn (enum insn_code, rtx, rtx, enum rtx_code);
extern void emit_unop_insn (enum insn_code, rtx, rtx, enum rtx_code);

/* Emit code to make a call to a constant function or a library call.  */
extern void emit_libcall_block (rtx, rtx, rtx, rtx);

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
extern int can_compare_p (enum rtx_code, machine_mode,
			  enum can_compare_purpose);
extern rtx prepare_operand (enum insn_code, rtx, int, machine_mode,
			    machine_mode, int);
/* Emit a pair of rtl insns to compare two rtx's and to jump
   to a label if the comparison is true.  */
extern void emit_cmp_and_jump_insns (rtx, rtx, enum rtx_code, rtx,
				     machine_mode, int, rtx, int prob=-1);

/* Generate code to indirectly jump to a location given in the rtx LOC.  */
extern void emit_indirect_jump (rtx);

#include "insn-config.h"

#ifndef GCC_INSN_CONFIG_H
#error "insn-config.h must be included before optabs.h"
#endif

#ifdef HAVE_conditional_move
/* Emit a conditional move operation.  */
rtx emit_conditional_move (rtx, enum rtx_code, rtx, rtx, machine_mode,
			   rtx, rtx, machine_mode, int);

/* Return nonzero if the conditional move is supported.  */
int can_conditionally_move_p (machine_mode mode);

#endif
rtx emit_conditional_add (rtx, enum rtx_code, rtx, rtx, machine_mode,
			  rtx, rtx, machine_mode, int);

/* Create but don't emit one rtl instruction to perform certain operations.
   Modes must match; operands must meet the operation's predicates.
   Likewise for subtraction and for just copying.  */
extern rtx gen_add2_insn (rtx, rtx);
extern rtx gen_add3_insn (rtx, rtx, rtx);
extern int have_add2_insn (rtx, rtx);
extern rtx gen_addptr3_insn (rtx, rtx, rtx);
extern int have_addptr3_insn (rtx, rtx, rtx);
extern rtx gen_sub2_insn (rtx, rtx);
extern rtx gen_sub3_insn (rtx, rtx, rtx);
extern int have_sub2_insn (rtx, rtx);

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p (machine_mode, machine_mode, int);

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn (rtx, rtx, machine_mode,
			    machine_mode, int);

/* Return the insn_code for a FLOAT_EXPR.  */
enum insn_code can_float_p (machine_mode, machine_mode, int);

/* Check whether an operation represented by the code CODE is a
   convert operation that is supported by the target platform in
   vector form */
bool supportable_convert_operation (enum tree_code, tree, tree, tree *, 
                                    enum tree_code *);

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float (rtx, rtx, int);

/* Generate code for a FIX_EXPR.  */
extern void expand_fix (rtx, rtx, int);

/* Generate code for a FIXED_CONVERT_EXPR.  */
extern void expand_fixed_convert (rtx, rtx, int, int);

/* Generate code for float to integral conversion.  */
extern bool expand_sfix_optab (rtx, rtx, convert_optab);

/* Report whether the machine description contains an insn which can
   perform the operation described by CODE and MODE.  */
extern int have_insn_for (enum rtx_code, machine_mode);

extern void gen_int_libfunc (optab, const char *, char, machine_mode);
extern void gen_fp_libfunc (optab, const char *, char, machine_mode);
extern void gen_fixed_libfunc (optab, const char *, char, machine_mode);
extern void gen_signed_fixed_libfunc (optab, const char *, char,
				      machine_mode);
extern void gen_unsigned_fixed_libfunc (optab, const char *, char,
					machine_mode);
extern void gen_int_fp_libfunc (optab, const char *, char, machine_mode);
extern void gen_intv_fp_libfunc (optab, const char *, char, machine_mode);
extern void gen_int_fp_fixed_libfunc (optab, const char *, char,
				      machine_mode);
extern void gen_int_fp_signed_fixed_libfunc (optab, const char *, char,
					     machine_mode);
extern void gen_int_fixed_libfunc (optab, const char *, char,
				   machine_mode);
extern void gen_int_signed_fixed_libfunc (optab, const char *, char,
					  machine_mode);
extern void gen_int_unsigned_fixed_libfunc (optab, const char *, char,
					    machine_mode);

extern void gen_interclass_conv_libfunc (convert_optab, const char *,
					 machine_mode, machine_mode);
extern void gen_int_to_fp_conv_libfunc (convert_optab, const char *,
					machine_mode, machine_mode);
extern void gen_ufloat_conv_libfunc (convert_optab, const char *,
				     machine_mode, machine_mode);
extern void gen_int_to_fp_nondecimal_conv_libfunc  (convert_optab,
						    const char *,
						    machine_mode,
						    machine_mode);
extern void gen_fp_to_int_conv_libfunc (convert_optab, const char *,
					machine_mode, machine_mode);
extern void gen_intraclass_conv_libfunc (convert_optab, const char *,
					 machine_mode, machine_mode);
extern void gen_trunc_conv_libfunc (convert_optab, const char *,
				    machine_mode, machine_mode);
extern void gen_extend_conv_libfunc (convert_optab, const char *,
				     machine_mode, machine_mode);
extern void gen_fract_conv_libfunc (convert_optab, const char *,
				    machine_mode, machine_mode);
extern void gen_fractuns_conv_libfunc (convert_optab, const char *,
				       machine_mode, machine_mode);
extern void gen_satfract_conv_libfunc (convert_optab, const char *,
				       machine_mode, machine_mode);
extern void gen_satfractuns_conv_libfunc (convert_optab, const char *,
					  machine_mode,
					  machine_mode);

/* Build a decl for a libfunc named NAME. */
extern tree build_libfunc_function (const char *);

/* Call this to initialize an optab function entry.  */
extern rtx init_one_libfunc (const char *);
extern rtx set_user_assembler_libfunc (const char *, const char *);

/* Call this to reset the function entry for one optab.  */
extern void set_optab_libfunc (optab, machine_mode, const char *);
extern void set_conv_libfunc (convert_optab, machine_mode,
			      machine_mode, const char *);

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */
extern void init_optabs (void);
extern void init_tree_optimization_optabs (tree);

/* Call this to install all of the __sync libcalls up to size MAX.  */
extern void init_sync_libfuncs (int max);

/* Generate a conditional trap instruction.  */
extern rtx gen_cond_trap (enum rtx_code, rtx, rtx, rtx);

/* Return true if target supports vector operations for VEC_PERM_EXPR.  */
extern bool can_vec_perm_p (machine_mode, bool, const unsigned char *);

/* Generate code for VEC_PERM_EXPR.  */
extern rtx expand_vec_perm (machine_mode, rtx, rtx, rtx, rtx);

/* Return tree if target supports vector operations for COND_EXPR.  */
bool expand_vec_cond_expr_p (tree, tree);

/* Generate code for VEC_COND_EXPR.  */
extern rtx expand_vec_cond_expr (tree, tree, tree, tree, rtx);

/* Return non-zero if target supports a given highpart multiplication.  */
extern int can_mult_highpart_p (machine_mode, bool);

/* Generate code for MULT_HIGHPART_EXPR.  */
extern rtx expand_mult_highpart (machine_mode, rtx, rtx, rtx, bool);

/* Return true if target supports vector masked load/store for mode.  */
extern bool can_vec_mask_load_store_p (machine_mode, bool);

/* Return true if there is an inline compare and swap pattern.  */
extern bool can_compare_and_swap_p (machine_mode, bool);

/* Return true if there is an inline atomic exchange pattern.  */
extern bool can_atomic_exchange_p (machine_mode, bool);

extern rtx expand_sync_lock_test_and_set (rtx, rtx, rtx);
extern rtx expand_atomic_test_and_set (rtx, rtx, enum memmodel);
extern rtx expand_atomic_exchange (rtx, rtx, rtx, enum memmodel);
extern bool expand_atomic_compare_and_swap (rtx *, rtx *, rtx, rtx, rtx, bool,
					    enum memmodel, enum memmodel);
/* Generate memory barriers.  */
extern void expand_mem_thread_fence (enum memmodel);
extern void expand_mem_signal_fence (enum memmodel);

rtx expand_atomic_load (rtx, rtx, enum memmodel);
rtx expand_atomic_store (rtx, rtx, enum memmodel, bool);
rtx expand_atomic_fetch_op (rtx, rtx, rtx, enum rtx_code, enum memmodel, 
			      bool);

extern bool insn_operand_matches (enum insn_code icode, unsigned int opno,
				  rtx operand);
extern bool valid_multiword_target_p (rtx);
extern void create_convert_operand_from_type (struct expand_operand *op,
					      rtx value, tree type);
extern bool maybe_legitimize_operands (enum insn_code icode,
				       unsigned int opno, unsigned int nops,
				       struct expand_operand *ops);
extern rtx maybe_gen_insn (enum insn_code icode, unsigned int nops,
			   struct expand_operand *ops);
extern bool maybe_expand_insn (enum insn_code icode, unsigned int nops,
			       struct expand_operand *ops);
extern bool maybe_expand_jump_insn (enum insn_code icode, unsigned int nops,
				    struct expand_operand *ops);
extern void expand_insn (enum insn_code icode, unsigned int nops,
			 struct expand_operand *ops);
extern void expand_jump_insn (enum insn_code icode, unsigned int nops,
			      struct expand_operand *ops);

/* Enumerates the possible extraction_insn operations.  */
enum extraction_pattern { EP_insv, EP_extv, EP_extzv };

extern bool get_best_reg_extraction_insn (extraction_insn *,
					  enum extraction_pattern,
					  unsigned HOST_WIDE_INT,
					  machine_mode);
extern bool get_best_mem_extraction_insn (extraction_insn *,
					  enum extraction_pattern,
					  HOST_WIDE_INT, HOST_WIDE_INT,
					  machine_mode);

extern bool lshift_cheap_p (bool);

extern enum rtx_code get_rtx_code (enum tree_code tcode, bool unsignedp);

#endif /* GCC_OPTABS_H */
