/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001-2025 Free Software Foundation, Inc.

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

#include "optabs-query.h"
#include "optabs-libfuncs.h"
#include "vec-perm-indices.h"

/* Generate code for a widening multiply.  */
extern rtx expand_widening_mult (machine_mode, rtx, rtx, rtx, int, optab);

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
  EXPAND_INTEGER,
  EXPAND_UNDEFINED_INPUT
};

/* Information about an operand for instruction expansion.  */
class expand_operand {
public:
  /* The type of operand.  */
  ENUM_BITFIELD (expand_operand_type) type : 8;

  /* True if any conversion should treat VALUE as being unsigned
     rather than signed.  Only meaningful for certain types.  */
  unsigned int unsigned_p : 1;

  /* Is the target operand.  */
  unsigned int target : 1;

  /* Unused; available for future use.  */
  unsigned int unused : 6;

  /* The mode passed to the convert_*_operand function.  It has a
     type-dependent meaning.  */
  ENUM_BITFIELD (machine_mode) mode : 16;

  /* The value of the operand.  */
  rtx value;

  /* The value of an EXPAND_INTEGER operand.  */
  poly_int64 int_value;
};

/* Initialize OP with the given fields.  Initialise the other fields
   to their default values.  */

inline void
create_expand_operand (class expand_operand *op,
		       enum expand_operand_type type,
		       rtx value, machine_mode mode,
		       bool unsigned_p, poly_int64 int_value = 0)
{
  op->type = type;
  op->unsigned_p = unsigned_p;
  op->target = 0;
  op->unused = 0;
  op->mode = mode;
  op->value = value;
  op->int_value = int_value;
}

/* Make OP describe an operand that must use rtx X, even if X is volatile.  */

inline void
create_fixed_operand (class expand_operand *op, rtx x)
{
  create_expand_operand (op, EXPAND_FIXED, x, VOIDmode, false);
}

/* Make OP describe an output operand that must have mode MODE.
   X, if nonnull, is a suggestion for where the output should be stored.
   It is OK for VALUE to be inconsistent with MODE, although it will just
   be ignored in that case.  */

inline void
create_output_operand (class expand_operand *op, rtx x,
		       machine_mode mode)
{
  create_expand_operand (op, EXPAND_OUTPUT, x, mode, false);
}

/* Make OP describe an input operand that must have mode MODE and
   value VALUE; MODE cannot be VOIDmode.  The backend may request that
   VALUE be copied into a different kind of rtx before being passed
   as an operand.  */

inline void
create_input_operand (class expand_operand *op, rtx value,
		      machine_mode mode)
{
  create_expand_operand (op, EXPAND_INPUT, value, mode, false);
}

/* Make OP describe an undefined input operand of mode MODE.  MODE cannot
   be null.  */

inline void
create_undefined_input_operand (class expand_operand *op, machine_mode mode)
{
  create_expand_operand (op, EXPAND_UNDEFINED_INPUT, gen_rtx_SCRATCH (mode),
			 mode, false);
}

/* Like create_input_operand, except that VALUE must first be converted
   to mode MODE.  UNSIGNED_P says whether VALUE is unsigned.  */

inline void
create_convert_operand_to (class expand_operand *op, rtx value,
			   machine_mode mode, bool unsigned_p)
{
  create_expand_operand (op, EXPAND_CONVERT_TO, value, mode, unsigned_p);
}

/* Make OP describe an input operand that should have the same value
   as VALUE, after any mode conversion that the backend might request.
   If VALUE is a CONST_INT, it should be treated as having mode MODE.
   UNSIGNED_P says whether VALUE is unsigned.

   The conversion of VALUE can include a combination of numerical
   conversion (as for convert_modes) and duplicating a scalar to fill
   a vector (if VALUE is a scalar but the operand is a vector).  */

inline void
create_convert_operand_from (class expand_operand *op, rtx value,
			     machine_mode mode, bool unsigned_p)
{
  create_expand_operand (op, EXPAND_CONVERT_FROM, value, mode, unsigned_p);
}


/* Make OP describe an input Pmode address operand.  VALUE is the value
   of the address, but it may need to be converted to Pmode first.  */

inline void
create_address_operand (class expand_operand *op, rtx value)
{
  create_expand_operand (op, EXPAND_ADDRESS, value, Pmode, false);
}

extern void create_integer_operand (class expand_operand *, poly_int64);

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

extern rtx expand_widen_pattern_expr (const struct separate_ops *, rtx , rtx , rtx,
                                      rtx, int);
extern rtx expand_ternary_op (machine_mode mode, optab ternary_optab,
			      rtx op0, rtx op1, rtx op2, rtx target,
			      int unsignedp);
extern rtx simplify_expand_binop (machine_mode mode, optab binoptab,
				  rtx op0, rtx op1, rtx target, int unsignedp,
				  enum optab_methods methods);
extern bool force_expand_binop (machine_mode, optab, rtx, rtx, rtx, int,
				enum optab_methods);
extern rtx expand_vector_broadcast (machine_mode, rtx);

extern rtx expand_doubleword_divmod (machine_mode, rtx, rtx, rtx *, bool);

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
extern bool expand_twoval_unop (optab, rtx, rtx, rtx, int);

/* Generate code to perform an operation on two operands with two results.  */
extern bool expand_twoval_binop (optab, rtx, rtx, rtx, rtx, int);

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
extern void emit_libcall_block (rtx_insn *, rtx, rtx, rtx);

/* The various uses that a comparison can have; used by can_compare_p:
   jumps, store flag operations.  */
enum can_compare_purpose
{
  ccp_jump,
  ccp_store_flag
};

/* Nonzero if a compare of mode MODE can be done straightforwardly
   (without splitting it into pieces).  */
extern bool can_compare_p (enum rtx_code, machine_mode,
			   enum can_compare_purpose);

/* Return whether the backend can emit a vector comparison (vec_cmp/vec_cmpu)
   for code CODE, comparing operands of mode VALUE_MODE and producing a result
   with MASK_MODE.  */
extern bool can_vec_cmp_compare_p (enum rtx_code, machine_mode, machine_mode);

/* Return whether the backend can emit vector set instructions for inserting
   element into vector at variable index position.  */
extern bool can_vec_set_var_idx_p (machine_mode);
extern bool can_vec_extract_var_idx_p (machine_mode, machine_mode);

extern rtx prepare_operand (enum insn_code, rtx, int, machine_mode,
			    machine_mode, int);
/* Emit a pair of rtl insns to compare two rtx's and to jump
   to a label if the comparison is true.  */
extern void emit_cmp_and_jump_insns (rtx, rtx, enum rtx_code, rtx,
				     machine_mode, int, rtx,
				     profile_probability prob
					= profile_probability::uninitialized ());
extern void emit_cmp_and_jump_insns (rtx, rtx, enum rtx_code, rtx,
				     machine_mode, int, tree, rtx,
				     profile_probability prob
					= profile_probability::uninitialized ());

/* Generate code to indirectly jump to a location given in the rtx LOC.  */
extern void emit_indirect_jump (rtx);

#include "insn-config.h"

#ifndef GCC_INSN_CONFIG_H
#error "insn-config.h must be included before optabs.h"
#endif

/* Emit a conditional move operation.  */
rtx emit_conditional_move (rtx, rtx_comparison, rtx, rtx, machine_mode, int);
rtx emit_conditional_move (rtx, rtx, rtx, rtx, rtx, machine_mode);

/* Emit a conditional negate or bitwise complement operation.  */
rtx emit_conditional_neg_or_complement (rtx, rtx_code, machine_mode, rtx,
					 rtx, rtx);

rtx emit_conditional_add (rtx, enum rtx_code, rtx, rtx, machine_mode,
			  rtx, rtx, machine_mode, int);

/* Create but don't emit one rtl instruction to perform certain operations.
   Modes must match; operands must meet the operation's predicates.
   Likewise for subtraction and for just copying.  */
extern rtx_insn *gen_add2_insn (rtx, rtx);
extern rtx_insn *gen_add3_insn (rtx, rtx, rtx);
extern bool have_add2_insn (rtx, rtx);
extern rtx_insn *gen_addptr3_insn (rtx, rtx, rtx);
extern bool have_addptr3_insn (rtx, rtx, rtx);
extern rtx_insn *gen_sub2_insn (rtx, rtx);
extern rtx_insn *gen_sub3_insn (rtx, rtx, rtx);
extern bool have_sub2_insn (rtx, rtx);

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx_insn *gen_extend_insn (rtx, rtx, machine_mode, machine_mode, int);

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
extern bool have_insn_for (enum rtx_code, machine_mode);

/* Generate a conditional trap instruction.  */
extern rtx_insn *gen_cond_trap (enum rtx_code, rtx, rtx, rtx);

/* Check whether the vec_perm can be interpreted as an and operation.  */
extern rtx vec_perm_and_mask (machine_mode mode, const vec_perm_indices &sel,
			      bool zero_op0_p);

/* Generate code for VEC_PERM_EXPR.  */
extern rtx expand_vec_perm_var (machine_mode, rtx, rtx, rtx, rtx);
extern rtx expand_vec_perm_const (machine_mode, rtx, rtx,
				  const vec_perm_builder &, machine_mode, rtx);

/* Generate code for vector comparison.  */
extern rtx expand_vec_cmp_expr (tree, tree, rtx);

/* Generate code for VEC_SERIES_EXPR.  */
extern rtx expand_vec_series_expr (machine_mode, rtx, rtx, rtx);

/* Generate code for MULT_HIGHPART_EXPR.  */
extern rtx expand_mult_highpart (machine_mode, rtx, rtx, rtx, bool);

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

extern void expand_asm_reg_clobber_mem_blockage (HARD_REG_SET);

extern bool insn_operand_matches (enum insn_code icode, unsigned int opno,
				  rtx operand);
extern bool valid_multiword_target_p (rtx);
extern void create_convert_operand_from_type (class expand_operand *op,
					      rtx value, tree type);
extern bool maybe_legitimize_operands (enum insn_code icode,
				       unsigned int opno, unsigned int nops,
				       class expand_operand *ops);
extern rtx_insn *maybe_gen_insn (enum insn_code icode, unsigned int nops,
				 class expand_operand *ops);
extern bool maybe_expand_insn (enum insn_code icode, unsigned int nops,
			       class expand_operand *ops);
extern bool maybe_expand_jump_insn (enum insn_code icode, unsigned int nops,
				    class expand_operand *ops);
extern void expand_insn (enum insn_code icode, unsigned int nops,
			 class expand_operand *ops);
extern void expand_jump_insn (enum insn_code icode, unsigned int nops,
			      class expand_operand *ops);

extern enum rtx_code get_rtx_code_1 (enum tree_code tcode, bool unsignedp);
extern enum rtx_code get_rtx_code (enum tree_code tcode, bool unsignedp);
extern rtx vector_compare_rtx (machine_mode cmp_mode, enum tree_code tcode,
			       tree t_op0, tree t_op1, bool unsignedp,
			       enum insn_code icode, unsigned int opno);


#endif /* GCC_OPTABS_H */
