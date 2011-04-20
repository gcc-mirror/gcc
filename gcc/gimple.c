/* Gimple IR support functions.

   Copyright 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "tree.h"
#include "ggc.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "value-prof.h"
#include "flags.h"
#include "alias.h"
#include "demangle.h"
#include "langhooks.h"

/* Global type table.  FIXME lto, it should be possible to re-use some
   of the type hashing routines in tree.c (type_hash_canon, type_hash_lookup,
   etc), but those assume that types were built with the various
   build_*_type routines which is not the case with the streamer.  */
static GTY((if_marked ("ggc_marked_p"), param_is (union tree_node)))
  htab_t gimple_types;
static GTY((if_marked ("ggc_marked_p"), param_is (union tree_node)))
  htab_t gimple_canonical_types;
static GTY((if_marked ("tree_int_map_marked_p"), param_is (struct tree_int_map)))
  htab_t type_hash_cache;
static GTY((if_marked ("tree_int_map_marked_p"), param_is (struct tree_int_map)))
  htab_t canonical_type_hash_cache;

/* Global type comparison cache.  This is by TYPE_UID for space efficiency
   and thus cannot use and does not need GC.  */
static htab_t gtc_visited;
static struct obstack gtc_ob;

/* All the tuples have their operand vector (if present) at the very bottom
   of the structure.  Therefore, the offset required to find the
   operands vector the size of the structure minus the size of the 1
   element tree array at the end (see gimple_ops).  */
#define DEFGSSTRUCT(SYM, STRUCT, HAS_TREE_OP) \
	(HAS_TREE_OP ? sizeof (struct STRUCT) - sizeof (tree) : 0),
EXPORTED_CONST size_t gimple_ops_offset_[] = {
#include "gsstruct.def"
};
#undef DEFGSSTRUCT

#define DEFGSSTRUCT(SYM, STRUCT, HAS_TREE_OP) sizeof(struct STRUCT),
static const size_t gsstruct_code_size[] = {
#include "gsstruct.def"
};
#undef DEFGSSTRUCT

#define DEFGSCODE(SYM, NAME, GSSCODE)	NAME,
const char *const gimple_code_name[] = {
#include "gimple.def"
};
#undef DEFGSCODE

#define DEFGSCODE(SYM, NAME, GSSCODE)	GSSCODE,
EXPORTED_CONST enum gimple_statement_structure_enum gss_for_code_[] = {
#include "gimple.def"
};
#undef DEFGSCODE

#ifdef GATHER_STATISTICS
/* Gimple stats.  */

int gimple_alloc_counts[(int) gimple_alloc_kind_all];
int gimple_alloc_sizes[(int) gimple_alloc_kind_all];

/* Keep in sync with gimple.h:enum gimple_alloc_kind.  */
static const char * const gimple_alloc_kind_names[] = {
    "assignments",
    "phi nodes",
    "conditionals",
    "sequences",
    "everything else"
};

#endif /* GATHER_STATISTICS */

/* A cache of gimple_seq objects.  Sequences are created and destroyed
   fairly often during gimplification.  */
static GTY ((deletable)) struct gimple_seq_d *gimple_seq_cache;

/* Private API manipulation functions shared only with some
   other files.  */
extern void gimple_set_stored_syms (gimple, bitmap, bitmap_obstack *);
extern void gimple_set_loaded_syms (gimple, bitmap, bitmap_obstack *);

/* Gimple tuple constructors.
   Note: Any constructor taking a ``gimple_seq'' as a parameter, can
   be passed a NULL to start with an empty sequence.  */

/* Set the code for statement G to CODE.  */

static inline void
gimple_set_code (gimple g, enum gimple_code code)
{
  g->gsbase.code = code;
}

/* Return the number of bytes needed to hold a GIMPLE statement with
   code CODE.  */

static inline size_t
gimple_size (enum gimple_code code)
{
  return gsstruct_code_size[gss_for_code (code)];
}

/* Allocate memory for a GIMPLE statement with code CODE and NUM_OPS
   operands.  */

gimple
gimple_alloc_stat (enum gimple_code code, unsigned num_ops MEM_STAT_DECL)
{
  size_t size;
  gimple stmt;

  size = gimple_size (code);
  if (num_ops > 0)
    size += sizeof (tree) * (num_ops - 1);

#ifdef GATHER_STATISTICS
  {
    enum gimple_alloc_kind kind = gimple_alloc_kind (code);
    gimple_alloc_counts[(int) kind]++;
    gimple_alloc_sizes[(int) kind] += size;
  }
#endif

  stmt = ggc_alloc_cleared_gimple_statement_d_stat (size PASS_MEM_STAT);
  gimple_set_code (stmt, code);
  gimple_set_num_ops (stmt, num_ops);

  /* Do not call gimple_set_modified here as it has other side
     effects and this tuple is still not completely built.  */
  stmt->gsbase.modified = 1;

  return stmt;
}

/* Set SUBCODE to be the code of the expression computed by statement G.  */

static inline void
gimple_set_subcode (gimple g, unsigned subcode)
{
  /* We only have 16 bits for the RHS code.  Assert that we are not
     overflowing it.  */
  gcc_assert (subcode < (1 << 16));
  g->gsbase.subcode = subcode;
}



/* Build a tuple with operands.  CODE is the statement to build (which
   must be one of the GIMPLE_WITH_OPS tuples).  SUBCODE is the sub-code
   for the new tuple.  NUM_OPS is the number of operands to allocate.  */

#define gimple_build_with_ops(c, s, n) \
  gimple_build_with_ops_stat (c, s, n MEM_STAT_INFO)

static gimple
gimple_build_with_ops_stat (enum gimple_code code, unsigned subcode,
		            unsigned num_ops MEM_STAT_DECL)
{
  gimple s = gimple_alloc_stat (code, num_ops PASS_MEM_STAT);
  gimple_set_subcode (s, subcode);

  return s;
}


/* Build a GIMPLE_RETURN statement returning RETVAL.  */

gimple
gimple_build_return (tree retval)
{
  gimple s = gimple_build_with_ops (GIMPLE_RETURN, ERROR_MARK, 1);
  if (retval)
    gimple_return_set_retval (s, retval);
  return s;
}

/* Reset alias information on call S.  */

void
gimple_call_reset_alias_info (gimple s)
{
  if (gimple_call_flags (s) & ECF_CONST)
    memset (gimple_call_use_set (s), 0, sizeof (struct pt_solution));
  else
    pt_solution_reset (gimple_call_use_set (s));
  if (gimple_call_flags (s) & (ECF_CONST|ECF_PURE|ECF_NOVOPS))
    memset (gimple_call_clobber_set (s), 0, sizeof (struct pt_solution));
  else
    pt_solution_reset (gimple_call_clobber_set (s));
}

/* Helper for gimple_build_call, gimple_build_call_vec and
   gimple_build_call_from_tree.  Build the basic components of a
   GIMPLE_CALL statement to function FN with NARGS arguments.  */

static inline gimple
gimple_build_call_1 (tree fn, unsigned nargs)
{
  gimple s = gimple_build_with_ops (GIMPLE_CALL, ERROR_MARK, nargs + 3);
  if (TREE_CODE (fn) == FUNCTION_DECL)
    fn = build_fold_addr_expr (fn);
  gimple_set_op (s, 1, fn);
  gimple_call_reset_alias_info (s);
  return s;
}


/* Build a GIMPLE_CALL statement to function FN with the arguments
   specified in vector ARGS.  */

gimple
gimple_build_call_vec (tree fn, VEC(tree, heap) *args)
{
  unsigned i;
  unsigned nargs = VEC_length (tree, args);
  gimple call = gimple_build_call_1 (fn, nargs);

  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, VEC_index (tree, args, i));

  return call;
}


/* Build a GIMPLE_CALL statement to function FN.  NARGS is the number of
   arguments.  The ... are the arguments.  */

gimple
gimple_build_call (tree fn, unsigned nargs, ...)
{
  va_list ap;
  gimple call;
  unsigned i;

  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL || is_gimple_call_addr (fn));

  call = gimple_build_call_1 (fn, nargs);

  va_start (ap, nargs);
  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, va_arg (ap, tree));
  va_end (ap);

  return call;
}


/* Build a GIMPLE_CALL statement from CALL_EXPR T.  Note that T is
   assumed to be in GIMPLE form already.  Minimal checking is done of
   this fact.  */

gimple
gimple_build_call_from_tree (tree t)
{
  unsigned i, nargs;
  gimple call;
  tree fndecl = get_callee_fndecl (t);

  gcc_assert (TREE_CODE (t) == CALL_EXPR);

  nargs = call_expr_nargs (t);
  call = gimple_build_call_1 (fndecl ? fndecl : CALL_EXPR_FN (t), nargs);

  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, CALL_EXPR_ARG (t, i));

  gimple_set_block (call, TREE_BLOCK (t));

  /* Carry all the CALL_EXPR flags to the new GIMPLE_CALL.  */
  gimple_call_set_chain (call, CALL_EXPR_STATIC_CHAIN (t));
  gimple_call_set_tail (call, CALL_EXPR_TAILCALL (t));
  gimple_call_set_cannot_inline (call, CALL_CANNOT_INLINE_P (t));
  gimple_call_set_return_slot_opt (call, CALL_EXPR_RETURN_SLOT_OPT (t));
  gimple_call_set_from_thunk (call, CALL_FROM_THUNK_P (t));
  gimple_call_set_va_arg_pack (call, CALL_EXPR_VA_ARG_PACK (t));
  gimple_call_set_nothrow (call, TREE_NOTHROW (t));
  gimple_set_no_warning (call, TREE_NO_WARNING (t));

  return call;
}


/* Extract the operands and code for expression EXPR into *SUBCODE_P,
   *OP1_P, *OP2_P and *OP3_P respectively.  */

void
extract_ops_from_tree_1 (tree expr, enum tree_code *subcode_p, tree *op1_p,
			 tree *op2_p, tree *op3_p)
{
  enum gimple_rhs_class grhs_class;

  *subcode_p = TREE_CODE (expr);
  grhs_class = get_gimple_rhs_class (*subcode_p);

  if (grhs_class == GIMPLE_TERNARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = TREE_OPERAND (expr, 1);
      *op3_p = TREE_OPERAND (expr, 2);
    }
  else if (grhs_class == GIMPLE_BINARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = TREE_OPERAND (expr, 1);
      *op3_p = NULL_TREE;
    }
  else if (grhs_class == GIMPLE_UNARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = NULL_TREE;
      *op3_p = NULL_TREE;
    }
  else if (grhs_class == GIMPLE_SINGLE_RHS)
    {
      *op1_p = expr;
      *op2_p = NULL_TREE;
      *op3_p = NULL_TREE;
    }
  else
    gcc_unreachable ();
}


/* Build a GIMPLE_ASSIGN statement.

   LHS of the assignment.
   RHS of the assignment which can be unary or binary.  */

gimple
gimple_build_assign_stat (tree lhs, tree rhs MEM_STAT_DECL)
{
  enum tree_code subcode;
  tree op1, op2, op3;

  extract_ops_from_tree_1 (rhs, &subcode, &op1, &op2, &op3);
  return gimple_build_assign_with_ops_stat (subcode, lhs, op1, op2, op3
  					    PASS_MEM_STAT);
}


/* Build a GIMPLE_ASSIGN statement with sub-code SUBCODE and operands
   OP1 and OP2.  If OP2 is NULL then SUBCODE must be of class
   GIMPLE_UNARY_RHS or GIMPLE_SINGLE_RHS.  */

gimple
gimple_build_assign_with_ops_stat (enum tree_code subcode, tree lhs, tree op1,
                                   tree op2, tree op3 MEM_STAT_DECL)
{
  unsigned num_ops;
  gimple p;

  /* Need 1 operand for LHS and 1 or 2 for the RHS (depending on the
     code).  */
  num_ops = get_gimple_rhs_num_ops (subcode) + 1;

  p = gimple_build_with_ops_stat (GIMPLE_ASSIGN, (unsigned)subcode, num_ops
  			          PASS_MEM_STAT);
  gimple_assign_set_lhs (p, lhs);
  gimple_assign_set_rhs1 (p, op1);
  if (op2)
    {
      gcc_assert (num_ops > 2);
      gimple_assign_set_rhs2 (p, op2);
    }

  if (op3)
    {
      gcc_assert (num_ops > 3);
      gimple_assign_set_rhs3 (p, op3);
    }

  return p;
}


/* Build a new GIMPLE_ASSIGN tuple and append it to the end of *SEQ_P.

   DST/SRC are the destination and source respectively.  You can pass
   ungimplified trees in DST or SRC, in which case they will be
   converted to a gimple operand if necessary.

   This function returns the newly created GIMPLE_ASSIGN tuple.  */

gimple
gimplify_assign (tree dst, tree src, gimple_seq *seq_p)
{
  tree t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
  gimplify_and_add (t, seq_p);
  ggc_free (t);
  return gimple_seq_last_stmt (*seq_p);
}


/* Build a GIMPLE_COND statement.

   PRED is the condition used to compare LHS and the RHS.
   T_LABEL is the label to jump to if the condition is true.
   F_LABEL is the label to jump to otherwise.  */

gimple
gimple_build_cond (enum tree_code pred_code, tree lhs, tree rhs,
		   tree t_label, tree f_label)
{
  gimple p;

  gcc_assert (TREE_CODE_CLASS (pred_code) == tcc_comparison);
  p = gimple_build_with_ops (GIMPLE_COND, pred_code, 4);
  gimple_cond_set_lhs (p, lhs);
  gimple_cond_set_rhs (p, rhs);
  gimple_cond_set_true_label (p, t_label);
  gimple_cond_set_false_label (p, f_label);
  return p;
}


/* Extract operands for a GIMPLE_COND statement out of COND_EXPR tree COND.  */

void
gimple_cond_get_ops_from_tree (tree cond, enum tree_code *code_p,
                               tree *lhs_p, tree *rhs_p)
{
  gcc_assert (TREE_CODE_CLASS (TREE_CODE (cond)) == tcc_comparison
	      || TREE_CODE (cond) == TRUTH_NOT_EXPR
	      || is_gimple_min_invariant (cond)
	      || SSA_VAR_P (cond));

  extract_ops_from_tree (cond, code_p, lhs_p, rhs_p);

  /* Canonicalize conditionals of the form 'if (!VAL)'.  */
  if (*code_p == TRUTH_NOT_EXPR)
    {
      *code_p = EQ_EXPR;
      gcc_assert (*lhs_p && *rhs_p == NULL_TREE);
      *rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
    }
  /* Canonicalize conditionals of the form 'if (VAL)'  */
  else if (TREE_CODE_CLASS (*code_p) != tcc_comparison)
    {
      *code_p = NE_EXPR;
      gcc_assert (*lhs_p && *rhs_p == NULL_TREE);
      *rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
    }
}


/* Build a GIMPLE_COND statement from the conditional expression tree
   COND.  T_LABEL and F_LABEL are as in gimple_build_cond.  */

gimple
gimple_build_cond_from_tree (tree cond, tree t_label, tree f_label)
{
  enum tree_code code;
  tree lhs, rhs;

  gimple_cond_get_ops_from_tree (cond, &code, &lhs, &rhs);
  return gimple_build_cond (code, lhs, rhs, t_label, f_label);
}

/* Set code, lhs, and rhs of a GIMPLE_COND from a suitable
   boolean expression tree COND.  */

void
gimple_cond_set_condition_from_tree (gimple stmt, tree cond)
{
  enum tree_code code;
  tree lhs, rhs;

  gimple_cond_get_ops_from_tree (cond, &code, &lhs, &rhs);
  gimple_cond_set_condition (stmt, code, lhs, rhs);
}

/* Build a GIMPLE_LABEL statement for LABEL.  */

gimple
gimple_build_label (tree label)
{
  gimple p = gimple_build_with_ops (GIMPLE_LABEL, ERROR_MARK, 1);
  gimple_label_set_label (p, label);
  return p;
}

/* Build a GIMPLE_GOTO statement to label DEST.  */

gimple
gimple_build_goto (tree dest)
{
  gimple p = gimple_build_with_ops (GIMPLE_GOTO, ERROR_MARK, 1);
  gimple_goto_set_dest (p, dest);
  return p;
}


/* Build a GIMPLE_NOP statement.  */

gimple
gimple_build_nop (void)
{
  return gimple_alloc (GIMPLE_NOP, 0);
}


/* Build a GIMPLE_BIND statement.
   VARS are the variables in BODY.
   BLOCK is the containing block.  */

gimple
gimple_build_bind (tree vars, gimple_seq body, tree block)
{
  gimple p = gimple_alloc (GIMPLE_BIND, 0);
  gimple_bind_set_vars (p, vars);
  if (body)
    gimple_bind_set_body (p, body);
  if (block)
    gimple_bind_set_block (p, block);
  return p;
}

/* Helper function to set the simple fields of a asm stmt.

   STRING is a pointer to a string that is the asm blocks assembly code.
   NINPUT is the number of register inputs.
   NOUTPUT is the number of register outputs.
   NCLOBBERS is the number of clobbered registers.
   */

static inline gimple
gimple_build_asm_1 (const char *string, unsigned ninputs, unsigned noutputs,
                    unsigned nclobbers, unsigned nlabels)
{
  gimple p;
  int size = strlen (string);

  /* ASMs with labels cannot have outputs.  This should have been
     enforced by the front end.  */
  gcc_assert (nlabels == 0 || noutputs == 0);

  p = gimple_build_with_ops (GIMPLE_ASM, ERROR_MARK,
			     ninputs + noutputs + nclobbers + nlabels);

  p->gimple_asm.ni = ninputs;
  p->gimple_asm.no = noutputs;
  p->gimple_asm.nc = nclobbers;
  p->gimple_asm.nl = nlabels;
  p->gimple_asm.string = ggc_alloc_string (string, size);

#ifdef GATHER_STATISTICS
  gimple_alloc_sizes[(int) gimple_alloc_kind (GIMPLE_ASM)] += size;
#endif

  return p;
}

/* Build a GIMPLE_ASM statement.

   STRING is the assembly code.
   NINPUT is the number of register inputs.
   NOUTPUT is the number of register outputs.
   NCLOBBERS is the number of clobbered registers.
   INPUTS is a vector of the input register parameters.
   OUTPUTS is a vector of the output register parameters.
   CLOBBERS is a vector of the clobbered register parameters.
   LABELS is a vector of destination labels.  */

gimple
gimple_build_asm_vec (const char *string, VEC(tree,gc)* inputs,
                      VEC(tree,gc)* outputs, VEC(tree,gc)* clobbers,
		      VEC(tree,gc)* labels)
{
  gimple p;
  unsigned i;

  p = gimple_build_asm_1 (string,
                          VEC_length (tree, inputs),
                          VEC_length (tree, outputs),
                          VEC_length (tree, clobbers),
			  VEC_length (tree, labels));

  for (i = 0; i < VEC_length (tree, inputs); i++)
    gimple_asm_set_input_op (p, i, VEC_index (tree, inputs, i));

  for (i = 0; i < VEC_length (tree, outputs); i++)
    gimple_asm_set_output_op (p, i, VEC_index (tree, outputs, i));

  for (i = 0; i < VEC_length (tree, clobbers); i++)
    gimple_asm_set_clobber_op (p, i, VEC_index (tree, clobbers, i));

  for (i = 0; i < VEC_length (tree, labels); i++)
    gimple_asm_set_label_op (p, i, VEC_index (tree, labels, i));

  return p;
}

/* Build a GIMPLE_CATCH statement.

  TYPES are the catch types.
  HANDLER is the exception handler.  */

gimple
gimple_build_catch (tree types, gimple_seq handler)
{
  gimple p = gimple_alloc (GIMPLE_CATCH, 0);
  gimple_catch_set_types (p, types);
  if (handler)
    gimple_catch_set_handler (p, handler);

  return p;
}

/* Build a GIMPLE_EH_FILTER statement.

   TYPES are the filter's types.
   FAILURE is the filter's failure action.  */

gimple
gimple_build_eh_filter (tree types, gimple_seq failure)
{
  gimple p = gimple_alloc (GIMPLE_EH_FILTER, 0);
  gimple_eh_filter_set_types (p, types);
  if (failure)
    gimple_eh_filter_set_failure (p, failure);

  return p;
}

/* Build a GIMPLE_EH_MUST_NOT_THROW statement.  */

gimple
gimple_build_eh_must_not_throw (tree decl)
{
  gimple p = gimple_alloc (GIMPLE_EH_MUST_NOT_THROW, 0);

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  gcc_assert (flags_from_decl_or_type (decl) & ECF_NORETURN);
  gimple_eh_must_not_throw_set_fndecl (p, decl);

  return p;
}

/* Build a GIMPLE_TRY statement.

   EVAL is the expression to evaluate.
   CLEANUP is the cleanup expression.
   KIND is either GIMPLE_TRY_CATCH or GIMPLE_TRY_FINALLY depending on
   whether this is a try/catch or a try/finally respectively.  */

gimple
gimple_build_try (gimple_seq eval, gimple_seq cleanup,
    		  enum gimple_try_flags kind)
{
  gimple p;

  gcc_assert (kind == GIMPLE_TRY_CATCH || kind == GIMPLE_TRY_FINALLY);
  p = gimple_alloc (GIMPLE_TRY, 0);
  gimple_set_subcode (p, kind);
  if (eval)
    gimple_try_set_eval (p, eval);
  if (cleanup)
    gimple_try_set_cleanup (p, cleanup);

  return p;
}

/* Construct a GIMPLE_WITH_CLEANUP_EXPR statement.

   CLEANUP is the cleanup expression.  */

gimple
gimple_build_wce (gimple_seq cleanup)
{
  gimple p = gimple_alloc (GIMPLE_WITH_CLEANUP_EXPR, 0);
  if (cleanup)
    gimple_wce_set_cleanup (p, cleanup);

  return p;
}


/* Build a GIMPLE_RESX statement.  */

gimple
gimple_build_resx (int region)
{
  gimple p = gimple_build_with_ops (GIMPLE_RESX, ERROR_MARK, 0);
  p->gimple_eh_ctrl.region = region;
  return p;
}


/* The helper for constructing a gimple switch statement.
   INDEX is the switch's index.
   NLABELS is the number of labels in the switch excluding the default.
   DEFAULT_LABEL is the default label for the switch statement.  */

gimple
gimple_build_switch_nlabels (unsigned nlabels, tree index, tree default_label)
{
  /* nlabels + 1 default label + 1 index.  */
  gimple p = gimple_build_with_ops (GIMPLE_SWITCH, ERROR_MARK,
				    1 + (default_label != NULL) + nlabels);
  gimple_switch_set_index (p, index);
  if (default_label)
    gimple_switch_set_default_label (p, default_label);
  return p;
}


/* Build a GIMPLE_SWITCH statement.

   INDEX is the switch's index.
   NLABELS is the number of labels in the switch excluding the DEFAULT_LABEL.
   ... are the labels excluding the default.  */

gimple
gimple_build_switch (unsigned nlabels, tree index, tree default_label, ...)
{
  va_list al;
  unsigned i, offset;
  gimple p = gimple_build_switch_nlabels (nlabels, index, default_label);

  /* Store the rest of the labels.  */
  va_start (al, default_label);
  offset = (default_label != NULL);
  for (i = 0; i < nlabels; i++)
    gimple_switch_set_label (p, i + offset, va_arg (al, tree));
  va_end (al);

  return p;
}


/* Build a GIMPLE_SWITCH statement.

   INDEX is the switch's index.
   DEFAULT_LABEL is the default label
   ARGS is a vector of labels excluding the default.  */

gimple
gimple_build_switch_vec (tree index, tree default_label, VEC(tree, heap) *args)
{
  unsigned i, offset, nlabels = VEC_length (tree, args);
  gimple p = gimple_build_switch_nlabels (nlabels, index, default_label);

  /* Copy the labels from the vector to the switch statement.  */
  offset = (default_label != NULL);
  for (i = 0; i < nlabels; i++)
    gimple_switch_set_label (p, i + offset, VEC_index (tree, args, i));

  return p;
}

/* Build a GIMPLE_EH_DISPATCH statement.  */

gimple
gimple_build_eh_dispatch (int region)
{
  gimple p = gimple_build_with_ops (GIMPLE_EH_DISPATCH, ERROR_MARK, 0);
  p->gimple_eh_ctrl.region = region;
  return p;
}

/* Build a new GIMPLE_DEBUG_BIND statement.

   VAR is bound to VALUE; block and location are taken from STMT.  */

gimple
gimple_build_debug_bind_stat (tree var, tree value, gimple stmt MEM_STAT_DECL)
{
  gimple p = gimple_build_with_ops_stat (GIMPLE_DEBUG,
					 (unsigned)GIMPLE_DEBUG_BIND, 2
					 PASS_MEM_STAT);

  gimple_debug_bind_set_var (p, var);
  gimple_debug_bind_set_value (p, value);
  if (stmt)
    {
      gimple_set_block (p, gimple_block (stmt));
      gimple_set_location (p, gimple_location (stmt));
    }

  return p;
}


/* Build a GIMPLE_OMP_CRITICAL statement.

   BODY is the sequence of statements for which only one thread can execute.
   NAME is optional identifier for this critical block.  */

gimple
gimple_build_omp_critical (gimple_seq body, tree name)
{
  gimple p = gimple_alloc (GIMPLE_OMP_CRITICAL, 0);
  gimple_omp_critical_set_name (p, name);
  if (body)
    gimple_omp_set_body (p, body);

  return p;
}

/* Build a GIMPLE_OMP_FOR statement.

   BODY is sequence of statements inside the for loop.
   CLAUSES, are any of the OMP loop construct's clauses: private, firstprivate,
   lastprivate, reductions, ordered, schedule, and nowait.
   COLLAPSE is the collapse count.
   PRE_BODY is the sequence of statements that are loop invariant.  */

gimple
gimple_build_omp_for (gimple_seq body, tree clauses, size_t collapse,
		      gimple_seq pre_body)
{
  gimple p = gimple_alloc (GIMPLE_OMP_FOR, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_for_set_clauses (p, clauses);
  p->gimple_omp_for.collapse = collapse;
  p->gimple_omp_for.iter
      = ggc_alloc_cleared_vec_gimple_omp_for_iter (collapse);
  if (pre_body)
    gimple_omp_for_set_pre_body (p, pre_body);

  return p;
}


/* Build a GIMPLE_OMP_PARALLEL statement.

   BODY is sequence of statements which are executed in parallel.
   CLAUSES, are the OMP parallel construct's clauses.
   CHILD_FN is the function created for the parallel threads to execute.
   DATA_ARG are the shared data argument(s).  */

gimple
gimple_build_omp_parallel (gimple_seq body, tree clauses, tree child_fn,
			   tree data_arg)
{
  gimple p = gimple_alloc (GIMPLE_OMP_PARALLEL, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_parallel_set_clauses (p, clauses);
  gimple_omp_parallel_set_child_fn (p, child_fn);
  gimple_omp_parallel_set_data_arg (p, data_arg);

  return p;
}


/* Build a GIMPLE_OMP_TASK statement.

   BODY is sequence of statements which are executed by the explicit task.
   CLAUSES, are the OMP parallel construct's clauses.
   CHILD_FN is the function created for the parallel threads to execute.
   DATA_ARG are the shared data argument(s).
   COPY_FN is the optional function for firstprivate initialization.
   ARG_SIZE and ARG_ALIGN are size and alignment of the data block.  */

gimple
gimple_build_omp_task (gimple_seq body, tree clauses, tree child_fn,
		       tree data_arg, tree copy_fn, tree arg_size,
		       tree arg_align)
{
  gimple p = gimple_alloc (GIMPLE_OMP_TASK, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_task_set_clauses (p, clauses);
  gimple_omp_task_set_child_fn (p, child_fn);
  gimple_omp_task_set_data_arg (p, data_arg);
  gimple_omp_task_set_copy_fn (p, copy_fn);
  gimple_omp_task_set_arg_size (p, arg_size);
  gimple_omp_task_set_arg_align (p, arg_align);

  return p;
}


/* Build a GIMPLE_OMP_SECTION statement for a sections statement.

   BODY is the sequence of statements in the section.  */

gimple
gimple_build_omp_section (gimple_seq body)
{
  gimple p = gimple_alloc (GIMPLE_OMP_SECTION, 0);
  if (body)
    gimple_omp_set_body (p, body);

  return p;
}


/* Build a GIMPLE_OMP_MASTER statement.

   BODY is the sequence of statements to be executed by just the master.  */

gimple
gimple_build_omp_master (gimple_seq body)
{
  gimple p = gimple_alloc (GIMPLE_OMP_MASTER, 0);
  if (body)
    gimple_omp_set_body (p, body);

  return p;
}


/* Build a GIMPLE_OMP_CONTINUE statement.

   CONTROL_DEF is the definition of the control variable.
   CONTROL_USE is the use of the control variable.  */

gimple
gimple_build_omp_continue (tree control_def, tree control_use)
{
  gimple p = gimple_alloc (GIMPLE_OMP_CONTINUE, 0);
  gimple_omp_continue_set_control_def (p, control_def);
  gimple_omp_continue_set_control_use (p, control_use);
  return p;
}

/* Build a GIMPLE_OMP_ORDERED statement.

   BODY is the sequence of statements inside a loop that will executed in
   sequence.  */

gimple
gimple_build_omp_ordered (gimple_seq body)
{
  gimple p = gimple_alloc (GIMPLE_OMP_ORDERED, 0);
  if (body)
    gimple_omp_set_body (p, body);

  return p;
}


/* Build a GIMPLE_OMP_RETURN statement.
   WAIT_P is true if this is a non-waiting return.  */

gimple
gimple_build_omp_return (bool wait_p)
{
  gimple p = gimple_alloc (GIMPLE_OMP_RETURN, 0);
  if (wait_p)
    gimple_omp_return_set_nowait (p);

  return p;
}


/* Build a GIMPLE_OMP_SECTIONS statement.

   BODY is a sequence of section statements.
   CLAUSES are any of the OMP sections contsruct's clauses: private,
   firstprivate, lastprivate, reduction, and nowait.  */

gimple
gimple_build_omp_sections (gimple_seq body, tree clauses)
{
  gimple p = gimple_alloc (GIMPLE_OMP_SECTIONS, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_sections_set_clauses (p, clauses);

  return p;
}


/* Build a GIMPLE_OMP_SECTIONS_SWITCH.  */

gimple
gimple_build_omp_sections_switch (void)
{
  return gimple_alloc (GIMPLE_OMP_SECTIONS_SWITCH, 0);
}


/* Build a GIMPLE_OMP_SINGLE statement.

   BODY is the sequence of statements that will be executed once.
   CLAUSES are any of the OMP single construct's clauses: private, firstprivate,
   copyprivate, nowait.  */

gimple
gimple_build_omp_single (gimple_seq body, tree clauses)
{
  gimple p = gimple_alloc (GIMPLE_OMP_SINGLE, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_single_set_clauses (p, clauses);

  return p;
}


/* Build a GIMPLE_OMP_ATOMIC_LOAD statement.  */

gimple
gimple_build_omp_atomic_load (tree lhs, tree rhs)
{
  gimple p = gimple_alloc (GIMPLE_OMP_ATOMIC_LOAD, 0);
  gimple_omp_atomic_load_set_lhs (p, lhs);
  gimple_omp_atomic_load_set_rhs (p, rhs);
  return p;
}

/* Build a GIMPLE_OMP_ATOMIC_STORE statement.

   VAL is the value we are storing.  */

gimple
gimple_build_omp_atomic_store (tree val)
{
  gimple p = gimple_alloc (GIMPLE_OMP_ATOMIC_STORE, 0);
  gimple_omp_atomic_store_set_val (p, val);
  return p;
}

/* Build a GIMPLE_PREDICT statement.  PREDICT is one of the predictors from
   predict.def, OUTCOME is NOT_TAKEN or TAKEN.  */

gimple
gimple_build_predict (enum br_predictor predictor, enum prediction outcome)
{
  gimple p = gimple_alloc (GIMPLE_PREDICT, 0);
  /* Ensure all the predictors fit into the lower bits of the subcode.  */
  gcc_assert ((int) END_PREDICTORS <= GF_PREDICT_TAKEN);
  gimple_predict_set_predictor (p, predictor);
  gimple_predict_set_outcome (p, outcome);
  return p;
}

#if defined ENABLE_GIMPLE_CHECKING
/* Complain of a gimple type mismatch and die.  */

void
gimple_check_failed (const_gimple gs, const char *file, int line,
		     const char *function, enum gimple_code code,
		     enum tree_code subcode)
{
  internal_error ("gimple check: expected %s(%s), have %s(%s) in %s, at %s:%d",
      		  gimple_code_name[code],
		  tree_code_name[subcode],
		  gimple_code_name[gimple_code (gs)],
		  gs->gsbase.subcode > 0
		    ? tree_code_name[gs->gsbase.subcode]
		    : "",
		  function, trim_filename (file), line);
}
#endif /* ENABLE_GIMPLE_CHECKING */


/* Allocate a new GIMPLE sequence in GC memory and return it.  If
   there are free sequences in GIMPLE_SEQ_CACHE return one of those
   instead.  */

gimple_seq
gimple_seq_alloc (void)
{
  gimple_seq seq = gimple_seq_cache;
  if (seq)
    {
      gimple_seq_cache = gimple_seq_cache->next_free;
      gcc_assert (gimple_seq_cache != seq);
      memset (seq, 0, sizeof (*seq));
    }
  else
    {
      seq = ggc_alloc_cleared_gimple_seq_d ();
#ifdef GATHER_STATISTICS
      gimple_alloc_counts[(int) gimple_alloc_kind_seq]++;
      gimple_alloc_sizes[(int) gimple_alloc_kind_seq] += sizeof (*seq);
#endif
    }

  return seq;
}

/* Return SEQ to the free pool of GIMPLE sequences.  */

void
gimple_seq_free (gimple_seq seq)
{
  if (seq == NULL)
    return;

  gcc_assert (gimple_seq_first (seq) == NULL);
  gcc_assert (gimple_seq_last (seq) == NULL);

  /* If this triggers, it's a sign that the same list is being freed
     twice.  */
  gcc_assert (seq != gimple_seq_cache || gimple_seq_cache == NULL);

  /* Add SEQ to the pool of free sequences.  */
  seq->next_free = gimple_seq_cache;
  gimple_seq_cache = seq;
}


/* Link gimple statement GS to the end of the sequence *SEQ_P.  If
   *SEQ_P is NULL, a new sequence is allocated.  */

void
gimple_seq_add_stmt (gimple_seq *seq_p, gimple gs)
{
  gimple_stmt_iterator si;

  if (gs == NULL)
    return;

  if (*seq_p == NULL)
    *seq_p = gimple_seq_alloc ();

  si = gsi_last (*seq_p);
  gsi_insert_after (&si, gs, GSI_NEW_STMT);
}


/* Append sequence SRC to the end of sequence *DST_P.  If *DST_P is
   NULL, a new sequence is allocated.  */

void
gimple_seq_add_seq (gimple_seq *dst_p, gimple_seq src)
{
  gimple_stmt_iterator si;

  if (src == NULL)
    return;

  if (*dst_p == NULL)
    *dst_p = gimple_seq_alloc ();

  si = gsi_last (*dst_p);
  gsi_insert_seq_after (&si, src, GSI_NEW_STMT);
}


/* Helper function of empty_body_p.  Return true if STMT is an empty
   statement.  */

static bool
empty_stmt_p (gimple stmt)
{
  if (gimple_code (stmt) == GIMPLE_NOP)
    return true;
  if (gimple_code (stmt) == GIMPLE_BIND)
    return empty_body_p (gimple_bind_body (stmt));
  return false;
}


/* Return true if BODY contains nothing but empty statements.  */

bool
empty_body_p (gimple_seq body)
{
  gimple_stmt_iterator i;

  if (gimple_seq_empty_p (body))
    return true;
  for (i = gsi_start (body); !gsi_end_p (i); gsi_next (&i))
    if (!empty_stmt_p (gsi_stmt (i))
	&& !is_gimple_debug (gsi_stmt (i)))
      return false;

  return true;
}


/* Perform a deep copy of sequence SRC and return the result.  */

gimple_seq
gimple_seq_copy (gimple_seq src)
{
  gimple_stmt_iterator gsi;
  gimple_seq new_seq = gimple_seq_alloc ();
  gimple stmt;

  for (gsi = gsi_start (src); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gimple_copy (gsi_stmt (gsi));
      gimple_seq_add_stmt (&new_seq, stmt);
    }

  return new_seq;
}


/* Walk all the statements in the sequence SEQ calling walk_gimple_stmt
   on each one.  WI is as in walk_gimple_stmt.

   If walk_gimple_stmt returns non-NULL, the walk is stopped, the
   value is stored in WI->CALLBACK_RESULT and the statement that
   produced the value is returned.

   Otherwise, all the statements are walked and NULL returned.  */

gimple
walk_gimple_seq (gimple_seq seq, walk_stmt_fn callback_stmt,
		 walk_tree_fn callback_op, struct walk_stmt_info *wi)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree ret = walk_gimple_stmt (&gsi, callback_stmt, callback_op, wi);
      if (ret)
	{
	  /* If CALLBACK_STMT or CALLBACK_OP return a value, WI must exist
	     to hold it.  */
	  gcc_assert (wi);
	  wi->callback_result = ret;
	  return gsi_stmt (gsi);
	}
    }

  if (wi)
    wi->callback_result = NULL_TREE;

  return NULL;
}


/* Helper function for walk_gimple_stmt.  Walk operands of a GIMPLE_ASM.  */

static tree
walk_gimple_asm (gimple stmt, walk_tree_fn callback_op,
		 struct walk_stmt_info *wi)
{
  tree ret, op;
  unsigned noutputs;
  const char **oconstraints;
  unsigned i, n;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;

  noutputs = gimple_asm_noutputs (stmt);
  oconstraints = (const char **) alloca ((noutputs) * sizeof (const char *));

  if (wi)
    wi->is_lhs = true;

  for (i = 0; i < noutputs; i++)
    {
      op = gimple_asm_output_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
      oconstraints[i] = constraint;
      parse_output_constraint (&constraint, i, 0, 0, &allows_mem, &allows_reg,
	                       &is_inout);
      if (wi)
	wi->val_only = (allows_reg || !allows_mem);
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  n = gimple_asm_ninputs (stmt);
  for (i = 0; i < n; i++)
    {
      op = gimple_asm_input_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
			      oconstraints, &allows_mem, &allows_reg);
      if (wi)
	{
	  wi->val_only = (allows_reg || !allows_mem);
          /* Although input "m" is not really a LHS, we need a lvalue.  */
	  wi->is_lhs = !wi->val_only;
	}
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  if (wi)
    {
      wi->is_lhs = false;
      wi->val_only = true;
    }

  n = gimple_asm_nlabels (stmt);
  for (i = 0; i < n; i++)
    {
      op = gimple_asm_label_op (stmt, i);
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  return NULL_TREE;
}


/* Helper function of WALK_GIMPLE_STMT.  Walk every tree operand in
   STMT.  CALLBACK_OP and WI are as in WALK_GIMPLE_STMT.

   CALLBACK_OP is called on each operand of STMT via walk_tree.
   Additional parameters to walk_tree must be stored in WI.  For each operand
   OP, walk_tree is called as:

	walk_tree (&OP, CALLBACK_OP, WI, WI->PSET)

   If CALLBACK_OP returns non-NULL for an operand, the remaining
   operands are not scanned.

   The return value is that returned by the last call to walk_tree, or
   NULL_TREE if no CALLBACK_OP is specified.  */

tree
walk_gimple_op (gimple stmt, walk_tree_fn callback_op,
		struct walk_stmt_info *wi)
{
  struct pointer_set_t *pset = (wi) ? wi->pset : NULL;
  unsigned i;
  tree ret = NULL_TREE;

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      /* Walk the RHS operands.  If the LHS is of a non-renamable type or
         is a register variable, we may use a COMPONENT_REF on the RHS.  */
      if (wi)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  wi->val_only
	    = (is_gimple_reg_type (TREE_TYPE (lhs)) && !is_gimple_reg (lhs))
	      || !gimple_assign_single_p (stmt);
	}

      for (i = 1; i < gimple_num_ops (stmt); i++)
	{
	  ret = walk_tree (gimple_op_ptr (stmt, i), callback_op, wi,
			   pset);
	  if (ret)
	    return ret;
	}

      /* Walk the LHS.  If the RHS is appropriate for a memory, we
	 may use a COMPONENT_REF on the LHS.  */
      if (wi)
	{
          /* If the RHS has more than 1 operand, it is not appropriate
             for the memory.  */
	  wi->val_only = !is_gimple_mem_rhs (gimple_assign_rhs1 (stmt))
                         || !gimple_assign_single_p (stmt);
	  wi->is_lhs = true;
	}

      ret = walk_tree (gimple_op_ptr (stmt, 0), callback_op, wi, pset);
      if (ret)
	return ret;

      if (wi)
	{
	  wi->val_only = true;
	  wi->is_lhs = false;
	}
      break;

    case GIMPLE_CALL:
      if (wi)
	{
	  wi->is_lhs = false;
	  wi->val_only = true;
	}

      ret = walk_tree (gimple_call_chain_ptr (stmt), callback_op, wi, pset);
      if (ret)
        return ret;

      ret = walk_tree (gimple_call_fn_ptr (stmt), callback_op, wi, pset);
      if (ret)
        return ret;

      for (i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  if (wi)
	    wi->val_only = is_gimple_reg_type (gimple_call_arg (stmt, i));
	  ret = walk_tree (gimple_call_arg_ptr (stmt, i), callback_op, wi,
			   pset);
	  if (ret)
	    return ret;
	}

      if (gimple_call_lhs (stmt))
	{
	  if (wi)
	    {
	      wi->is_lhs = true;
	      wi->val_only = is_gimple_reg_type (gimple_call_lhs (stmt));
	    }

	  ret = walk_tree (gimple_call_lhs_ptr (stmt), callback_op, wi, pset);
	  if (ret)
	    return ret;
	}

      if (wi)
	{
	  wi->is_lhs = false;
	  wi->val_only = true;
	}
      break;

    case GIMPLE_CATCH:
      ret = walk_tree (gimple_catch_types_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_EH_FILTER:
      ret = walk_tree (gimple_eh_filter_types_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_ASM:
      ret = walk_gimple_asm (stmt, callback_op, wi);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_CONTINUE:
      ret = walk_tree (gimple_omp_continue_control_def_ptr (stmt),
	  	       callback_op, wi, pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_continue_control_use_ptr (stmt),
	  	       callback_op, wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_CRITICAL:
      ret = walk_tree (gimple_omp_critical_name_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_FOR:
      ret = walk_tree (gimple_omp_for_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
	{
	  ret = walk_tree (gimple_omp_for_index_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_initial_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_final_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_incr_ptr (stmt, i), callback_op,
			   wi, pset);
	}
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_PARALLEL:
      ret = walk_tree (gimple_omp_parallel_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_parallel_child_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_parallel_data_arg_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_TASK:
      ret = walk_tree (gimple_omp_task_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_child_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_data_arg_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_copy_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_arg_size_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_arg_align_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_SECTIONS:
      ret = walk_tree (gimple_omp_sections_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_sections_control_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;

      break;

    case GIMPLE_OMP_SINGLE:
      ret = walk_tree (gimple_omp_single_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
      ret = walk_tree (gimple_omp_atomic_load_lhs_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_atomic_load_rhs_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_ATOMIC_STORE:
      ret = walk_tree (gimple_omp_atomic_store_val_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

      /* Tuples that do not have operands.  */
    case GIMPLE_NOP:
    case GIMPLE_RESX:
    case GIMPLE_OMP_RETURN:
    case GIMPLE_PREDICT:
      break;

    default:
      {
	enum gimple_statement_structure_enum gss;
	gss = gimple_statement_structure (stmt);
	if (gss == GSS_WITH_OPS || gss == GSS_WITH_MEM_OPS)
	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      ret = walk_tree (gimple_op_ptr (stmt, i), callback_op, wi, pset);
	      if (ret)
		return ret;
	    }
      }
      break;
    }

  return NULL_TREE;
}


/* Walk the current statement in GSI (optionally using traversal state
   stored in WI).  If WI is NULL, no state is kept during traversal.
   The callback CALLBACK_STMT is called.  If CALLBACK_STMT indicates
   that it has handled all the operands of the statement, its return
   value is returned.  Otherwise, the return value from CALLBACK_STMT
   is discarded and its operands are scanned.

   If CALLBACK_STMT is NULL or it didn't handle the operands,
   CALLBACK_OP is called on each operand of the statement via
   walk_gimple_op.  If walk_gimple_op returns non-NULL for any
   operand, the remaining operands are not scanned.  In this case, the
   return value from CALLBACK_OP is returned.

   In any other case, NULL_TREE is returned.  */

tree
walk_gimple_stmt (gimple_stmt_iterator *gsi, walk_stmt_fn callback_stmt,
		  walk_tree_fn callback_op, struct walk_stmt_info *wi)
{
  gimple ret;
  tree tree_ret;
  gimple stmt = gsi_stmt (*gsi);

  if (wi)
    wi->gsi = *gsi;

  if (wi && wi->want_locations && gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  ret = NULL;

  /* Invoke the statement callback.  Return if the callback handled
     all of STMT operands by itself.  */
  if (callback_stmt)
    {
      bool handled_ops = false;
      tree_ret = callback_stmt (gsi, &handled_ops, wi);
      if (handled_ops)
	return tree_ret;

      /* If CALLBACK_STMT did not handle operands, it should not have
	 a value to return.  */
      gcc_assert (tree_ret == NULL);

      /* Re-read stmt in case the callback changed it.  */
      stmt = gsi_stmt (*gsi);
    }

  /* If CALLBACK_OP is defined, invoke it on every operand of STMT.  */
  if (callback_op)
    {
      tree_ret = walk_gimple_op (stmt, callback_op, wi);
      if (tree_ret)
	return tree_ret;
    }

  /* If STMT can have statements inside (e.g. GIMPLE_BIND), walk them.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      ret = walk_gimple_seq (gimple_bind_body (stmt), callback_stmt,
	                     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_CATCH:
      ret = walk_gimple_seq (gimple_catch_handler (stmt), callback_stmt,
	                     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_EH_FILTER:
      ret = walk_gimple_seq (gimple_eh_filter_failure (stmt), callback_stmt,
		             callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_TRY:
      ret = walk_gimple_seq (gimple_try_eval (stmt), callback_stmt, callback_op,
	                     wi);
      if (ret)
	return wi->callback_result;

      ret = walk_gimple_seq (gimple_try_cleanup (stmt), callback_stmt,
	                     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_OMP_FOR:
      ret = walk_gimple_seq (gimple_omp_for_pre_body (stmt), callback_stmt,
		             callback_op, wi);
      if (ret)
	return wi->callback_result;

      /* FALL THROUGH.  */
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
      ret = walk_gimple_seq (gimple_omp_body (stmt), callback_stmt, callback_op,
	                     wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_WITH_CLEANUP_EXPR:
      ret = walk_gimple_seq (gimple_wce_cleanup (stmt), callback_stmt,
			     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    default:
      gcc_assert (!gimple_has_substatements (stmt));
      break;
    }

  return NULL;
}


/* Set sequence SEQ to be the GIMPLE body for function FN.  */

void
gimple_set_body (tree fndecl, gimple_seq seq)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  if (fn == NULL)
    {
      /* If FNDECL still does not have a function structure associated
	 with it, then it does not make sense for it to receive a
	 GIMPLE body.  */
      gcc_assert (seq == NULL);
    }
  else
    fn->gimple_body = seq;
}


/* Return the body of GIMPLE statements for function FN.  After the
   CFG pass, the function body doesn't exist anymore because it has
   been split up into basic blocks.  In this case, it returns
   NULL.  */

gimple_seq
gimple_body (tree fndecl)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  return fn ? fn->gimple_body : NULL;
}

/* Return true when FNDECL has Gimple body either in unlowered
   or CFG form.  */
bool
gimple_has_body_p (tree fndecl)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  return (gimple_body (fndecl) || (fn && fn->cfg));
}

/* Detect flags from a GIMPLE_CALL.  This is just like
   call_expr_flags, but for gimple tuples.  */

int
gimple_call_flags (const_gimple stmt)
{
  int flags;
  tree decl = gimple_call_fndecl (stmt);
  tree t;

  if (decl)
    flags = flags_from_decl_or_type (decl);
  else
    {
      t = TREE_TYPE (gimple_call_fn (stmt));
      if (t && TREE_CODE (t) == POINTER_TYPE)
	flags = flags_from_decl_or_type (TREE_TYPE (t));
      else
	flags = 0;
    }

  if (stmt->gsbase.subcode & GF_CALL_NOTHROW)
    flags |= ECF_NOTHROW;

  return flags;
}

/* Detects argument flags for argument number ARG on call STMT.  */

int
gimple_call_arg_flags (const_gimple stmt, unsigned arg)
{
  tree type = TREE_TYPE (TREE_TYPE (gimple_call_fn (stmt)));
  tree attr = lookup_attribute ("fn spec", TYPE_ATTRIBUTES (type));
  if (!attr)
    return 0;

  attr = TREE_VALUE (TREE_VALUE (attr));
  if (1 + arg >= (unsigned) TREE_STRING_LENGTH (attr))
    return 0;

  switch (TREE_STRING_POINTER (attr)[1 + arg])
    {
    case 'x':
    case 'X':
      return EAF_UNUSED;

    case 'R':
      return EAF_DIRECT | EAF_NOCLOBBER | EAF_NOESCAPE;

    case 'r':
      return EAF_NOCLOBBER | EAF_NOESCAPE;

    case 'W':
      return EAF_DIRECT | EAF_NOESCAPE;

    case 'w':
      return EAF_NOESCAPE;

    case '.':
    default:
      return 0;
    }
}

/* Detects return flags for the call STMT.  */

int
gimple_call_return_flags (const_gimple stmt)
{
  tree type;
  tree attr = NULL_TREE;

  if (gimple_call_flags (stmt) & ECF_MALLOC)
    return ERF_NOALIAS;

  type = TREE_TYPE (TREE_TYPE (gimple_call_fn (stmt)));
  attr = lookup_attribute ("fn spec", TYPE_ATTRIBUTES (type));
  if (!attr)
    return 0;

  attr = TREE_VALUE (TREE_VALUE (attr));
  if (TREE_STRING_LENGTH (attr) < 1)
    return 0;

  switch (TREE_STRING_POINTER (attr)[0])
    {
    case '1':
    case '2':
    case '3':
    case '4':
      return ERF_RETURNS_ARG | (TREE_STRING_POINTER (attr)[0] - '1');

    case 'm':
      return ERF_NOALIAS;

    case '.':
    default:
      return 0;
    }
}


/* Return true if GS is a copy assignment.  */

bool
gimple_assign_copy_p (gimple gs)
{
  return (gimple_assign_single_p (gs)
	  && is_gimple_val (gimple_op (gs, 1)));
}


/* Return true if GS is a SSA_NAME copy assignment.  */

bool
gimple_assign_ssa_name_copy_p (gimple gs)
{
  return (gimple_assign_single_p (gs)
	  && TREE_CODE (gimple_assign_lhs (gs)) == SSA_NAME
	  && TREE_CODE (gimple_assign_rhs1 (gs)) == SSA_NAME);
}


/* Return true if GS is an assignment with a unary RHS, but the
   operator has no effect on the assigned value.  The logic is adapted
   from STRIP_NOPS.  This predicate is intended to be used in tuplifying
   instances in which STRIP_NOPS was previously applied to the RHS of
   an assignment.

   NOTE: In the use cases that led to the creation of this function
   and of gimple_assign_single_p, it is typical to test for either
   condition and to proceed in the same manner.  In each case, the
   assigned value is represented by the single RHS operand of the
   assignment.  I suspect there may be cases where gimple_assign_copy_p,
   gimple_assign_single_p, or equivalent logic is used where a similar
   treatment of unary NOPs is appropriate.  */

bool
gimple_assign_unary_nop_p (gimple gs)
{
  return (is_gimple_assign (gs)
          && (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (gs))
              || gimple_assign_rhs_code (gs) == NON_LVALUE_EXPR)
          && gimple_assign_rhs1 (gs) != error_mark_node
          && (TYPE_MODE (TREE_TYPE (gimple_assign_lhs (gs)))
              == TYPE_MODE (TREE_TYPE (gimple_assign_rhs1 (gs)))));
}

/* Set BB to be the basic block holding G.  */

void
gimple_set_bb (gimple stmt, basic_block bb)
{
  stmt->gsbase.bb = bb;

  /* If the statement is a label, add the label to block-to-labels map
     so that we can speed up edge creation for GIMPLE_GOTOs.  */
  if (cfun->cfg && gimple_code (stmt) == GIMPLE_LABEL)
    {
      tree t;
      int uid;

      t = gimple_label_label (stmt);
      uid = LABEL_DECL_UID (t);
      if (uid == -1)
	{
	  unsigned old_len = VEC_length (basic_block, label_to_block_map);
	  LABEL_DECL_UID (t) = uid = cfun->cfg->last_label_uid++;
	  if (old_len <= (unsigned) uid)
	    {
	      unsigned new_len = 3 * uid / 2 + 1;

	      VEC_safe_grow_cleared (basic_block, gc, label_to_block_map,
				     new_len);
	    }
	}

      VEC_replace (basic_block, label_to_block_map, uid, bb);
    }
}


/* Modify the RHS of the assignment pointed-to by GSI using the
   operands in the expression tree EXPR.

   NOTE: The statement pointed-to by GSI may be reallocated if it
   did not have enough operand slots.

   This function is useful to convert an existing tree expression into
   the flat representation used for the RHS of a GIMPLE assignment.
   It will reallocate memory as needed to expand or shrink the number
   of operand slots needed to represent EXPR.

   NOTE: If you find yourself building a tree and then calling this
   function, you are most certainly doing it the slow way.  It is much
   better to build a new assignment or to use the function
   gimple_assign_set_rhs_with_ops, which does not require an
   expression tree to be built.  */

void
gimple_assign_set_rhs_from_tree (gimple_stmt_iterator *gsi, tree expr)
{
  enum tree_code subcode;
  tree op1, op2, op3;

  extract_ops_from_tree_1 (expr, &subcode, &op1, &op2, &op3);
  gimple_assign_set_rhs_with_ops_1 (gsi, subcode, op1, op2, op3);
}


/* Set the RHS of assignment statement pointed-to by GSI to CODE with
   operands OP1, OP2 and OP3.

   NOTE: The statement pointed-to by GSI may be reallocated if it
   did not have enough operand slots.  */

void
gimple_assign_set_rhs_with_ops_1 (gimple_stmt_iterator *gsi, enum tree_code code,
				  tree op1, tree op2, tree op3)
{
  unsigned new_rhs_ops = get_gimple_rhs_num_ops (code);
  gimple stmt = gsi_stmt (*gsi);

  /* If the new CODE needs more operands, allocate a new statement.  */
  if (gimple_num_ops (stmt) < new_rhs_ops + 1)
    {
      tree lhs = gimple_assign_lhs (stmt);
      gimple new_stmt = gimple_alloc (gimple_code (stmt), new_rhs_ops + 1);
      memcpy (new_stmt, stmt, gimple_size (gimple_code (stmt)));
      gsi_replace (gsi, new_stmt, true);
      stmt = new_stmt;

      /* The LHS needs to be reset as this also changes the SSA name
	 on the LHS.  */
      gimple_assign_set_lhs (stmt, lhs);
    }

  gimple_set_num_ops (stmt, new_rhs_ops + 1);
  gimple_set_subcode (stmt, code);
  gimple_assign_set_rhs1 (stmt, op1);
  if (new_rhs_ops > 1)
    gimple_assign_set_rhs2 (stmt, op2);
  if (new_rhs_ops > 2)
    gimple_assign_set_rhs3 (stmt, op3);
}


/* Return the LHS of a statement that performs an assignment,
   either a GIMPLE_ASSIGN or a GIMPLE_CALL.  Returns NULL_TREE
   for a call to a function that returns no value, or for a
   statement other than an assignment or a call.  */

tree
gimple_get_lhs (const_gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);

  if (code == GIMPLE_ASSIGN)
    return gimple_assign_lhs (stmt);
  else if (code == GIMPLE_CALL)
    return gimple_call_lhs (stmt);
  else
    return NULL_TREE;
}


/* Set the LHS of a statement that performs an assignment,
   either a GIMPLE_ASSIGN or a GIMPLE_CALL.  */

void
gimple_set_lhs (gimple stmt, tree lhs)
{
  enum gimple_code code = gimple_code (stmt);

  if (code == GIMPLE_ASSIGN)
    gimple_assign_set_lhs (stmt, lhs);
  else if (code == GIMPLE_CALL)
    gimple_call_set_lhs (stmt, lhs);
  else
    gcc_unreachable();
}

/* Replace the LHS of STMT, an assignment, either a GIMPLE_ASSIGN or a
   GIMPLE_CALL, with NLHS, in preparation for modifying the RHS to an
   expression with a different value.

   This will update any annotations (say debug bind stmts) referring
   to the original LHS, so that they use the RHS instead.  This is
   done even if NLHS and LHS are the same, for it is understood that
   the RHS will be modified afterwards, and NLHS will not be assigned
   an equivalent value.

   Adjusting any non-annotation uses of the LHS, if needed, is a
   responsibility of the caller.

   The effect of this call should be pretty much the same as that of
   inserting a copy of STMT before STMT, and then removing the
   original stmt, at which time gsi_remove() would have update
   annotations, but using this function saves all the inserting,
   copying and removing.  */

void
gimple_replace_lhs (gimple stmt, tree nlhs)
{
  if (MAY_HAVE_DEBUG_STMTS)
    {
      tree lhs = gimple_get_lhs (stmt);

      gcc_assert (SSA_NAME_DEF_STMT (lhs) == stmt);

      insert_debug_temp_for_var_def (NULL, lhs);
    }

  gimple_set_lhs (stmt, nlhs);
}

/* Return a deep copy of statement STMT.  All the operands from STMT
   are reallocated and copied using unshare_expr.  The DEF, USE, VDEF
   and VUSE operand arrays are set to empty in the new copy.  */

gimple
gimple_copy (gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);
  unsigned num_ops = gimple_num_ops (stmt);
  gimple copy = gimple_alloc (code, num_ops);
  unsigned i;

  /* Shallow copy all the fields from STMT.  */
  memcpy (copy, stmt, gimple_size (code));

  /* If STMT has sub-statements, deep-copy them as well.  */
  if (gimple_has_substatements (stmt))
    {
      gimple_seq new_seq;
      tree t;

      switch (gimple_code (stmt))
	{
	case GIMPLE_BIND:
	  new_seq = gimple_seq_copy (gimple_bind_body (stmt));
	  gimple_bind_set_body (copy, new_seq);
	  gimple_bind_set_vars (copy, unshare_expr (gimple_bind_vars (stmt)));
	  gimple_bind_set_block (copy, gimple_bind_block (stmt));
	  break;

	case GIMPLE_CATCH:
	  new_seq = gimple_seq_copy (gimple_catch_handler (stmt));
	  gimple_catch_set_handler (copy, new_seq);
	  t = unshare_expr (gimple_catch_types (stmt));
	  gimple_catch_set_types (copy, t);
	  break;

	case GIMPLE_EH_FILTER:
	  new_seq = gimple_seq_copy (gimple_eh_filter_failure (stmt));
	  gimple_eh_filter_set_failure (copy, new_seq);
	  t = unshare_expr (gimple_eh_filter_types (stmt));
	  gimple_eh_filter_set_types (copy, t);
	  break;

	case GIMPLE_TRY:
	  new_seq = gimple_seq_copy (gimple_try_eval (stmt));
	  gimple_try_set_eval (copy, new_seq);
	  new_seq = gimple_seq_copy (gimple_try_cleanup (stmt));
	  gimple_try_set_cleanup (copy, new_seq);
	  break;

	case GIMPLE_OMP_FOR:
	  new_seq = gimple_seq_copy (gimple_omp_for_pre_body (stmt));
	  gimple_omp_for_set_pre_body (copy, new_seq);
	  t = unshare_expr (gimple_omp_for_clauses (stmt));
	  gimple_omp_for_set_clauses (copy, t);
	  copy->gimple_omp_for.iter
	    = ggc_alloc_vec_gimple_omp_for_iter
	    (gimple_omp_for_collapse (stmt));
	  for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
	    {
	      gimple_omp_for_set_cond (copy, i,
				       gimple_omp_for_cond (stmt, i));
	      gimple_omp_for_set_index (copy, i,
					gimple_omp_for_index (stmt, i));
	      t = unshare_expr (gimple_omp_for_initial (stmt, i));
	      gimple_omp_for_set_initial (copy, i, t);
	      t = unshare_expr (gimple_omp_for_final (stmt, i));
	      gimple_omp_for_set_final (copy, i, t);
	      t = unshare_expr (gimple_omp_for_incr (stmt, i));
	      gimple_omp_for_set_incr (copy, i, t);
	    }
	  goto copy_omp_body;

	case GIMPLE_OMP_PARALLEL:
	  t = unshare_expr (gimple_omp_parallel_clauses (stmt));
	  gimple_omp_parallel_set_clauses (copy, t);
	  t = unshare_expr (gimple_omp_parallel_child_fn (stmt));
	  gimple_omp_parallel_set_child_fn (copy, t);
	  t = unshare_expr (gimple_omp_parallel_data_arg (stmt));
	  gimple_omp_parallel_set_data_arg (copy, t);
	  goto copy_omp_body;

	case GIMPLE_OMP_TASK:
	  t = unshare_expr (gimple_omp_task_clauses (stmt));
	  gimple_omp_task_set_clauses (copy, t);
	  t = unshare_expr (gimple_omp_task_child_fn (stmt));
	  gimple_omp_task_set_child_fn (copy, t);
	  t = unshare_expr (gimple_omp_task_data_arg (stmt));
	  gimple_omp_task_set_data_arg (copy, t);
	  t = unshare_expr (gimple_omp_task_copy_fn (stmt));
	  gimple_omp_task_set_copy_fn (copy, t);
	  t = unshare_expr (gimple_omp_task_arg_size (stmt));
	  gimple_omp_task_set_arg_size (copy, t);
	  t = unshare_expr (gimple_omp_task_arg_align (stmt));
	  gimple_omp_task_set_arg_align (copy, t);
	  goto copy_omp_body;

	case GIMPLE_OMP_CRITICAL:
	  t = unshare_expr (gimple_omp_critical_name (stmt));
	  gimple_omp_critical_set_name (copy, t);
	  goto copy_omp_body;

	case GIMPLE_OMP_SECTIONS:
	  t = unshare_expr (gimple_omp_sections_clauses (stmt));
	  gimple_omp_sections_set_clauses (copy, t);
	  t = unshare_expr (gimple_omp_sections_control (stmt));
	  gimple_omp_sections_set_control (copy, t);
	  /* FALLTHRU  */

	case GIMPLE_OMP_SINGLE:
	case GIMPLE_OMP_SECTION:
	case GIMPLE_OMP_MASTER:
	case GIMPLE_OMP_ORDERED:
	copy_omp_body:
	  new_seq = gimple_seq_copy (gimple_omp_body (stmt));
	  gimple_omp_set_body (copy, new_seq);
	  break;

	case GIMPLE_WITH_CLEANUP_EXPR:
	  new_seq = gimple_seq_copy (gimple_wce_cleanup (stmt));
	  gimple_wce_set_cleanup (copy, new_seq);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Make copy of operands.  */
  if (num_ops > 0)
    {
      for (i = 0; i < num_ops; i++)
	gimple_set_op (copy, i, unshare_expr (gimple_op (stmt, i)));

      /* Clear out SSA operand vectors on COPY.  */
      if (gimple_has_ops (stmt))
	{
	  gimple_set_def_ops (copy, NULL);
	  gimple_set_use_ops (copy, NULL);
	}

      if (gimple_has_mem_ops (stmt))
	{
	  gimple_set_vdef (copy, gimple_vdef (stmt));
	  gimple_set_vuse (copy, gimple_vuse (stmt));
	}

      /* SSA operands need to be updated.  */
      gimple_set_modified (copy, true);
    }

  return copy;
}


/* Set the MODIFIED flag to MODIFIEDP, iff the gimple statement G has
   a MODIFIED field.  */

void
gimple_set_modified (gimple s, bool modifiedp)
{
  if (gimple_has_ops (s))
    {
      s->gsbase.modified = (unsigned) modifiedp;

      if (modifiedp
	  && cfun->gimple_df
	  && is_gimple_call (s)
	  && gimple_call_noreturn_p (s))
	VEC_safe_push (gimple, gc, MODIFIED_NORETURN_CALLS (cfun), s);
    }
}


/* Return true if statement S has side-effects.  We consider a
   statement to have side effects if:

   - It is a GIMPLE_CALL not marked with ECF_PURE or ECF_CONST.
   - Any of its operands are marked TREE_THIS_VOLATILE or TREE_SIDE_EFFECTS.  */

bool
gimple_has_side_effects (const_gimple s)
{
  unsigned i;

  if (is_gimple_debug (s))
    return false;

  /* We don't have to scan the arguments to check for
     volatile arguments, though, at present, we still
     do a scan to check for TREE_SIDE_EFFECTS.  */
  if (gimple_has_volatile_ops (s))
    return true;

  if (is_gimple_call (s))
    {
      unsigned nargs = gimple_call_num_args (s);

      if (!(gimple_call_flags (s) & (ECF_CONST | ECF_PURE)))
        return true;
      else if (gimple_call_flags (s) & ECF_LOOPING_CONST_OR_PURE)
	/* An infinite loop is considered a side effect.  */
	return true;

      if (gimple_call_lhs (s)
          && TREE_SIDE_EFFECTS (gimple_call_lhs (s)))
	{
	  gcc_assert (gimple_has_volatile_ops (s));
	  return true;
	}

      if (TREE_SIDE_EFFECTS (gimple_call_fn (s)))
        return true;

      for (i = 0; i < nargs; i++)
        if (TREE_SIDE_EFFECTS (gimple_call_arg (s, i)))
	  {
	    gcc_assert (gimple_has_volatile_ops (s));
	    return true;
	  }

      return false;
    }
  else
    {
      for (i = 0; i < gimple_num_ops (s); i++)
	if (TREE_SIDE_EFFECTS (gimple_op (s, i)))
	  {
	    gcc_assert (gimple_has_volatile_ops (s));
	    return true;
	  }
    }

  return false;
}

/* Return true if the RHS of statement S has side effects.
   We may use it to determine if it is admissable to replace
   an assignment or call with a copy of a previously-computed
   value.  In such cases, side-effects due the the LHS are
   preserved.  */

bool
gimple_rhs_has_side_effects (const_gimple s)
{
  unsigned i;

  if (is_gimple_call (s))
    {
      unsigned nargs = gimple_call_num_args (s);

      if (!(gimple_call_flags (s) & (ECF_CONST | ECF_PURE)))
        return true;

      /* We cannot use gimple_has_volatile_ops here,
         because we must ignore a volatile LHS.  */
      if (TREE_SIDE_EFFECTS (gimple_call_fn (s))
          || TREE_THIS_VOLATILE (gimple_call_fn (s)))
	{
	  gcc_assert (gimple_has_volatile_ops (s));
	  return true;
	}

      for (i = 0; i < nargs; i++)
        if (TREE_SIDE_EFFECTS (gimple_call_arg (s, i))
            || TREE_THIS_VOLATILE (gimple_call_arg (s, i)))
          return true;

      return false;
    }
  else if (is_gimple_assign (s))
    {
      /* Skip the first operand, the LHS. */
      for (i = 1; i < gimple_num_ops (s); i++)
	if (TREE_SIDE_EFFECTS (gimple_op (s, i))
            || TREE_THIS_VOLATILE (gimple_op (s, i)))
	  {
	    gcc_assert (gimple_has_volatile_ops (s));
	    return true;
	  }
    }
  else if (is_gimple_debug (s))
    return false;
  else
    {
      /* For statements without an LHS, examine all arguments.  */
      for (i = 0; i < gimple_num_ops (s); i++)
	if (TREE_SIDE_EFFECTS (gimple_op (s, i))
            || TREE_THIS_VOLATILE (gimple_op (s, i)))
	  {
	    gcc_assert (gimple_has_volatile_ops (s));
	    return true;
	  }
    }

  return false;
}

/* Helper for gimple_could_trap_p and gimple_assign_rhs_could_trap_p.
   Return true if S can trap.  When INCLUDE_MEM is true, check whether
   the memory operations could trap.  When INCLUDE_STORES is true and
   S is a GIMPLE_ASSIGN, the LHS of the assignment is also checked.  */

bool
gimple_could_trap_p_1 (gimple s, bool include_mem, bool include_stores)
{
  tree t, div = NULL_TREE;
  enum tree_code op;

  if (include_mem)
    {
      unsigned i, start = (is_gimple_assign (s) && !include_stores) ? 1 : 0;

      for (i = start; i < gimple_num_ops (s); i++)
	if (tree_could_trap_p (gimple_op (s, i)))
	  return true;
    }

  switch (gimple_code (s))
    {
    case GIMPLE_ASM:
      return gimple_asm_volatile_p (s);

    case GIMPLE_CALL:
      t = gimple_call_fndecl (s);
      /* Assume that calls to weak functions may trap.  */
      if (!t || !DECL_P (t) || DECL_WEAK (t))
	return true;
      return false;

    case GIMPLE_ASSIGN:
      t = gimple_expr_type (s);
      op = gimple_assign_rhs_code (s);
      if (get_gimple_rhs_class (op) == GIMPLE_BINARY_RHS)
	div = gimple_assign_rhs2 (s);
      return (operation_could_trap_p (op, FLOAT_TYPE_P (t),
				      (INTEGRAL_TYPE_P (t)
				       && TYPE_OVERFLOW_TRAPS (t)),
				      div));

    default:
      break;
    }

  return false;
}

/* Return true if statement S can trap.  */

bool
gimple_could_trap_p (gimple s)
{
  return gimple_could_trap_p_1 (s, true, true);
}

/* Return true if RHS of a GIMPLE_ASSIGN S can trap.  */

bool
gimple_assign_rhs_could_trap_p (gimple s)
{
  gcc_assert (is_gimple_assign (s));
  return gimple_could_trap_p_1 (s, true, false);
}


/* Print debugging information for gimple stmts generated.  */

void
dump_gimple_statistics (void)
{
#ifdef GATHER_STATISTICS
  int i, total_tuples = 0, total_bytes = 0;

  fprintf (stderr, "\nGIMPLE statements\n");
  fprintf (stderr, "Kind                   Stmts      Bytes\n");
  fprintf (stderr, "---------------------------------------\n");
  for (i = 0; i < (int) gimple_alloc_kind_all; ++i)
    {
      fprintf (stderr, "%-20s %7d %10d\n", gimple_alloc_kind_names[i],
	  gimple_alloc_counts[i], gimple_alloc_sizes[i]);
      total_tuples += gimple_alloc_counts[i];
      total_bytes += gimple_alloc_sizes[i];
    }
  fprintf (stderr, "---------------------------------------\n");
  fprintf (stderr, "%-20s %7d %10d\n", "Total", total_tuples, total_bytes);
  fprintf (stderr, "---------------------------------------\n");
#else
  fprintf (stderr, "No gimple statistics\n");
#endif
}


/* Return the number of operands needed on the RHS of a GIMPLE
   assignment for an expression with tree code CODE.  */

unsigned
get_gimple_rhs_num_ops (enum tree_code code)
{
  enum gimple_rhs_class rhs_class = get_gimple_rhs_class (code);

  if (rhs_class == GIMPLE_UNARY_RHS || rhs_class == GIMPLE_SINGLE_RHS)
    return 1;
  else if (rhs_class == GIMPLE_BINARY_RHS)
    return 2;
  else if (rhs_class == GIMPLE_TERNARY_RHS)
    return 3;
  else
    gcc_unreachable ();
}

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   			    \
  (unsigned char)							    \
  ((TYPE) == tcc_unary ? GIMPLE_UNARY_RHS				    \
   : ((TYPE) == tcc_binary						    \
      || (TYPE) == tcc_comparison) ? GIMPLE_BINARY_RHS   		    \
   : ((TYPE) == tcc_constant						    \
      || (TYPE) == tcc_declaration					    \
      || (TYPE) == tcc_reference) ? GIMPLE_SINGLE_RHS			    \
   : ((SYM) == TRUTH_AND_EXPR						    \
      || (SYM) == TRUTH_OR_EXPR						    \
      || (SYM) == TRUTH_XOR_EXPR) ? GIMPLE_BINARY_RHS			    \
   : (SYM) == TRUTH_NOT_EXPR ? GIMPLE_UNARY_RHS				    \
   : ((SYM) == WIDEN_MULT_PLUS_EXPR					    \
      || (SYM) == WIDEN_MULT_MINUS_EXPR					    \
      || (SYM) == FMA_EXPR) ? GIMPLE_TERNARY_RHS			    \
   : ((SYM) == COND_EXPR						    \
      || (SYM) == CONSTRUCTOR						    \
      || (SYM) == OBJ_TYPE_REF						    \
      || (SYM) == ASSERT_EXPR						    \
      || (SYM) == ADDR_EXPR						    \
      || (SYM) == WITH_SIZE_EXPR					    \
      || (SYM) == SSA_NAME						    \
      || (SYM) == POLYNOMIAL_CHREC					    \
      || (SYM) == DOT_PROD_EXPR						    \
      || (SYM) == VEC_COND_EXPR						    \
      || (SYM) == REALIGN_LOAD_EXPR) ? GIMPLE_SINGLE_RHS		    \
   : GIMPLE_INVALID_RHS),
#define END_OF_BASE_TREE_CODES (unsigned char) GIMPLE_INVALID_RHS,

const unsigned char gimple_rhs_class_table[] = {
#include "all-tree.def"
};

#undef DEFTREECODE
#undef END_OF_BASE_TREE_CODES

/* For the definitive definition of GIMPLE, see doc/tree-ssa.texi.  */

/* Validation of GIMPLE expressions.  */

/* Returns true iff T is a valid RHS for an assignment to a renamed
   user -- or front-end generated artificial -- variable.  */

bool
is_gimple_reg_rhs (tree t)
{
  return get_gimple_rhs_class (TREE_CODE (t)) != GIMPLE_INVALID_RHS;
}

/* Returns true iff T is a valid RHS for an assignment to an un-renamed
   LHS, or for a call argument.  */

bool
is_gimple_mem_rhs (tree t)
{
  /* If we're dealing with a renamable type, either source or dest must be
     a renamed variable.  */
  if (is_gimple_reg_type (TREE_TYPE (t)))
    return is_gimple_val (t);
  else
    return is_gimple_val (t) || is_gimple_lvalue (t);
}

/*  Return true if T is a valid LHS for a GIMPLE assignment expression.  */

bool
is_gimple_lvalue (tree t)
{
  return (is_gimple_addressable (t)
	  || TREE_CODE (t) == WITH_SIZE_EXPR
	  /* These are complex lvalues, but don't have addresses, so they
	     go here.  */
	  || TREE_CODE (t) == BIT_FIELD_REF);
}

/*  Return true if T is a GIMPLE condition.  */

bool
is_gimple_condexpr (tree t)
{
  return (is_gimple_val (t) || (COMPARISON_CLASS_P (t)
				&& !tree_could_trap_p (t)
				&& is_gimple_val (TREE_OPERAND (t, 0))
				&& is_gimple_val (TREE_OPERAND (t, 1))));
}

/*  Return true if T is something whose address can be taken.  */

bool
is_gimple_addressable (tree t)
{
  return (is_gimple_id (t) || handled_component_p (t)
	  || TREE_CODE (t) == MEM_REF);
}

/* Return true if T is a valid gimple constant.  */

bool
is_gimple_constant (const_tree t)
{
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
      return true;

    /* Vector constant constructors are gimple invariant.  */
    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
	return TREE_CONSTANT (t);
      else
	return false;

    default:
      return false;
    }
}

/* Return true if T is a gimple address.  */

bool
is_gimple_address (const_tree t)
{
  tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = TREE_OPERAND (t, 0);
  while (handled_component_p (op))
    {
      if ((TREE_CODE (op) == ARRAY_REF
	   || TREE_CODE (op) == ARRAY_RANGE_REF)
	  && !is_gimple_val (TREE_OPERAND (op, 1)))
	    return false;

      op = TREE_OPERAND (op, 0);
    }

  if (CONSTANT_CLASS_P (op) || TREE_CODE (op) == MEM_REF)
    return true;

  switch (TREE_CODE (op))
    {
    case PARM_DECL:
    case RESULT_DECL:
    case LABEL_DECL:
    case FUNCTION_DECL:
    case VAR_DECL:
    case CONST_DECL:
      return true;

    default:
      return false;
    }
}

/* Strip out all handled components that produce invariant
   offsets.  */

static const_tree
strip_invariant_refs (const_tree op)
{
  while (handled_component_p (op))
    {
      switch (TREE_CODE (op))
	{
	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (!is_gimple_constant (TREE_OPERAND (op, 1))
	      || TREE_OPERAND (op, 2) != NULL_TREE
	      || TREE_OPERAND (op, 3) != NULL_TREE)
	    return NULL;
	  break;

	case COMPONENT_REF:
	  if (TREE_OPERAND (op, 2) != NULL_TREE)
	    return NULL;
	  break;

	default:;
	}
      op = TREE_OPERAND (op, 0);
    }

  return op;
}

/* Return true if T is a gimple invariant address.  */

bool
is_gimple_invariant_address (const_tree t)
{
  const_tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = strip_invariant_refs (TREE_OPERAND (t, 0));
  if (!op)
    return false;

  if (TREE_CODE (op) == MEM_REF)
    {
      const_tree op0 = TREE_OPERAND (op, 0);
      return (TREE_CODE (op0) == ADDR_EXPR
	      && (CONSTANT_CLASS_P (TREE_OPERAND (op0, 0))
		  || decl_address_invariant_p (TREE_OPERAND (op0, 0))));
    }

  return CONSTANT_CLASS_P (op) || decl_address_invariant_p (op);
}

/* Return true if T is a gimple invariant address at IPA level
   (so addresses of variables on stack are not allowed).  */

bool
is_gimple_ip_invariant_address (const_tree t)
{
  const_tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = strip_invariant_refs (TREE_OPERAND (t, 0));

  return op && (CONSTANT_CLASS_P (op) || decl_address_ip_invariant_p (op));
}

/* Return true if T is a GIMPLE minimal invariant.  It's a restricted
   form of function invariant.  */

bool
is_gimple_min_invariant (const_tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    return is_gimple_invariant_address (t);

  return is_gimple_constant (t);
}

/* Return true if T is a GIMPLE interprocedural invariant.  It's a restricted
   form of gimple minimal invariant.  */

bool
is_gimple_ip_invariant (const_tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    return is_gimple_ip_invariant_address (t);

  return is_gimple_constant (t);
}

/* Return true if T looks like a valid GIMPLE statement.  */

bool
is_gimple_stmt (tree t)
{
  const enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case NOP_EXPR:
      /* The only valid NOP_EXPR is the empty statement.  */
      return IS_EMPTY_STMT (t);

    case BIND_EXPR:
    case COND_EXPR:
      /* These are only valid if they're void.  */
      return TREE_TYPE (t) == NULL || VOID_TYPE_P (TREE_TYPE (t));

    case SWITCH_EXPR:
    case GOTO_EXPR:
    case RETURN_EXPR:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
    case EH_FILTER_EXPR:
    case CATCH_EXPR:
    case ASM_EXPR:
    case STATEMENT_LIST:
    case OMP_PARALLEL:
    case OMP_FOR:
    case OMP_SECTIONS:
    case OMP_SECTION:
    case OMP_SINGLE:
    case OMP_MASTER:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_TASK:
      /* These are always void.  */
      return true;

    case CALL_EXPR:
    case MODIFY_EXPR:
    case PREDICT_EXPR:
      /* These are valid regardless of their type.  */
      return true;

    default:
      return false;
    }
}

/* Return true if T is a variable.  */

bool
is_gimple_variable (tree t)
{
  return (TREE_CODE (t) == VAR_DECL
	  || TREE_CODE (t) == PARM_DECL
	  || TREE_CODE (t) == RESULT_DECL
	  || TREE_CODE (t) == SSA_NAME);
}

/*  Return true if T is a GIMPLE identifier (something with an address).  */

bool
is_gimple_id (tree t)
{
  return (is_gimple_variable (t)
	  || TREE_CODE (t) == FUNCTION_DECL
	  || TREE_CODE (t) == LABEL_DECL
	  || TREE_CODE (t) == CONST_DECL
	  /* Allow string constants, since they are addressable.  */
	  || TREE_CODE (t) == STRING_CST);
}

/* Return true if TYPE is a suitable type for a scalar register variable.  */

bool
is_gimple_reg_type (tree type)
{
  return !AGGREGATE_TYPE_P (type);
}

/* Return true if T is a non-aggregate register variable.  */

bool
is_gimple_reg (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    t = SSA_NAME_VAR (t);

  if (!is_gimple_variable (t))
    return false;

  if (!is_gimple_reg_type (TREE_TYPE (t)))
    return false;

  /* A volatile decl is not acceptable because we can't reuse it as
     needed.  We need to copy it into a temp first.  */
  if (TREE_THIS_VOLATILE (t))
    return false;

  /* We define "registers" as things that can be renamed as needed,
     which with our infrastructure does not apply to memory.  */
  if (needs_to_live_in_memory (t))
    return false;

  /* Hard register variables are an interesting case.  For those that
     are call-clobbered, we don't know where all the calls are, since
     we don't (want to) take into account which operations will turn
     into libcalls at the rtl level.  For those that are call-saved,
     we don't currently model the fact that calls may in fact change
     global hard registers, nor do we examine ASM_CLOBBERS at the tree
     level, and so miss variable changes that might imply.  All around,
     it seems safest to not do too much optimization with these at the
     tree level at all.  We'll have to rely on the rtl optimizers to
     clean this up, as there we've got all the appropriate bits exposed.  */
  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    return false;

  /* Complex and vector values must have been put into SSA-like form.
     That is, no assignments to the individual components.  */
  if (TREE_CODE (TREE_TYPE (t)) == COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
    return DECL_GIMPLE_REG_P (t);

  return true;
}


/* Return true if T is a GIMPLE variable whose address is not needed.  */

bool
is_gimple_non_addressable (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    t = SSA_NAME_VAR (t);

  return (is_gimple_variable (t) && ! needs_to_live_in_memory (t));
}

/* Return true if T is a GIMPLE rvalue, i.e. an identifier or a constant.  */

bool
is_gimple_val (tree t)
{
  /* Make loads from volatiles and memory vars explicit.  */
  if (is_gimple_variable (t)
      && is_gimple_reg_type (TREE_TYPE (t))
      && !is_gimple_reg (t))
    return false;

  return (is_gimple_variable (t) || is_gimple_min_invariant (t));
}

/* Similarly, but accept hard registers as inputs to asm statements.  */

bool
is_gimple_asm_val (tree t)
{
  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    return true;

  return is_gimple_val (t);
}

/* Return true if T is a GIMPLE minimal lvalue.  */

bool
is_gimple_min_lval (tree t)
{
  if (!(t = CONST_CAST_TREE (strip_invariant_refs (t))))
    return false;
  return (is_gimple_id (t) || TREE_CODE (t) == MEM_REF);
}

/* Return true if T is a valid function operand of a CALL_EXPR.  */

bool
is_gimple_call_addr (tree t)
{
  return (TREE_CODE (t) == OBJ_TYPE_REF || is_gimple_val (t));
}

/* Return true if T is a valid address operand of a MEM_REF.  */

bool
is_gimple_mem_ref_addr (tree t)
{
  return (is_gimple_reg (t)
	  || TREE_CODE (t) == INTEGER_CST
	  || (TREE_CODE (t) == ADDR_EXPR
	      && (CONSTANT_CLASS_P (TREE_OPERAND (t, 0))
		  || decl_address_invariant_p (TREE_OPERAND (t, 0)))));
}

/* If T makes a function call, return the corresponding CALL_EXPR operand.
   Otherwise, return NULL_TREE.  */

tree
get_call_expr_in (tree t)
{
  if (TREE_CODE (t) == MODIFY_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == CALL_EXPR)
    return t;
  return NULL_TREE;
}


/* Given a memory reference expression T, return its base address.
   The base address of a memory reference expression is the main
   object being referenced.  For instance, the base address for
   'array[i].fld[j]' is 'array'.  You can think of this as stripping
   away the offset part from a memory address.

   This function calls handled_component_p to strip away all the inner
   parts of the memory reference until it reaches the base object.  */

tree
get_base_address (tree t)
{
  while (handled_component_p (t))
    t = TREE_OPERAND (t, 0);

  if ((TREE_CODE (t) == MEM_REF
       || TREE_CODE (t) == TARGET_MEM_REF)
      && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR)
    t = TREE_OPERAND (TREE_OPERAND (t, 0), 0);

  if (TREE_CODE (t) == SSA_NAME
      || DECL_P (t)
      || TREE_CODE (t) == STRING_CST
      || TREE_CODE (t) == CONSTRUCTOR
      || INDIRECT_REF_P (t)
      || TREE_CODE (t) == MEM_REF
      || TREE_CODE (t) == TARGET_MEM_REF)
    return t;
  else
    return NULL_TREE;
}

void
recalculate_side_effects (tree t)
{
  enum tree_code code = TREE_CODE (t);
  int len = TREE_OPERAND_LENGTH (t);
  int i;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_expression:
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  /* All of these have side-effects, no matter what their
	     operands are.  */
	  return;

	default:
	  break;
	}
      /* Fall through.  */

    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:       /* a unary arithmetic expression */
    case tcc_binary:      /* a binary arithmetic expression */
    case tcc_reference:   /* a reference */
    case tcc_vl_exp:        /* a function call */
      TREE_SIDE_EFFECTS (t) = TREE_THIS_VOLATILE (t);
      for (i = 0; i < len; ++i)
	{
	  tree op = TREE_OPERAND (t, i);
	  if (op && TREE_SIDE_EFFECTS (op))
	    TREE_SIDE_EFFECTS (t) = 1;
	}
      break;

    case tcc_constant:
      /* No side-effects.  */
      return;

    default:
      gcc_unreachable ();
   }
}

/* Canonicalize a tree T for use in a COND_EXPR as conditional.  Returns
   a canonicalized tree that is valid for a COND_EXPR or NULL_TREE, if
   we failed to create one.  */

tree
canonicalize_cond_expr_cond (tree t)
{
  /* Strip conversions around boolean operations.  */
  if (CONVERT_EXPR_P (t)
      && truth_value_p (TREE_CODE (TREE_OPERAND (t, 0))))
    t = TREE_OPERAND (t, 0);

  /* For (bool)x use x != 0.  */
  if (CONVERT_EXPR_P (t)
      && TREE_CODE (TREE_TYPE (t)) == BOOLEAN_TYPE)
    {
      tree top0 = TREE_OPERAND (t, 0);
      t = build2 (NE_EXPR, TREE_TYPE (t),
		  top0, build_int_cst (TREE_TYPE (top0), 0));
    }
  /* For !x use x == 0.  */
  else if (TREE_CODE (t) == TRUTH_NOT_EXPR)
    {
      tree top0 = TREE_OPERAND (t, 0);
      t = build2 (EQ_EXPR, TREE_TYPE (t),
		  top0, build_int_cst (TREE_TYPE (top0), 0));
    }
  /* For cmp ? 1 : 0 use cmp.  */
  else if (TREE_CODE (t) == COND_EXPR
	   && COMPARISON_CLASS_P (TREE_OPERAND (t, 0))
	   && integer_onep (TREE_OPERAND (t, 1))
	   && integer_zerop (TREE_OPERAND (t, 2)))
    {
      tree top0 = TREE_OPERAND (t, 0);
      t = build2 (TREE_CODE (top0), TREE_TYPE (t),
		  TREE_OPERAND (top0, 0), TREE_OPERAND (top0, 1));
    }

  if (is_gimple_condexpr (t))
    return t;

  return NULL_TREE;
}

/* Build a GIMPLE_CALL identical to STMT but skipping the arguments in
   the positions marked by the set ARGS_TO_SKIP.  */

gimple
gimple_call_copy_skip_args (gimple stmt, bitmap args_to_skip)
{
  int i;
  tree fn = gimple_call_fn (stmt);
  int nargs = gimple_call_num_args (stmt);
  VEC(tree, heap) *vargs = VEC_alloc (tree, heap, nargs);
  gimple new_stmt;

  for (i = 0; i < nargs; i++)
    if (!bitmap_bit_p (args_to_skip, i))
      VEC_quick_push (tree, vargs, gimple_call_arg (stmt, i));

  new_stmt = gimple_build_call_vec (fn, vargs);
  VEC_free (tree, heap, vargs);
  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
  gimple_set_vdef (new_stmt, gimple_vdef (stmt));

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));

  gimple_set_modified (new_stmt, true);

  return new_stmt;
}


static hashval_t gimple_type_hash_1 (const void *, enum gtc_mode);

/* Structure used to maintain a cache of some type pairs compared by
   gimple_types_compatible_p when comparing aggregate types.  There are
   three possible values for SAME_P:

   	-2: The pair (T1, T2) has just been inserted in the table.
	 0: T1 and T2 are different types.
	 1: T1 and T2 are the same type.

   The two elements in the SAME_P array are indexed by the comparison
   mode gtc_mode.  */

struct type_pair_d
{
  unsigned int uid1;
  unsigned int uid2;
  signed char same_p[2];
};
typedef struct type_pair_d *type_pair_t;

DEF_VEC_P(type_pair_t);
DEF_VEC_ALLOC_P(type_pair_t,heap);

/* Return a hash value for the type pair pointed-to by P.  */

static hashval_t
type_pair_hash (const void *p)
{
  const struct type_pair_d *pair = (const struct type_pair_d *) p;
  hashval_t val1 = pair->uid1;
  hashval_t val2 = pair->uid2;
  return (iterative_hash_hashval_t (val2, val1)
	  ^ iterative_hash_hashval_t (val1, val2));
}

/* Compare two type pairs pointed-to by P1 and P2.  */

static int
type_pair_eq (const void *p1, const void *p2)
{
  const struct type_pair_d *pair1 = (const struct type_pair_d *) p1;
  const struct type_pair_d *pair2 = (const struct type_pair_d *) p2;
  return ((pair1->uid1 == pair2->uid1 && pair1->uid2 == pair2->uid2)
	  || (pair1->uid1 == pair2->uid2 && pair1->uid2 == pair2->uid1));
}

/* Lookup the pair of types T1 and T2 in *VISITED_P.  Insert a new
   entry if none existed.  */

static type_pair_t
lookup_type_pair (tree t1, tree t2, htab_t *visited_p, struct obstack *ob_p)
{
  struct type_pair_d pair;
  type_pair_t p;
  void **slot;

  if (*visited_p == NULL)
    {
      *visited_p = htab_create (251, type_pair_hash, type_pair_eq, NULL);
      gcc_obstack_init (ob_p);
    }

  pair.uid1 = TYPE_UID (t1);
  pair.uid2 = TYPE_UID (t2);
  slot = htab_find_slot (*visited_p, &pair, INSERT);

  if (*slot)
    p = *((type_pair_t *) slot);
  else
    {
      p = XOBNEW (ob_p, struct type_pair_d);
      p->uid1 = TYPE_UID (t1);
      p->uid2 = TYPE_UID (t2);
      p->same_p[0] = -2;
      p->same_p[1] = -2;
      *slot = (void *) p;
    }

  return p;
}

/* Per pointer state for the SCC finding.  The on_sccstack flag
   is not strictly required, it is true when there is no hash value
   recorded for the type and false otherwise.  But querying that
   is slower.  */

struct sccs
{
  unsigned int dfsnum;
  unsigned int low;
  bool on_sccstack;
  union {
    hashval_t hash;
    signed char same_p;
  } u;
};

static unsigned int next_dfs_num;
static unsigned int gtc_next_dfs_num;


/* GIMPLE type merging cache.  A direct-mapped cache based on TYPE_UID.  */

typedef struct GTY(()) gimple_type_leader_entry_s {
  tree type;
  tree leader;
} gimple_type_leader_entry;

#define GIMPLE_TYPE_LEADER_SIZE 16381
static GTY((length("GIMPLE_TYPE_LEADER_SIZE"))) gimple_type_leader_entry
  *gimple_type_leader;

/* Lookup an existing leader for T and return it or NULL_TREE, if
   there is none in the cache.  */

static tree
gimple_lookup_type_leader (tree t)
{
  gimple_type_leader_entry *leader;

  if (!gimple_type_leader)
    return NULL_TREE;

  leader = &gimple_type_leader[TYPE_UID (t) % GIMPLE_TYPE_LEADER_SIZE];
  if (leader->type != t)
    return NULL_TREE;

  return leader->leader;
}

/* Return true if T1 and T2 have the same name.  If FOR_COMPLETION_P is
   true then if any type has no name return false, otherwise return
   true if both types have no names.  */

static bool
compare_type_names_p (tree t1, tree t2, bool for_completion_p)
{
  tree name1 = TYPE_NAME (t1);
  tree name2 = TYPE_NAME (t2);

  /* Consider anonymous types all unique for completion.  */
  if (for_completion_p
      && (!name1 || !name2))
    return false;

  if (name1 && TREE_CODE (name1) == TYPE_DECL)
    {
      name1 = DECL_NAME (name1);
      if (for_completion_p
	  && !name1)
	return false;
    }
  gcc_assert (!name1 || TREE_CODE (name1) == IDENTIFIER_NODE);

  if (name2 && TREE_CODE (name2) == TYPE_DECL)
    {
      name2 = DECL_NAME (name2);
      if (for_completion_p
	  && !name2)
	return false;
    }
  gcc_assert (!name2 || TREE_CODE (name2) == IDENTIFIER_NODE);

  /* Identifiers can be compared with pointer equality rather
     than a string comparison.  */
  if (name1 == name2)
    return true;

  return false;
}

/* Return true if the field decls F1 and F2 are at the same offset.

   This is intended to be used on GIMPLE types only.  In order to
   compare GENERIC types, use fields_compatible_p instead.  */

bool
gimple_compare_field_offset (tree f1, tree f2)
{
  if (DECL_OFFSET_ALIGN (f1) == DECL_OFFSET_ALIGN (f2))
    {
      tree offset1 = DECL_FIELD_OFFSET (f1);
      tree offset2 = DECL_FIELD_OFFSET (f2);
      return ((offset1 == offset2
	       /* Once gimplification is done, self-referential offsets are
		  instantiated as operand #2 of the COMPONENT_REF built for
		  each access and reset.  Therefore, they are not relevant
		  anymore and fields are interchangeable provided that they
		  represent the same access.  */
	       || (TREE_CODE (offset1) == PLACEHOLDER_EXPR
		   && TREE_CODE (offset2) == PLACEHOLDER_EXPR
		   && (DECL_SIZE (f1) == DECL_SIZE (f2)
		       || (TREE_CODE (DECL_SIZE (f1)) == PLACEHOLDER_EXPR
			   && TREE_CODE (DECL_SIZE (f2)) == PLACEHOLDER_EXPR)
		       || operand_equal_p (DECL_SIZE (f1), DECL_SIZE (f2), 0))
		   && DECL_ALIGN (f1) == DECL_ALIGN (f2))
	       || operand_equal_p (offset1, offset2, 0))
	      && tree_int_cst_equal (DECL_FIELD_BIT_OFFSET (f1),
				     DECL_FIELD_BIT_OFFSET (f2)));
    }

  /* Fortran and C do not always agree on what DECL_OFFSET_ALIGN
     should be, so handle differing ones specially by decomposing
     the offset into a byte and bit offset manually.  */
  if (host_integerp (DECL_FIELD_OFFSET (f1), 0)
      && host_integerp (DECL_FIELD_OFFSET (f2), 0))
    {
      unsigned HOST_WIDE_INT byte_offset1, byte_offset2;
      unsigned HOST_WIDE_INT bit_offset1, bit_offset2;
      bit_offset1 = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (f1));
      byte_offset1 = (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (f1))
		      + bit_offset1 / BITS_PER_UNIT);
      bit_offset2 = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (f2));
      byte_offset2 = (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (f2))
		      + bit_offset2 / BITS_PER_UNIT);
      if (byte_offset1 != byte_offset2)
	return false;
      return bit_offset1 % BITS_PER_UNIT == bit_offset2 % BITS_PER_UNIT;
    }

  return false;
}

/* If the type T1 and the type T2 are a complete and an incomplete
   variant of the same type return true.  */

static bool
gimple_compatible_complete_and_incomplete_subtype_p (tree t1, tree t2)
{
  /* If one pointer points to an incomplete type variant of
     the other pointed-to type they are the same.  */
  if (TREE_CODE (t1) == TREE_CODE (t2)
      && RECORD_OR_UNION_TYPE_P (t1)
      && (!COMPLETE_TYPE_P (t1)
	  || !COMPLETE_TYPE_P (t2))
      && TYPE_QUALS (t1) == TYPE_QUALS (t2)
      && compare_type_names_p (TYPE_MAIN_VARIANT (t1),
			       TYPE_MAIN_VARIANT (t2), true))
    return true;
  return false;
}

static bool
gimple_types_compatible_p_1 (tree, tree, enum gtc_mode, type_pair_t,
			     VEC(type_pair_t, heap) **,
			     struct pointer_map_t *, struct obstack *);

/* DFS visit the edge from the callers type pair with state *STATE to
   the pair T1, T2 while operating in FOR_MERGING_P mode.
   Update the merging status if it is not part of the SCC containing the
   callers pair and return it.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static bool
gtc_visit (tree t1, tree t2, enum gtc_mode mode,
	   struct sccs *state,
	   VEC(type_pair_t, heap) **sccstack,
	   struct pointer_map_t *sccstate,
	   struct obstack *sccstate_obstack)
{
  struct sccs *cstate = NULL;
  type_pair_t p;
  void **slot;

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* If the types have been previously registered and found equal
     they still are.  */
  if (mode == GTC_MERGE)
    {
      tree leader1 = gimple_lookup_type_leader (t1);
      tree leader2 = gimple_lookup_type_leader (t2);
      if (leader1 == t2
	  || t1 == leader2
	  || (leader1 && leader1 == leader2))
	return true;
    }
  else if (mode == GTC_DIAG)
    {
      if (TYPE_CANONICAL (t1)
	  && TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2))
	return true;
    }

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Can't be the same type if they have different CV qualifiers.  */
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return false;

  /* Void types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE)
    return true;

  /* Do some simple checks before doing three hashtable queries.  */
  if (INTEGRAL_TYPE_P (t1)
      || SCALAR_FLOAT_TYPE_P (t1)
      || FIXED_POINT_TYPE_P (t1)
      || TREE_CODE (t1) == VECTOR_TYPE
      || TREE_CODE (t1) == COMPLEX_TYPE
      || TREE_CODE (t1) == OFFSET_TYPE)
    {
      /* Can't be the same type if they have different alignment,
	 sign, precision or mode.  */
      if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
	  || TYPE_PRECISION (t1) != TYPE_PRECISION (t2)
	  || TYPE_MODE (t1) != TYPE_MODE (t2)
	  || TYPE_UNSIGNED (t1) != TYPE_UNSIGNED (t2))
	return false;

      if (TREE_CODE (t1) == INTEGER_TYPE
	  && (TYPE_IS_SIZETYPE (t1) != TYPE_IS_SIZETYPE (t2)
	      || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)))
	return false;

      /* That's all we need to check for float and fixed-point types.  */
      if (SCALAR_FLOAT_TYPE_P (t1)
	  || FIXED_POINT_TYPE_P (t1))
	return true;

      /* For integral types fall thru to more complex checks.  */
    }

  else if (AGGREGATE_TYPE_P (t1) || POINTER_TYPE_P (t1))
    {
      /* Can't be the same type if they have different alignment or mode.  */
      if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
	  || TYPE_MODE (t1) != TYPE_MODE (t2))
	return false;
    }

  /* If the hash values of t1 and t2 are different the types can't
     possibly be the same.  This helps keeping the type-pair hashtable
     small, only tracking comparisons for hash collisions.  */
  if (gimple_type_hash_1 (t1, mode) != gimple_type_hash_1 (t2, mode))
    return false;

  /* Allocate a new cache entry for this comparison.  */
  p = lookup_type_pair (t1, t2, &gtc_visited, &gtc_ob);
  if (p->same_p[mode] == 0 || p->same_p[mode] == 1)
    {
      /* We have already decided whether T1 and T2 are the
	 same, return the cached result.  */
      return p->same_p[mode] == 1;
    }

  if ((slot = pointer_map_contains (sccstate, p)) != NULL)
    cstate = (struct sccs *)*slot;
  /* Not yet visited.  DFS recurse.  */
  if (!cstate)
    {
      gimple_types_compatible_p_1 (t1, t2, mode, p,
				   sccstack, sccstate, sccstate_obstack);
      cstate = (struct sccs *)* pointer_map_contains (sccstate, p);
      state->low = MIN (state->low, cstate->low);
    }
  /* If the type is still on the SCC stack adjust the parents low.  */
  if (cstate->dfsnum < state->dfsnum
      && cstate->on_sccstack)
    state->low = MIN (cstate->dfsnum, state->low);

  /* Return the current lattice value.  We start with an equality
     assumption so types part of a SCC will be optimistically
     treated equal unless proven otherwise.  */
  return cstate->u.same_p;
}

/* Worker for gimple_types_compatible.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static bool
gimple_types_compatible_p_1 (tree t1, tree t2, enum gtc_mode mode,
			     type_pair_t p,
			     VEC(type_pair_t, heap) **sccstack,
			     struct pointer_map_t *sccstate,
			     struct obstack *sccstate_obstack)
{
  struct sccs *state;

  gcc_assert (p->same_p[mode] == -2);

  state = XOBNEW (sccstate_obstack, struct sccs);
  *pointer_map_insert (sccstate, p) = state;

  VEC_safe_push (type_pair_t, heap, *sccstack, p);
  state->dfsnum = gtc_next_dfs_num++;
  state->low = state->dfsnum;
  state->on_sccstack = true;
  /* Start with an equality assumption.  As we DFS recurse into child
     SCCs this assumption may get revisited.  */
  state->u.same_p = 1;

  /* If their attributes are not the same they can't be the same type.  */
  if (!attribute_list_equal (TYPE_ATTRIBUTES (t1), TYPE_ATTRIBUTES (t2)))
    goto different_types;

  /* Do type-specific comparisons.  */
  switch (TREE_CODE (t1))
    {
    case VECTOR_TYPE:
    case COMPLEX_TYPE:
      if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2), mode,
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;
      goto same_types;

    case ARRAY_TYPE:
      /* Array types are the same if the element types are the same and
	 the number of elements are the same.  */
      if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2), mode,
		      state, sccstack, sccstate, sccstate_obstack)
	  || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)
	  || TYPE_NONALIASED_COMPONENT (t1) != TYPE_NONALIASED_COMPONENT (t2))
	goto different_types;
      else
	{
	  tree i1 = TYPE_DOMAIN (t1);
	  tree i2 = TYPE_DOMAIN (t2);

	  /* For an incomplete external array, the type domain can be
 	     NULL_TREE.  Check this condition also.  */
	  if (i1 == NULL_TREE && i2 == NULL_TREE)
	    goto same_types;
	  else if (i1 == NULL_TREE || i2 == NULL_TREE)
	    goto different_types;
	  /* If for a complete array type the possibly gimplified sizes
	     are different the types are different.  */
	  else if (((TYPE_SIZE (i1) != NULL) ^ (TYPE_SIZE (i2) != NULL))
		   || (TYPE_SIZE (i1)
		       && TYPE_SIZE (i2)
		       && !operand_equal_p (TYPE_SIZE (i1), TYPE_SIZE (i2), 0)))
	    goto different_types;
	  else
	    {
	      tree min1 = TYPE_MIN_VALUE (i1);
	      tree min2 = TYPE_MIN_VALUE (i2);
	      tree max1 = TYPE_MAX_VALUE (i1);
	      tree max2 = TYPE_MAX_VALUE (i2);

	      /* The minimum/maximum values have to be the same.  */
	      if ((min1 == min2
		   || (min1 && min2
		       && ((TREE_CODE (min1) == PLACEHOLDER_EXPR
			    && TREE_CODE (min2) == PLACEHOLDER_EXPR)
		           || operand_equal_p (min1, min2, 0))))
		  && (max1 == max2
		      || (max1 && max2
			  && ((TREE_CODE (max1) == PLACEHOLDER_EXPR
			       && TREE_CODE (max2) == PLACEHOLDER_EXPR)
			      || operand_equal_p (max1, max2, 0)))))
		goto same_types;
	      else
		goto different_types;
	    }
	}

    case METHOD_TYPE:
      /* Method types should belong to the same class.  */
      if (!gtc_visit (TYPE_METHOD_BASETYPE (t1), TYPE_METHOD_BASETYPE (t2),
		      mode, state, sccstack, sccstate, sccstate_obstack))
	goto different_types;

      /* Fallthru  */

    case FUNCTION_TYPE:
      /* Function types are the same if the return type and arguments types
	 are the same.  */
      if ((mode != GTC_DIAG
	   || !gimple_compatible_complete_and_incomplete_subtype_p
	         (TREE_TYPE (t1), TREE_TYPE (t2)))
	  && !gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2), mode,
			 state, sccstack, sccstate, sccstate_obstack))
	goto different_types;

      if (!targetm.comp_type_attributes (t1, t2))
	goto different_types;

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2))
	goto same_types;
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if ((mode == GTC_MERGE
		   || !gimple_compatible_complete_and_incomplete_subtype_p
		         (TREE_VALUE (parms1), TREE_VALUE (parms2)))
		  && !gtc_visit (TREE_VALUE (parms1), TREE_VALUE (parms2), mode,
				 state, sccstack, sccstate, sccstate_obstack))
		goto different_types;
	    }

	  if (parms1 || parms2)
	    goto different_types;

	  goto same_types;
	}

    case OFFSET_TYPE:
      {
	if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2), mode,
			state, sccstack, sccstate, sccstate_obstack)
	    || !gtc_visit (TYPE_OFFSET_BASETYPE (t1),
			   TYPE_OFFSET_BASETYPE (t2), mode,
			   state, sccstack, sccstate, sccstate_obstack))
	  goto different_types;

	goto same_types;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	/* If the two pointers have different ref-all attributes,
	   they can't be the same type.  */
	if (TYPE_REF_CAN_ALIAS_ALL (t1) != TYPE_REF_CAN_ALIAS_ALL (t2))
	  goto different_types;

	/* If one pointer points to an incomplete type variant of
	   the other pointed-to type they are the same.  */
	if (mode == GTC_DIAG
	    && gimple_compatible_complete_and_incomplete_subtype_p
	         (TREE_TYPE (t1), TREE_TYPE (t2)))
	  goto same_types;

	/* Otherwise, pointer and reference types are the same if the
	   pointed-to types are the same.  */
	if (gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2), mode,
		       state, sccstack, sccstate, sccstate_obstack))
	  goto same_types;

	goto different_types;
      }

    case NULLPTR_TYPE:
      /* There is only one decltype(nullptr).  */
      goto same_types;

    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
      {
	tree min1 = TYPE_MIN_VALUE (t1);
	tree max1 = TYPE_MAX_VALUE (t1);
	tree min2 = TYPE_MIN_VALUE (t2);
	tree max2 = TYPE_MAX_VALUE (t2);
	bool min_equal_p = false;
	bool max_equal_p = false;

	/* If either type has a minimum value, the other type must
	   have the same.  */
	if (min1 == NULL_TREE && min2 == NULL_TREE)
	  min_equal_p = true;
	else if (min1 && min2 && operand_equal_p (min1, min2, 0))
	  min_equal_p = true;

	/* Likewise, if either type has a maximum value, the other
	   type must have the same.  */
	if (max1 == NULL_TREE && max2 == NULL_TREE)
	  max_equal_p = true;
	else if (max1 && max2 && operand_equal_p (max1, max2, 0))
	  max_equal_p = true;

	if (!min_equal_p || !max_equal_p)
	  goto different_types;

	goto same_types;
      }

    case ENUMERAL_TYPE:
      {
	/* FIXME lto, we cannot check bounds on enumeral types because
	   different front ends will produce different values.
	   In C, enumeral types are integers, while in C++ each element
	   will have its own symbolic value.  We should decide how enums
	   are to be represented in GIMPLE and have each front end lower
	   to that.  */
	tree v1, v2;

	/* For enumeral types, all the values must be the same.  */
	if (TYPE_VALUES (t1) == TYPE_VALUES (t2))
	  goto same_types;

	for (v1 = TYPE_VALUES (t1), v2 = TYPE_VALUES (t2);
	     v1 && v2;
	     v1 = TREE_CHAIN (v1), v2 = TREE_CHAIN (v2))
	  {
	    tree c1 = TREE_VALUE (v1);
	    tree c2 = TREE_VALUE (v2);

	    if (TREE_CODE (c1) == CONST_DECL)
	      c1 = DECL_INITIAL (c1);

	    if (TREE_CODE (c2) == CONST_DECL)
	      c2 = DECL_INITIAL (c2);

	    if (tree_int_cst_equal (c1, c2) != 1)
	      goto different_types;

	    if (mode == GTC_MERGE && TREE_PURPOSE (v1) != TREE_PURPOSE (v2))
	      goto different_types;
	  }

	/* If one enumeration has more values than the other, they
	   are not the same.  */
	if (v1 || v2)
	  goto different_types;

	goto same_types;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* The struct tags shall compare equal.  */
	if (mode == GTC_MERGE
	    && !compare_type_names_p (TYPE_MAIN_VARIANT (t1),
				      TYPE_MAIN_VARIANT (t2), false))
	  goto different_types;

	/* For aggregate types, all the fields must be the same.  */
	for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
	     f1 && f2;
	     f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	  {
	    /* The fields must have the same name, offset and type.  */
	    if ((mode == GTC_MERGE
		 && DECL_NAME (f1) != DECL_NAME (f2))
		|| DECL_NONADDRESSABLE_P (f1) != DECL_NONADDRESSABLE_P (f2)
		|| !gimple_compare_field_offset (f1, f2)
		|| !gtc_visit (TREE_TYPE (f1), TREE_TYPE (f2), mode,
			       state, sccstack, sccstate, sccstate_obstack))
	      goto different_types;
	  }

	/* If one aggregate has more fields than the other, they
	   are not the same.  */
	if (f1 || f2)
	  goto different_types;

	goto same_types;
      }

    default:
      gcc_unreachable ();
    }

  /* Common exit path for types that are not compatible.  */
different_types:
  state->u.same_p = 0;
  goto pop;

  /* Common exit path for types that are compatible.  */
same_types:
  gcc_assert (state->u.same_p == 1);

pop:
  if (state->low == state->dfsnum)
    {
      type_pair_t x;

      /* Pop off the SCC and set its cache values to the final
         comparison result.  */
      do
	{
	  struct sccs *cstate;
	  x = VEC_pop (type_pair_t, *sccstack);
	  cstate = (struct sccs *)*pointer_map_contains (sccstate, x);
	  cstate->on_sccstack = false;
	  x->same_p[mode] = state->u.same_p;
	}
      while (x != p);
    }

  return state->u.same_p;
}

/* Return true iff T1 and T2 are structurally identical.  When
   FOR_MERGING_P is true the an incomplete type and a complete type
   are considered different, otherwise they are considered compatible.  */

bool
gimple_types_compatible_p (tree t1, tree t2, enum gtc_mode mode)
{
  VEC(type_pair_t, heap) *sccstack = NULL;
  struct pointer_map_t *sccstate;
  struct obstack sccstate_obstack;
  type_pair_t p = NULL;
  bool res;

  /* Before starting to set up the SCC machinery handle simple cases.  */

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* If the types have been previously registered and found equal
     they still are.  */
  if (mode == GTC_MERGE)
    {
      tree leader1 = gimple_lookup_type_leader (t1);
      tree leader2 = gimple_lookup_type_leader (t2);
      if (leader1 == t2
	  || t1 == leader2
	  || (leader1 && leader1 == leader2))
	return true;
    }
  else if (mode == GTC_DIAG)
    {
      if (TYPE_CANONICAL (t1)
	  && TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2))
	return true;
    }

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Can't be the same type if they have different CV qualifiers.  */
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return false;

  /* Void types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE)
    return true;

  /* Do some simple checks before doing three hashtable queries.  */
  if (INTEGRAL_TYPE_P (t1)
      || SCALAR_FLOAT_TYPE_P (t1)
      || FIXED_POINT_TYPE_P (t1)
      || TREE_CODE (t1) == VECTOR_TYPE
      || TREE_CODE (t1) == COMPLEX_TYPE
      || TREE_CODE (t1) == OFFSET_TYPE)
    {
      /* Can't be the same type if they have different alignment,
	 sign, precision or mode.  */
      if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
	  || TYPE_PRECISION (t1) != TYPE_PRECISION (t2)
	  || TYPE_MODE (t1) != TYPE_MODE (t2)
	  || TYPE_UNSIGNED (t1) != TYPE_UNSIGNED (t2))
	return false;

      if (TREE_CODE (t1) == INTEGER_TYPE
	  && (TYPE_IS_SIZETYPE (t1) != TYPE_IS_SIZETYPE (t2)
	      || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)))
	return false;

      /* That's all we need to check for float and fixed-point types.  */
      if (SCALAR_FLOAT_TYPE_P (t1)
	  || FIXED_POINT_TYPE_P (t1))
	return true;

      /* For integral types fall thru to more complex checks.  */
    }

  else if (AGGREGATE_TYPE_P (t1) || POINTER_TYPE_P (t1))
    {
      /* Can't be the same type if they have different alignment or mode.  */
      if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
	  || TYPE_MODE (t1) != TYPE_MODE (t2))
	return false;
    }

  /* If the hash values of t1 and t2 are different the types can't
     possibly be the same.  This helps keeping the type-pair hashtable
     small, only tracking comparisons for hash collisions.  */
  if (gimple_type_hash_1 (t1, mode) != gimple_type_hash_1 (t2, mode))
    return false;

  /* If we've visited this type pair before (in the case of aggregates
     with self-referential types), and we made a decision, return it.  */
  p = lookup_type_pair (t1, t2, &gtc_visited, &gtc_ob);
  if (p->same_p[mode] == 0 || p->same_p[mode] == 1)
    {
      /* We have already decided whether T1 and T2 are the
	 same, return the cached result.  */
      return p->same_p[mode] == 1;
    }

  /* Now set up the SCC machinery for the comparison.  */
  gtc_next_dfs_num = 1;
  sccstate = pointer_map_create ();
  gcc_obstack_init (&sccstate_obstack);
  res = gimple_types_compatible_p_1 (t1, t2, mode, p,
				     &sccstack, sccstate, &sccstate_obstack);
  VEC_free (type_pair_t, heap, sccstack);
  pointer_map_destroy (sccstate);
  obstack_free (&sccstate_obstack, NULL);

  return res;
}


static hashval_t
iterative_hash_gimple_type (tree, hashval_t, VEC(tree, heap) **,
			    struct pointer_map_t *, struct obstack *,
			    enum gtc_mode);

/* DFS visit the edge from the callers type with state *STATE to T.
   Update the callers type hash V with the hash for T if it is not part
   of the SCC containing the callers type and return it.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static hashval_t
visit (tree t, struct sccs *state, hashval_t v,
       VEC (tree, heap) **sccstack,
       struct pointer_map_t *sccstate,
       struct obstack *sccstate_obstack, enum gtc_mode mode)
{
  struct sccs *cstate = NULL;
  struct tree_int_map m;
  void **slot;

  /* If there is a hash value recorded for this type then it can't
     possibly be part of our parent SCC.  Simply mix in its hash.  */
  m.base.from = t;
  if ((slot = htab_find_slot (mode == GTC_MERGE
			      ? type_hash_cache : canonical_type_hash_cache,
			      &m, NO_INSERT))
      && *slot)
    return iterative_hash_hashval_t (((struct tree_int_map *) *slot)->to, v);

  if ((slot = pointer_map_contains (sccstate, t)) != NULL)
    cstate = (struct sccs *)*slot;
  if (!cstate)
    {
      hashval_t tem;
      /* Not yet visited.  DFS recurse.  */
      tem = iterative_hash_gimple_type (t, v,
					sccstack, sccstate, sccstate_obstack,
					mode);
      if (!cstate)
	cstate = (struct sccs *)* pointer_map_contains (sccstate, t);
      state->low = MIN (state->low, cstate->low);
      /* If the type is no longer on the SCC stack and thus is not part
         of the parents SCC mix in its hash value.  Otherwise we will
	 ignore the type for hashing purposes and return the unaltered
	 hash value.  */
      if (!cstate->on_sccstack)
	return tem;
    }
  if (cstate->dfsnum < state->dfsnum
      && cstate->on_sccstack)
    state->low = MIN (cstate->dfsnum, state->low);

  /* We are part of our parents SCC, skip this type during hashing
     and return the unaltered hash value.  */
  return v;
}

/* Hash NAME with the previous hash value V and return it.  */

static hashval_t
iterative_hash_name (tree name, hashval_t v)
{
  if (!name)
    return v;
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  if (!name)
    return v;
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);
  return iterative_hash_object (IDENTIFIER_HASH_VALUE (name), v);
}

/* Returning a hash value for gimple type TYPE combined with VAL.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.

   To hash a type we end up hashing in types that are reachable.
   Through pointers we can end up with cycles which messes up the
   required property that we need to compute the same hash value
   for structurally equivalent types.  To avoid this we have to
   hash all types in a cycle (the SCC) in a commutative way.  The
   easiest way is to not mix in the hashes of the SCC members at
   all.  To make this work we have to delay setting the hash
   values of the SCC until it is complete.  */

static hashval_t
iterative_hash_gimple_type (tree type, hashval_t val,
			    VEC(tree, heap) **sccstack,
			    struct pointer_map_t *sccstate,
			    struct obstack *sccstate_obstack,
			    enum gtc_mode mode)
{
  hashval_t v;
  void **slot;
  struct sccs *state;

  /* Not visited during this DFS walk.  */
  gcc_checking_assert (!pointer_map_contains (sccstate, type));
  state = XOBNEW (sccstate_obstack, struct sccs);
  *pointer_map_insert (sccstate, type) = state;

  VEC_safe_push (tree, heap, *sccstack, type);
  state->dfsnum = next_dfs_num++;
  state->low = state->dfsnum;
  state->on_sccstack = true;

  /* Combine a few common features of types so that types are grouped into
     smaller sets; when searching for existing matching types to merge,
     only existing types having the same features as the new type will be
     checked.  */
  v = iterative_hash_hashval_t (TREE_CODE (type), 0);
  v = iterative_hash_hashval_t (TYPE_QUALS (type), v);
  v = iterative_hash_hashval_t (TREE_ADDRESSABLE (type), v);

  /* Do not hash the types size as this will cause differences in
     hash values for the complete vs. the incomplete type variant.  */

  /* Incorporate common features of numerical types.  */
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || FIXED_POINT_TYPE_P (type))
    {
      v = iterative_hash_hashval_t (TYPE_PRECISION (type), v);
      v = iterative_hash_hashval_t (TYPE_MODE (type), v);
      v = iterative_hash_hashval_t (TYPE_UNSIGNED (type), v);
    }

  /* For pointer and reference types, fold in information about the type
     pointed to but do not recurse into possibly incomplete types to
     avoid hash differences for complete vs. incomplete types.  */
  if (POINTER_TYPE_P (type))
    {
      if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (type)))
	{
	  v = iterative_hash_hashval_t (TREE_CODE (TREE_TYPE (type)), v);
	  v = iterative_hash_name
		(TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (type))), v);
	}
      else
	v = visit (TREE_TYPE (type), state, v,
		   sccstack, sccstate, sccstate_obstack, mode);
    }

  /* For integer types hash the types min/max values and the string flag.  */
  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      /* OMP lowering can introduce error_mark_node in place of
	 random local decls in types.  */
      if (TYPE_MIN_VALUE (type) != error_mark_node)
	v = iterative_hash_expr (TYPE_MIN_VALUE (type), v);
      if (TYPE_MAX_VALUE (type) != error_mark_node)
	v = iterative_hash_expr (TYPE_MAX_VALUE (type), v);
      v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);
    }

  /* For array types hash their domain and the string flag.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type))
    {
      v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);
      v = visit (TYPE_DOMAIN (type), state, v,
		 sccstack, sccstate, sccstate_obstack, mode);
    }

  /* Recurse for aggregates with a single element type.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      || TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    v = visit (TREE_TYPE (type), state, v,
	       sccstack, sccstate, sccstate_obstack, mode);

  /* Incorporate function return and argument types.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      unsigned na;
      tree p;

      /* For method types also incorporate their parent class.  */
      if (TREE_CODE (type) == METHOD_TYPE)
	v = visit (TYPE_METHOD_BASETYPE (type), state, v,
		   sccstack, sccstate, sccstate_obstack, mode);

      /* For result types allow mismatch in completeness.  */
      if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (type)))
	{
	  v = iterative_hash_hashval_t (TREE_CODE (TREE_TYPE (type)), v);
	  v = iterative_hash_name
		(TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (type))), v);
	}
      else
	v = visit (TREE_TYPE (type), state, v,
		   sccstack, sccstate, sccstate_obstack, mode);

      for (p = TYPE_ARG_TYPES (type), na = 0; p; p = TREE_CHAIN (p))
	{
	  /* For argument types allow mismatch in completeness.  */
	  if (RECORD_OR_UNION_TYPE_P (TREE_VALUE (p)))
	    {
	      v = iterative_hash_hashval_t (TREE_CODE (TREE_VALUE (p)), v);
	      v = iterative_hash_name
		    (TYPE_NAME (TYPE_MAIN_VARIANT (TREE_VALUE (p))), v);
	    }
	  else
	    v = visit (TREE_VALUE (p), state, v,
		       sccstack, sccstate, sccstate_obstack, mode);
	  na++;
	}

      v = iterative_hash_hashval_t (na, v);
    }

  if (TREE_CODE (type) == RECORD_TYPE
      || TREE_CODE (type) == UNION_TYPE
      || TREE_CODE (type) == QUAL_UNION_TYPE)
    {
      unsigned nf;
      tree f;

      if (mode == GTC_MERGE)
	v = iterative_hash_name (TYPE_NAME (TYPE_MAIN_VARIANT (type)), v);

      for (f = TYPE_FIELDS (type), nf = 0; f; f = TREE_CHAIN (f))
	{
	  if (mode == GTC_MERGE)
	    v = iterative_hash_name (DECL_NAME (f), v);
	  v = visit (TREE_TYPE (f), state, v,
		     sccstack, sccstate, sccstate_obstack, mode);
	  nf++;
	}

      v = iterative_hash_hashval_t (nf, v);
    }

  /* Record hash for us.  */
  state->u.hash = v;

  /* See if we found an SCC.  */
  if (state->low == state->dfsnum)
    {
      tree x;

      /* Pop off the SCC and set its hash values.  */
      do
	{
	  struct sccs *cstate;
	  struct tree_int_map *m = ggc_alloc_cleared_tree_int_map ();
	  x = VEC_pop (tree, *sccstack);
	  cstate = (struct sccs *)*pointer_map_contains (sccstate, x);
	  cstate->on_sccstack = false;
	  m->base.from = x;
	  m->to = cstate->u.hash;
	  slot = htab_find_slot (mode == GTC_MERGE
				 ? type_hash_cache : canonical_type_hash_cache,
				 m, INSERT);
	  gcc_assert (!*slot);
	  *slot = (void *) m;
	}
      while (x != type);
    }

  return iterative_hash_hashval_t (v, val);
}


/* Returns a hash value for P (assumed to be a type).  The hash value
   is computed using some distinguishing features of the type.  Note
   that we cannot use pointer hashing here as we may be dealing with
   two distinct instances of the same type.

   This function should produce the same hash value for two compatible
   types according to gimple_types_compatible_p.  */

static hashval_t
gimple_type_hash_1 (const void *p, enum gtc_mode mode)
{
  const_tree t = (const_tree) p;
  VEC(tree, heap) *sccstack = NULL;
  struct pointer_map_t *sccstate;
  struct obstack sccstate_obstack;
  hashval_t val;
  void **slot;
  struct tree_int_map m;

  if (mode == GTC_MERGE
      && type_hash_cache == NULL)
    type_hash_cache = htab_create_ggc (512, tree_int_map_hash,
				       tree_int_map_eq, NULL);
  else if (mode == GTC_DIAG
	   && canonical_type_hash_cache == NULL)
    canonical_type_hash_cache = htab_create_ggc (512, tree_int_map_hash,
						 tree_int_map_eq, NULL);

  m.base.from = CONST_CAST_TREE (t);
  if ((slot = htab_find_slot (mode == GTC_MERGE
			      ? type_hash_cache : canonical_type_hash_cache,
			      &m, NO_INSERT))
      && *slot)
    return iterative_hash_hashval_t (((struct tree_int_map *) *slot)->to, 0);

  /* Perform a DFS walk and pre-hash all reachable types.  */
  next_dfs_num = 1;
  sccstate = pointer_map_create ();
  gcc_obstack_init (&sccstate_obstack);
  val = iterative_hash_gimple_type (CONST_CAST_TREE (t), 0,
				    &sccstack, sccstate, &sccstate_obstack,
				    mode);
  VEC_free (tree, heap, sccstack);
  pointer_map_destroy (sccstate);
  obstack_free (&sccstate_obstack, NULL);

  return val;
}

static hashval_t
gimple_type_hash (const void *p)
{
  return gimple_type_hash_1 (p, GTC_MERGE);
}

static hashval_t
gimple_canonical_type_hash (const void *p)
{
  return gimple_type_hash_1 (p, GTC_DIAG);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
gimple_type_eq (const void *p1, const void *p2)
{
  const_tree t1 = (const_tree) p1;
  const_tree t2 = (const_tree) p2;
  return gimple_types_compatible_p (CONST_CAST_TREE (t1),
				    CONST_CAST_TREE (t2), GTC_MERGE);
}


/* Register type T in the global type table gimple_types.
   If another type T', compatible with T, already existed in
   gimple_types then return T', otherwise return T.  This is used by
   LTO to merge identical types read from different TUs.  */

tree
gimple_register_type (tree t)
{
  void **slot;
  gimple_type_leader_entry *leader;
  tree mv_leader = NULL_TREE;

  gcc_assert (TYPE_P (t));

  if (!gimple_type_leader)
    gimple_type_leader = ggc_alloc_cleared_vec_gimple_type_leader_entry_s
				(GIMPLE_TYPE_LEADER_SIZE);
  /* If we registered this type before return the cached result.  */
  leader = &gimple_type_leader[TYPE_UID (t) % GIMPLE_TYPE_LEADER_SIZE];
  if (leader->type == t)
    return leader->leader;

  /* Always register the main variant first.  This is important so we
     pick up the non-typedef variants as canonical, otherwise we'll end
     up taking typedef ids for structure tags during comparison.  */
  if (TYPE_MAIN_VARIANT (t) != t)
    mv_leader = gimple_register_type (TYPE_MAIN_VARIANT (t));

  if (gimple_types == NULL)
    gimple_types = htab_create_ggc (16381, gimple_type_hash, gimple_type_eq, 0);

  slot = htab_find_slot (gimple_types, t, INSERT);
  if (*slot
      && *(tree *)slot != t)
    {
      tree new_type = (tree) *((tree *) slot);

      /* Do not merge types with different addressability.  */
      gcc_assert (TREE_ADDRESSABLE (t) == TREE_ADDRESSABLE (new_type));

      /* If t is not its main variant then make t unreachable from its
	 main variant list.  Otherwise we'd queue up a lot of duplicates
	 there.  */
      if (t != TYPE_MAIN_VARIANT (t))
	{
	  tree tem = TYPE_MAIN_VARIANT (t);
	  while (tem && TYPE_NEXT_VARIANT (tem) != t)
	    tem = TYPE_NEXT_VARIANT (tem);
	  if (tem)
	    TYPE_NEXT_VARIANT (tem) = TYPE_NEXT_VARIANT (t);
	  TYPE_NEXT_VARIANT (t) = NULL_TREE;
	}

      /* If we are a pointer then remove us from the pointer-to or
	 reference-to chain.  Otherwise we'd queue up a lot of duplicates
	 there.  */
      if (TREE_CODE (t) == POINTER_TYPE)
	{
	  if (TYPE_POINTER_TO (TREE_TYPE (t)) == t)
	    TYPE_POINTER_TO (TREE_TYPE (t)) = TYPE_NEXT_PTR_TO (t);
	  else
	    {
	      tree tem = TYPE_POINTER_TO (TREE_TYPE (t));
	      while (tem && TYPE_NEXT_PTR_TO (tem) != t)
		tem = TYPE_NEXT_PTR_TO (tem);
	      if (tem)
		TYPE_NEXT_PTR_TO (tem) = TYPE_NEXT_PTR_TO (t);
	    }
	  TYPE_NEXT_PTR_TO (t) = NULL_TREE;
	}
      else if (TREE_CODE (t) == REFERENCE_TYPE)
	{
	  if (TYPE_REFERENCE_TO (TREE_TYPE (t)) == t)
	    TYPE_REFERENCE_TO (TREE_TYPE (t)) = TYPE_NEXT_REF_TO (t);
	  else
	    {
	      tree tem = TYPE_REFERENCE_TO (TREE_TYPE (t));
	      while (tem && TYPE_NEXT_REF_TO (tem) != t)
		tem = TYPE_NEXT_REF_TO (tem);
	      if (tem)
		TYPE_NEXT_REF_TO (tem) = TYPE_NEXT_REF_TO (t);
	    }
	  TYPE_NEXT_REF_TO (t) = NULL_TREE;
	}

      leader->type = t;
      leader->leader = new_type;
      t = new_type;
    }
  else
    {
      leader->type = t;
      leader->leader = t;
      /* We're the type leader.  Make our TYPE_MAIN_VARIANT valid.  */
      if (TYPE_MAIN_VARIANT (t) != t
	  && TYPE_MAIN_VARIANT (t) != mv_leader)
	{
	  /* Remove us from our main variant list as we are not the variant
	     leader and the variant leader will change.  */
	  tree tem = TYPE_MAIN_VARIANT (t);
	  while (tem && TYPE_NEXT_VARIANT (tem) != t)
	    tem = TYPE_NEXT_VARIANT (tem);
	  if (tem)
	    TYPE_NEXT_VARIANT (tem) = TYPE_NEXT_VARIANT (t);
	  TYPE_NEXT_VARIANT (t) = NULL_TREE;
	  /* Adjust our main variant.  Linking us into its variant list
	     will happen at fixup time.  */
	  TYPE_MAIN_VARIANT (t) = mv_leader;
	}
      *slot = (void *) t;
    }

  return t;
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
gimple_canonical_type_eq (const void *p1, const void *p2)
{
  const_tree t1 = (const_tree) p1;
  const_tree t2 = (const_tree) p2;
  return gimple_types_compatible_p (CONST_CAST_TREE (t1),
				    CONST_CAST_TREE (t2), GTC_DIAG);
}

/* Register type T in the global type table gimple_types.
   If another type T', compatible with T, already existed in
   gimple_types then return T', otherwise return T.  This is used by
   LTO to merge identical types read from different TUs.  */

tree
gimple_register_canonical_type (tree t)
{
  void **slot;
  tree orig_t = t;

  gcc_assert (TYPE_P (t));

  if (TYPE_CANONICAL (t))
    return TYPE_CANONICAL (t);

  /* Always register the type itself first so that if it turns out
     to be the canonical type it will be the one we merge to as well.  */
  t = gimple_register_type (t);

  /* Always register the main variant first.  This is important so we
     pick up the non-typedef variants as canonical, otherwise we'll end
     up taking typedef ids for structure tags during comparison.  */
  if (TYPE_MAIN_VARIANT (t) != t)
    gimple_register_canonical_type (TYPE_MAIN_VARIANT (t));

  if (gimple_canonical_types == NULL)
    gimple_canonical_types = htab_create_ggc (16381, gimple_canonical_type_hash,
					      gimple_canonical_type_eq, 0);

  slot = htab_find_slot (gimple_canonical_types, t, INSERT);
  if (*slot
      && *(tree *)slot != t)
    {
      tree new_type = (tree) *((tree *) slot);

      TYPE_CANONICAL (t) = new_type;
      t = new_type;
    }
  else
    {
      TYPE_CANONICAL (t) = t;
      *slot = (void *) t;
    }

  /* Also cache the canonical type in the non-leaders.  */
  TYPE_CANONICAL (orig_t) = t;

  return t;
}


/* Show statistics on references to the global type table gimple_types.  */

void
print_gimple_types_stats (void)
{
  if (gimple_types)
    fprintf (stderr, "GIMPLE type table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n",
	     (long) htab_size (gimple_types),
	     (long) htab_elements (gimple_types),
	     (long) gimple_types->searches,
	     (long) gimple_types->collisions,
	     htab_collisions (gimple_types));
  else
    fprintf (stderr, "GIMPLE type table is empty\n");
  if (type_hash_cache)
    fprintf (stderr, "GIMPLE type hash table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n",
	     (long) htab_size (type_hash_cache),
	     (long) htab_elements (type_hash_cache),
	     (long) type_hash_cache->searches,
	     (long) type_hash_cache->collisions,
	     htab_collisions (type_hash_cache));
  else
    fprintf (stderr, "GIMPLE type hash table is empty\n");
  if (gimple_canonical_types)
    fprintf (stderr, "GIMPLE canonical type table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n",
	     (long) htab_size (gimple_canonical_types),
	     (long) htab_elements (gimple_canonical_types),
	     (long) gimple_canonical_types->searches,
	     (long) gimple_canonical_types->collisions,
	     htab_collisions (gimple_canonical_types));
  else
    fprintf (stderr, "GIMPLE canonical type table is empty\n");
  if (canonical_type_hash_cache)
    fprintf (stderr, "GIMPLE canonical type hash table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n",
	     (long) htab_size (canonical_type_hash_cache),
	     (long) htab_elements (canonical_type_hash_cache),
	     (long) canonical_type_hash_cache->searches,
	     (long) canonical_type_hash_cache->collisions,
	     htab_collisions (canonical_type_hash_cache));
  else
    fprintf (stderr, "GIMPLE canonical type hash table is empty\n");
  if (gtc_visited)
    fprintf (stderr, "GIMPLE type comparison table: size %ld, %ld "
	     "elements, %ld searches, %ld collisions (ratio: %f)\n",
	     (long) htab_size (gtc_visited),
	     (long) htab_elements (gtc_visited),
	     (long) gtc_visited->searches,
	     (long) gtc_visited->collisions,
	     htab_collisions (gtc_visited));
  else
    fprintf (stderr, "GIMPLE type comparison table is empty\n");
}

/* Free the gimple type hashtables used for LTO type merging.  */

void
free_gimple_type_tables (void)
{
  /* Last chance to print stats for the tables.  */
  if (flag_lto_report)
    print_gimple_types_stats ();

  if (gimple_types)
    {
      htab_delete (gimple_types);
      gimple_types = NULL;
    }
  if (gimple_canonical_types)
    {
      htab_delete (gimple_canonical_types);
      gimple_canonical_types = NULL;
    }
  if (type_hash_cache)
    {
      htab_delete (type_hash_cache);
      type_hash_cache = NULL;
    }
  if (canonical_type_hash_cache)
    {
      htab_delete (canonical_type_hash_cache);
      canonical_type_hash_cache = NULL;
    }
  if (gtc_visited)
    {
      htab_delete (gtc_visited);
      obstack_free (&gtc_ob, NULL);
      gtc_visited = NULL;
    }
  gimple_type_leader = NULL;
}


/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

static tree
gimple_signed_or_unsigned_type (bool unsignedp, tree type)
{
  tree type1;

  type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node
      || type1 == char_type_node
      || type1 == unsigned_char_type_node)
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (type1 == integer_type_node || type1 == unsigned_type_node)
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (type1 == short_integer_type_node || type1 == short_unsigned_type_node)
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (type1 == long_integer_type_node || type1 == long_unsigned_type_node)
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (type1 == long_long_integer_type_node
      || type1 == long_long_unsigned_type_node)
    return unsignedp
           ? long_long_unsigned_type_node
	   : long_long_integer_type_node;
  if (int128_integer_type_node && (type1 == int128_integer_type_node || type1 == int128_unsigned_type_node))
    return unsignedp
           ? int128_unsigned_type_node
	   : int128_integer_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node || type1 == unsigned_intTI_type_node)
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (type1 == intDI_type_node || type1 == unsigned_intDI_type_node)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (type1 == intSI_type_node || type1 == unsigned_intSI_type_node)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (type1 == intHI_type_node || type1 == unsigned_intHI_type_node)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (type1 == intQI_type_node || type1 == unsigned_intQI_type_node)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

#define GIMPLE_FIXED_TYPES(NAME)	    \
  if (type1 == short_ ## NAME ## _type_node \
      || type1 == unsigned_short_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_short_ ## NAME ## _type_node \
		     : short_ ## NAME ## _type_node; \
  if (type1 == NAME ## _type_node \
      || type1 == unsigned_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_ ## NAME ## _type_node \
		     : NAME ## _type_node; \
  if (type1 == long_ ## NAME ## _type_node \
      || type1 == unsigned_long_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_long_ ## NAME ## _type_node \
		     : long_ ## NAME ## _type_node; \
  if (type1 == long_long_ ## NAME ## _type_node \
      || type1 == unsigned_long_long_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_long_long_ ## NAME ## _type_node \
		     : long_long_ ## NAME ## _type_node;

#define GIMPLE_FIXED_MODE_TYPES(NAME) \
  if (type1 == NAME ## _type_node \
      || type1 == u ## NAME ## _type_node) \
    return unsignedp ? u ## NAME ## _type_node \
		     : NAME ## _type_node;

#define GIMPLE_FIXED_TYPES_SAT(NAME) \
  if (type1 == sat_ ## short_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_short_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_short_ ## NAME ## _type_node \
		     : sat_ ## short_ ## NAME ## _type_node; \
  if (type1 == sat_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_ ## NAME ## _type_node \
		     : sat_ ## NAME ## _type_node; \
  if (type1 == sat_ ## long_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_long_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_long_ ## NAME ## _type_node \
		     : sat_ ## long_ ## NAME ## _type_node; \
  if (type1 == sat_ ## long_long_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_long_long_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_long_long_ ## NAME ## _type_node \
		     : sat_ ## long_long_ ## NAME ## _type_node;

#define GIMPLE_FIXED_MODE_TYPES_SAT(NAME)	\
  if (type1 == sat_ ## NAME ## _type_node \
      || type1 == sat_ ## u ## NAME ## _type_node) \
    return unsignedp ? sat_ ## u ## NAME ## _type_node \
		     : sat_ ## NAME ## _type_node;

  GIMPLE_FIXED_TYPES (fract);
  GIMPLE_FIXED_TYPES_SAT (fract);
  GIMPLE_FIXED_TYPES (accum);
  GIMPLE_FIXED_TYPES_SAT (accum);

  GIMPLE_FIXED_MODE_TYPES (qq);
  GIMPLE_FIXED_MODE_TYPES (hq);
  GIMPLE_FIXED_MODE_TYPES (sq);
  GIMPLE_FIXED_MODE_TYPES (dq);
  GIMPLE_FIXED_MODE_TYPES (tq);
  GIMPLE_FIXED_MODE_TYPES_SAT (qq);
  GIMPLE_FIXED_MODE_TYPES_SAT (hq);
  GIMPLE_FIXED_MODE_TYPES_SAT (sq);
  GIMPLE_FIXED_MODE_TYPES_SAT (dq);
  GIMPLE_FIXED_MODE_TYPES_SAT (tq);
  GIMPLE_FIXED_MODE_TYPES (ha);
  GIMPLE_FIXED_MODE_TYPES (sa);
  GIMPLE_FIXED_MODE_TYPES (da);
  GIMPLE_FIXED_MODE_TYPES (ta);
  GIMPLE_FIXED_MODE_TYPES_SAT (ha);
  GIMPLE_FIXED_MODE_TYPES_SAT (sa);
  GIMPLE_FIXED_MODE_TYPES_SAT (da);
  GIMPLE_FIXED_MODE_TYPES_SAT (ta);

  /* For ENUMERAL_TYPEs in C++, must check the mode of the types, not
     the precision; they have precision set to match their range, but
     may use a wider mode to match an ABI.  If we change modes, we may
     wind up with bad conversions.  For INTEGER_TYPEs in C, must check
     the precision as well, so as to yield correct results for
     bit-field types.  C++ does not have these separate bit-field
     types, and producing a signed or unsigned variant of an
     ENUMERAL_TYPE may cause other problems as well.  */
  if (!INTEGRAL_TYPE_P (type)
      || TYPE_UNSIGNED (type) == unsignedp)
    return type;

#define TYPE_OK(node)							    \
  (TYPE_MODE (type) == TYPE_MODE (node)					    \
   && TYPE_PRECISION (type) == TYPE_PRECISION (node))
  if (TYPE_OK (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_OK (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_OK (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_OK (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_OK (long_long_integer_type_node))
    return (unsignedp
	    ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  if (int128_integer_type_node && TYPE_OK (int128_integer_type_node))
    return (unsignedp
	    ? int128_unsigned_type_node
	    : int128_integer_type_node);

#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_OK (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_OK (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_OK (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_OK (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_OK (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

#undef GIMPLE_FIXED_TYPES
#undef GIMPLE_FIXED_MODE_TYPES
#undef GIMPLE_FIXED_TYPES_SAT
#undef GIMPLE_FIXED_MODE_TYPES_SAT
#undef TYPE_OK

  return build_nonstandard_integer_type (TYPE_PRECISION (type), unsignedp);
}


/* Return an unsigned type the same as TYPE in other respects.  */

tree
gimple_unsigned_type (tree type)
{
  return gimple_signed_or_unsigned_type (true, type);
}


/* Return a signed type the same as TYPE in other respects.  */

tree
gimple_signed_type (tree type)
{
  return gimple_signed_or_unsigned_type (false, type);
}


/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

alias_set_type
gimple_get_alias_set (tree t)
{
  tree u;

  /* Permit type-punning when accessing a union, provided the access
     is directly through the union.  For example, this code does not
     permit taking the address of a union member and then storing
     through it.  Even the type-punning allowed here is a GCC
     extension, albeit a common and useful one; the C standard says
     that such accesses have implementation-defined behavior.  */
  for (u = t;
       TREE_CODE (u) == COMPONENT_REF || TREE_CODE (u) == ARRAY_REF;
       u = TREE_OPERAND (u, 0))
    if (TREE_CODE (u) == COMPONENT_REF
	&& TREE_CODE (TREE_TYPE (TREE_OPERAND (u, 0))) == UNION_TYPE)
      return 0;

  /* That's all the expressions we handle specially.  */
  if (!TYPE_P (t))
    return -1;

  /* For convenience, follow the C standard when dealing with
     character types.  Any object may be accessed via an lvalue that
     has character type.  */
  if (t == char_type_node
      || t == signed_char_type_node
      || t == unsigned_char_type_node)
    return 0;

  /* Allow aliasing between signed and unsigned variants of the same
     type.  We treat the signed variant as canonical.  */
  if (TREE_CODE (t) == INTEGER_TYPE && TYPE_UNSIGNED (t))
    {
      tree t1 = gimple_signed_type (t);

      /* t1 == t can happen for boolean nodes which are always unsigned.  */
      if (t1 != t)
	return get_alias_set (t1);
    }

  return -1;
}


/* Data structure used to count the number of dereferences to PTR
   inside an expression.  */
struct count_ptr_d
{
  tree ptr;
  unsigned num_stores;
  unsigned num_loads;
};

/* Helper for count_uses_and_derefs.  Called by walk_tree to look for
   (ALIGN/MISALIGNED_)INDIRECT_REF nodes for the pointer passed in DATA.  */

static tree
count_ptr_derefs (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi_p = (struct walk_stmt_info *) data;
  struct count_ptr_d *count_p = (struct count_ptr_d *) wi_p->info;

  /* Do not walk inside ADDR_EXPR nodes.  In the expression &ptr->fld,
     pointer 'ptr' is *not* dereferenced, it is simply used to compute
     the address of 'fld' as 'ptr + offsetof(fld)'.  */
  if (TREE_CODE (*tp) == ADDR_EXPR)
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (TREE_CODE (*tp) == MEM_REF && TREE_OPERAND (*tp, 0) == count_p->ptr)
    {
      if (wi_p->is_lhs)
	count_p->num_stores++;
      else
	count_p->num_loads++;
    }

  return NULL_TREE;
}

/* Count the number of direct and indirect uses for pointer PTR in
   statement STMT.  The number of direct uses is stored in
   *NUM_USES_P.  Indirect references are counted separately depending
   on whether they are store or load operations.  The counts are
   stored in *NUM_STORES_P and *NUM_LOADS_P.  */

void
count_uses_and_derefs (tree ptr, gimple stmt, unsigned *num_uses_p,
		       unsigned *num_loads_p, unsigned *num_stores_p)
{
  ssa_op_iter i;
  tree use;

  *num_uses_p = 0;
  *num_loads_p = 0;
  *num_stores_p = 0;

  /* Find out the total number of uses of PTR in STMT.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, i, SSA_OP_USE)
    if (use == ptr)
      (*num_uses_p)++;

  /* Now count the number of indirect references to PTR.  This is
     truly awful, but we don't have much choice.  There are no parent
     pointers inside INDIRECT_REFs, so an expression like
     '*x_1 = foo (x_1, *x_1)' needs to be traversed piece by piece to
     find all the indirect and direct uses of x_1 inside.  The only
     shortcut we can take is the fact that GIMPLE only allows
     INDIRECT_REFs inside the expressions below.  */
  if (is_gimple_assign (stmt)
      || gimple_code (stmt) == GIMPLE_RETURN
      || gimple_code (stmt) == GIMPLE_ASM
      || is_gimple_call (stmt))
    {
      struct walk_stmt_info wi;
      struct count_ptr_d count;

      count.ptr = ptr;
      count.num_stores = 0;
      count.num_loads = 0;

      memset (&wi, 0, sizeof (wi));
      wi.info = &count;
      walk_gimple_op (stmt, count_ptr_derefs, &wi);

      *num_stores_p = count.num_stores;
      *num_loads_p = count.num_loads;
    }

  gcc_assert (*num_uses_p >= *num_loads_p + *num_stores_p);
}

/* From a tree operand OP return the base of a load or store operation
   or NULL_TREE if OP is not a load or a store.  */

static tree
get_base_loadstore (tree op)
{
  while (handled_component_p (op))
    op = TREE_OPERAND (op, 0);
  if (DECL_P (op)
      || INDIRECT_REF_P (op)
      || TREE_CODE (op) == MEM_REF
      || TREE_CODE (op) == TARGET_MEM_REF)
    return op;
  return NULL_TREE;
}

/* For the statement STMT call the callbacks VISIT_LOAD, VISIT_STORE and
   VISIT_ADDR if non-NULL on loads, store and address-taken operands
   passing the STMT, the base of the operand and DATA to it.  The base
   will be either a decl, an indirect reference (including TARGET_MEM_REF)
   or the argument of an address expression.
   Returns the results of these callbacks or'ed.  */

bool
walk_stmt_load_store_addr_ops (gimple stmt, void *data,
			       bool (*visit_load)(gimple, tree, void *),
			       bool (*visit_store)(gimple, tree, void *),
			       bool (*visit_addr)(gimple, tree, void *))
{
  bool ret = false;
  unsigned i;
  if (gimple_assign_single_p (stmt))
    {
      tree lhs, rhs;
      if (visit_store)
	{
	  lhs = get_base_loadstore (gimple_assign_lhs (stmt));
	  if (lhs)
	    ret |= visit_store (stmt, lhs, data);
	}
      rhs = gimple_assign_rhs1 (stmt);
      while (handled_component_p (rhs))
	rhs = TREE_OPERAND (rhs, 0);
      if (visit_addr)
	{
	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (rhs, 0), data);
	  else if (TREE_CODE (rhs) == TARGET_MEM_REF
		   && TREE_CODE (TMR_BASE (rhs)) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (TMR_BASE (rhs), 0), data);
	  else if (TREE_CODE (rhs) == OBJ_TYPE_REF
		   && TREE_CODE (OBJ_TYPE_REF_OBJECT (rhs)) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (OBJ_TYPE_REF_OBJECT (rhs),
						   0), data);
          lhs = gimple_assign_lhs (stmt);
	  if (TREE_CODE (lhs) == TARGET_MEM_REF
              && TREE_CODE (TMR_BASE (lhs)) == ADDR_EXPR)
            ret |= visit_addr (stmt, TREE_OPERAND (TMR_BASE (lhs), 0), data);
	}
      if (visit_load)
	{
	  rhs = get_base_loadstore (rhs);
	  if (rhs)
	    ret |= visit_load (stmt, rhs, data);
	}
    }
  else if (visit_addr
	   && (is_gimple_assign (stmt)
	       || gimple_code (stmt) == GIMPLE_COND))
    {
      for (i = 0; i < gimple_num_ops (stmt); ++i)
	if (gimple_op (stmt, i)
	    && TREE_CODE (gimple_op (stmt, i)) == ADDR_EXPR)
	  ret |= visit_addr (stmt, TREE_OPERAND (gimple_op (stmt, i), 0), data);
    }
  else if (is_gimple_call (stmt))
    {
      if (visit_store)
	{
	  tree lhs = gimple_call_lhs (stmt);
	  if (lhs)
	    {
	      lhs = get_base_loadstore (lhs);
	      if (lhs)
		ret |= visit_store (stmt, lhs, data);
	    }
	}
      if (visit_load || visit_addr)
	for (i = 0; i < gimple_call_num_args (stmt); ++i)
	  {
	    tree rhs = gimple_call_arg (stmt, i);
	    if (visit_addr
		&& TREE_CODE (rhs) == ADDR_EXPR)
	      ret |= visit_addr (stmt, TREE_OPERAND (rhs, 0), data);
	    else if (visit_load)
	      {
		rhs = get_base_loadstore (rhs);
		if (rhs)
		  ret |= visit_load (stmt, rhs, data);
	      }
	  }
      if (visit_addr
	  && gimple_call_chain (stmt)
	  && TREE_CODE (gimple_call_chain (stmt)) == ADDR_EXPR)
	ret |= visit_addr (stmt, TREE_OPERAND (gimple_call_chain (stmt), 0),
			   data);
      if (visit_addr
	  && gimple_call_return_slot_opt_p (stmt)
	  && gimple_call_lhs (stmt) != NULL_TREE
	  && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (stmt))))
	ret |= visit_addr (stmt, gimple_call_lhs (stmt), data);
    }
  else if (gimple_code (stmt) == GIMPLE_ASM)
    {
      unsigned noutputs;
      const char *constraint;
      const char **oconstraints;
      bool allows_mem, allows_reg, is_inout;
      noutputs = gimple_asm_noutputs (stmt);
      oconstraints = XALLOCAVEC (const char *, noutputs);
      if (visit_store || visit_addr)
	for (i = 0; i < gimple_asm_noutputs (stmt); ++i)
	  {
	    tree link = gimple_asm_output_op (stmt, i);
	    tree op = get_base_loadstore (TREE_VALUE (link));
	    if (op && visit_store)
	      ret |= visit_store (stmt, op, data);
	    if (visit_addr)
	      {
		constraint = TREE_STRING_POINTER
		    (TREE_VALUE (TREE_PURPOSE (link)));
		oconstraints[i] = constraint;
		parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
					 &allows_reg, &is_inout);
		if (op && !allows_reg && allows_mem)
		  ret |= visit_addr (stmt, op, data);
	      }
	  }
      if (visit_load || visit_addr)
	for (i = 0; i < gimple_asm_ninputs (stmt); ++i)
	  {
	    tree link = gimple_asm_input_op (stmt, i);
	    tree op = TREE_VALUE (link);
	    if (visit_addr
		&& TREE_CODE (op) == ADDR_EXPR)
	      ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	    else if (visit_load || visit_addr)
	      {
		op = get_base_loadstore (op);
		if (op)
		  {
		    if (visit_load)
		      ret |= visit_load (stmt, op, data);
		    if (visit_addr)
		      {
			constraint = TREE_STRING_POINTER
			    (TREE_VALUE (TREE_PURPOSE (link)));
			parse_input_constraint (&constraint, 0, 0, noutputs,
						0, oconstraints,
						&allows_mem, &allows_reg);
			if (!allows_reg && allows_mem)
			  ret |= visit_addr (stmt, op, data);
		      }
		  }
	      }
	  }
    }
  else if (gimple_code (stmt) == GIMPLE_RETURN)
    {
      tree op = gimple_return_retval (stmt);
      if (op)
	{
	  if (visit_addr
	      && TREE_CODE (op) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	  else if (visit_load)
	    {
	      op = get_base_loadstore (op);
	      if (op)
		ret |= visit_load (stmt, op, data);
	    }
	}
    }
  else if (visit_addr
	   && gimple_code (stmt) == GIMPLE_PHI)
    {
      for (i = 0; i < gimple_phi_num_args (stmt); ++i)
	{
	  tree op = PHI_ARG_DEF (stmt, i);
	  if (TREE_CODE (op) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	}
    }

  return ret;
}

/* Like walk_stmt_load_store_addr_ops but with NULL visit_addr.  IPA-CP
   should make a faster clone for this case.  */

bool
walk_stmt_load_store_ops (gimple stmt, void *data,
			  bool (*visit_load)(gimple, tree, void *),
			  bool (*visit_store)(gimple, tree, void *))
{
  return walk_stmt_load_store_addr_ops (stmt, data,
					visit_load, visit_store, NULL);
}

/* Helper for gimple_ior_addresses_taken_1.  */

static bool
gimple_ior_addresses_taken_1 (gimple stmt ATTRIBUTE_UNUSED,
			      tree addr, void *data)
{
  bitmap addresses_taken = (bitmap)data;
  addr = get_base_address (addr);
  if (addr
      && DECL_P (addr))
    {
      bitmap_set_bit (addresses_taken, DECL_UID (addr));
      return true;
    }
  return false;
}

/* Set the bit for the uid of all decls that have their address taken
   in STMT in the ADDRESSES_TAKEN bitmap.  Returns true if there
   were any in this stmt.  */

bool
gimple_ior_addresses_taken (bitmap addresses_taken, gimple stmt)
{
  return walk_stmt_load_store_addr_ops (stmt, addresses_taken, NULL, NULL,
					gimple_ior_addresses_taken_1);
}


/* Return a printable name for symbol DECL.  */

const char *
gimple_decl_printable_name (tree decl, int verbosity)
{
  if (!DECL_NAME (decl))
    return NULL;

  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      const char *str, *mangled_str;
      int dmgl_opts = DMGL_NO_OPTS;

      if (verbosity >= 2)
	{
	  dmgl_opts = DMGL_VERBOSE
		      | DMGL_ANSI
		      | DMGL_GNU_V3
		      | DMGL_RET_POSTFIX;
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    dmgl_opts |= DMGL_PARAMS;
	}

      mangled_str = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      str = cplus_demangle_v3 (mangled_str, dmgl_opts);
      return (str) ? str : mangled_str;
    }

  return IDENTIFIER_POINTER (DECL_NAME (decl));
}

/* Return true when STMT is builtins call to CODE.  */

bool
gimple_call_builtin_p (gimple stmt, enum built_in_function code)
{
  tree fndecl;
  return (is_gimple_call (stmt)
	  && (fndecl = gimple_call_fndecl (stmt)) != NULL
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (fndecl) == code);
}

#include "gt-gimple.h"
