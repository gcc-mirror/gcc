/* Gimple IR support functions.

   Copyright (C) 2007-2014 Free Software Foundation, Inc.
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
#include "calls.h"
#include "stmt.h"
#include "stor-layout.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple.h"
#include "gimplify.h"
#include "diagnostic.h"
#include "value-prof.h"
#include "flags.h"
#include "alias.h"
#include "demangle.h"
#include "langhooks.h"
#include "bitmap.h"


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

#define DEFGSSTRUCT(SYM, STRUCT, HAS_TREE_OP) sizeof (struct STRUCT),
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

/* Gimple stats.  */

int gimple_alloc_counts[(int) gimple_alloc_kind_all];
int gimple_alloc_sizes[(int) gimple_alloc_kind_all];

/* Keep in sync with gimple.h:enum gimple_alloc_kind.  */
static const char * const gimple_alloc_kind_names[] = {
    "assignments",
    "phi nodes",
    "conditionals",
    "everything else"
};

/* Gimple tuple constructors.
   Note: Any constructor taking a ``gimple_seq'' as a parameter, can
   be passed a NULL to start with an empty sequence.  */

/* Set the code for statement G to CODE.  */

static inline void
gimple_set_code (gimple g, enum gimple_code code)
{
  g->code = code;
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

  if (GATHER_STATISTICS)
    {
      enum gimple_alloc_kind kind = gimple_alloc_kind (code);
      gimple_alloc_counts[(int) kind]++;
      gimple_alloc_sizes[(int) kind] += size;
    }

  stmt = ggc_alloc_cleared_gimple_statement_stat (size PASS_MEM_STAT);
  gimple_set_code (stmt, code);
  gimple_set_num_ops (stmt, num_ops);

  /* Do not call gimple_set_modified here as it has other side
     effects and this tuple is still not completely built.  */
  stmt->modified = 1;
  gimple_init_singleton (stmt);

  return stmt;
}

/* Set SUBCODE to be the code of the expression computed by statement G.  */

static inline void
gimple_set_subcode (gimple g, unsigned subcode)
{
  /* We only have 16 bits for the RHS code.  Assert that we are not
     overflowing it.  */
  gcc_assert (subcode < (1 << 16));
  g->subcode = subcode;
}



/* Build a tuple with operands.  CODE is the statement to build (which
   must be one of the GIMPLE_WITH_OPS tuples).  SUBCODE is the subcode
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

/* Helper for gimple_build_call, gimple_build_call_valist,
   gimple_build_call_vec and gimple_build_call_from_tree.  Build the basic
   components of a GIMPLE_CALL statement to function FN with NARGS
   arguments.  */

static inline gimple
gimple_build_call_1 (tree fn, unsigned nargs)
{
  gimple s = gimple_build_with_ops (GIMPLE_CALL, ERROR_MARK, nargs + 3);
  if (TREE_CODE (fn) == FUNCTION_DECL)
    fn = build_fold_addr_expr (fn);
  gimple_set_op (s, 1, fn);
  gimple_call_set_fntype (s, TREE_TYPE (TREE_TYPE (fn)));
  gimple_call_reset_alias_info (s);
  return s;
}


/* Build a GIMPLE_CALL statement to function FN with the arguments
   specified in vector ARGS.  */

gimple
gimple_build_call_vec (tree fn, vec<tree> args)
{
  unsigned i;
  unsigned nargs = args.length ();
  gimple call = gimple_build_call_1 (fn, nargs);

  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, args[i]);

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


/* Build a GIMPLE_CALL statement to function FN.  NARGS is the number of
   arguments.  AP contains the arguments.  */

gimple
gimple_build_call_valist (tree fn, unsigned nargs, va_list ap)
{
  gimple call;
  unsigned i;

  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL || is_gimple_call_addr (fn));

  call = gimple_build_call_1 (fn, nargs);

  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, va_arg (ap, tree));

  return call;
}


/* Helper for gimple_build_call_internal and gimple_build_call_internal_vec.
   Build the basic components of a GIMPLE_CALL statement to internal
   function FN with NARGS arguments.  */

static inline gimple
gimple_build_call_internal_1 (enum internal_fn fn, unsigned nargs)
{
  gimple s = gimple_build_with_ops (GIMPLE_CALL, ERROR_MARK, nargs + 3);
  s->subcode |= GF_CALL_INTERNAL;
  gimple_call_set_internal_fn (s, fn);
  gimple_call_reset_alias_info (s);
  return s;
}


/* Build a GIMPLE_CALL statement to internal function FN.  NARGS is
   the number of arguments.  The ... are the arguments.  */

gimple
gimple_build_call_internal (enum internal_fn fn, unsigned nargs, ...)
{
  va_list ap;
  gimple call;
  unsigned i;

  call = gimple_build_call_internal_1 (fn, nargs);
  va_start (ap, nargs);
  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, va_arg (ap, tree));
  va_end (ap);

  return call;
}


/* Build a GIMPLE_CALL statement to internal function FN with the arguments
   specified in vector ARGS.  */

gimple
gimple_build_call_internal_vec (enum internal_fn fn, vec<tree> args)
{
  unsigned i, nargs;
  gimple call;

  nargs = args.length ();
  call = gimple_build_call_internal_1 (fn, nargs);
  for (i = 0; i < nargs; i++)
    gimple_call_set_arg (call, i, args[i]);

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
  gimple_call_set_return_slot_opt (call, CALL_EXPR_RETURN_SLOT_OPT (t));
  if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA_WITH_ALIGN))
    gimple_call_set_alloca_for_var (call, CALL_ALLOCA_FOR_VAR_P (t));
  else
    gimple_call_set_from_thunk (call, CALL_FROM_THUNK_P (t));
  gimple_call_set_va_arg_pack (call, CALL_EXPR_VA_ARG_PACK (t));
  gimple_call_set_nothrow (call, TREE_NOTHROW (t));
  gimple_set_no_warning (call, TREE_NO_WARNING (t));

  return call;
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
  return gimple_build_assign_with_ops (subcode, lhs, op1, op2, op3
				       PASS_MEM_STAT);
}


/* Build a GIMPLE_ASSIGN statement with subcode SUBCODE and operands
   OP1 and OP2.  If OP2 is NULL then SUBCODE must be of class
   GIMPLE_UNARY_RHS or GIMPLE_SINGLE_RHS.  */

gimple
gimple_build_assign_with_ops (enum tree_code subcode, tree lhs, tree op1,
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

gimple
gimple_build_assign_with_ops (enum tree_code subcode, tree lhs, tree op1,
			      tree op2 MEM_STAT_DECL)
{
  return gimple_build_assign_with_ops (subcode, lhs, op1, op2, NULL_TREE
				       PASS_MEM_STAT);
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
  gimple_statement_asm *p;
  int size = strlen (string);

  /* ASMs with labels cannot have outputs.  This should have been
     enforced by the front end.  */
  gcc_assert (nlabels == 0 || noutputs == 0);

  p = as_a <gimple_statement_asm *> (
        gimple_build_with_ops (GIMPLE_ASM, ERROR_MARK,
			       ninputs + noutputs + nclobbers + nlabels));

  p->ni = ninputs;
  p->no = noutputs;
  p->nc = nclobbers;
  p->nl = nlabels;
  p->string = ggc_alloc_string (string, size);

  if (GATHER_STATISTICS)
    gimple_alloc_sizes[(int) gimple_alloc_kind (GIMPLE_ASM)] += size;

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
gimple_build_asm_vec (const char *string, vec<tree, va_gc> *inputs,
                      vec<tree, va_gc> *outputs, vec<tree, va_gc> *clobbers,
		      vec<tree, va_gc> *labels)
{
  gimple p;
  unsigned i;

  p = gimple_build_asm_1 (string,
                          vec_safe_length (inputs),
                          vec_safe_length (outputs),
                          vec_safe_length (clobbers),
			  vec_safe_length (labels));

  for (i = 0; i < vec_safe_length (inputs); i++)
    gimple_asm_set_input_op (p, i, (*inputs)[i]);

  for (i = 0; i < vec_safe_length (outputs); i++)
    gimple_asm_set_output_op (p, i, (*outputs)[i]);

  for (i = 0; i < vec_safe_length (clobbers); i++)
    gimple_asm_set_clobber_op (p, i, (*clobbers)[i]);

  for (i = 0; i < vec_safe_length (labels); i++)
    gimple_asm_set_label_op (p, i, (*labels)[i]);

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

/* Build a GIMPLE_EH_ELSE statement.  */

gimple
gimple_build_eh_else (gimple_seq n_body, gimple_seq e_body)
{
  gimple p = gimple_alloc (GIMPLE_EH_ELSE, 0);
  gimple_eh_else_set_n_body (p, n_body);
  gimple_eh_else_set_e_body (p, e_body);
  return p;
}

/* Build a GIMPLE_TRY statement.

   EVAL is the expression to evaluate.
   CLEANUP is the cleanup expression.
   KIND is either GIMPLE_TRY_CATCH or GIMPLE_TRY_FINALLY depending on
   whether this is a try/catch or a try/finally respectively.  */

gimple_statement_try *
gimple_build_try (gimple_seq eval, gimple_seq cleanup,
    		  enum gimple_try_flags kind)
{
  gimple_statement_try *p;

  gcc_assert (kind == GIMPLE_TRY_CATCH || kind == GIMPLE_TRY_FINALLY);
  p = as_a <gimple_statement_try *> (gimple_alloc (GIMPLE_TRY, 0));
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
  gimple_statement_resx *p =
    as_a <gimple_statement_resx *> (
      gimple_build_with_ops (GIMPLE_RESX, ERROR_MARK, 0));
  p->region = region;
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
  gcc_checking_assert (default_label);
  gimple p = gimple_build_with_ops (GIMPLE_SWITCH, ERROR_MARK,
				    1 + 1 + nlabels);
  gimple_switch_set_index (p, index);
  gimple_switch_set_default_label (p, default_label);
  return p;
}

/* Build a GIMPLE_SWITCH statement.

   INDEX is the switch's index.
   DEFAULT_LABEL is the default label
   ARGS is a vector of labels excluding the default.  */

gimple
gimple_build_switch (tree index, tree default_label, vec<tree> args)
{
  unsigned i, nlabels = args.length ();

  gimple p = gimple_build_switch_nlabels (nlabels, index, default_label);

  /* Copy the labels from the vector to the switch statement.  */
  for (i = 0; i < nlabels; i++)
    gimple_switch_set_label (p, i + 1, args[i]);

  return p;
}

/* Build a GIMPLE_EH_DISPATCH statement.  */

gimple
gimple_build_eh_dispatch (int region)
{
  gimple_statement_eh_dispatch *p =
    as_a <gimple_statement_eh_dispatch *> (
      gimple_build_with_ops (GIMPLE_EH_DISPATCH, ERROR_MARK, 0));
  p->region = region;
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
    gimple_set_location (p, gimple_location (stmt));

  return p;
}


/* Build a new GIMPLE_DEBUG_SOURCE_BIND statement.

   VAR is bound to VALUE; block and location are taken from STMT.  */

gimple
gimple_build_debug_source_bind_stat (tree var, tree value,
				     gimple stmt MEM_STAT_DECL)
{
  gimple p = gimple_build_with_ops_stat (GIMPLE_DEBUG,
					 (unsigned)GIMPLE_DEBUG_SOURCE_BIND, 2
					 PASS_MEM_STAT);

  gimple_debug_source_bind_set_var (p, var);
  gimple_debug_source_bind_set_value (p, value);
  if (stmt)
    gimple_set_location (p, gimple_location (stmt));

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
   KIND is the `for' variant.
   CLAUSES, are any of the OMP loop construct's clauses: private, firstprivate,
   lastprivate, reductions, ordered, schedule, and nowait.
   COLLAPSE is the collapse count.
   PRE_BODY is the sequence of statements that are loop invariant.  */

gimple
gimple_build_omp_for (gimple_seq body, int kind, tree clauses, size_t collapse,
		      gimple_seq pre_body)
{
  gimple_statement_omp_for *p =
    as_a <gimple_statement_omp_for *> (gimple_alloc (GIMPLE_OMP_FOR, 0));
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_for_set_clauses (p, clauses);
  gimple_omp_for_set_kind (p, kind);
  p->collapse = collapse;
  p->iter =  static_cast <struct gimple_omp_for_iter *> (
   ggc_internal_cleared_vec_alloc_stat (sizeof (*p->iter),
					collapse MEM_STAT_INFO));

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


/* Build a GIMPLE_OMP_TASKGROUP statement.

   BODY is the sequence of statements to be executed by the taskgroup
   construct.  */

gimple
gimple_build_omp_taskgroup (gimple_seq body)
{
  gimple p = gimple_alloc (GIMPLE_OMP_TASKGROUP, 0);
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


/* Build a GIMPLE_OMP_TARGET statement.

   BODY is the sequence of statements that will be executed.
   CLAUSES are any of the OMP target construct's clauses.  */

gimple
gimple_build_omp_target (gimple_seq body, int kind, tree clauses)
{
  gimple p = gimple_alloc (GIMPLE_OMP_TARGET, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_target_set_clauses (p, clauses);
  gimple_omp_target_set_kind (p, kind);

  return p;
}


/* Build a GIMPLE_OMP_TEAMS statement.

   BODY is the sequence of statements that will be executed.
   CLAUSES are any of the OMP teams construct's clauses.  */

gimple
gimple_build_omp_teams (gimple_seq body, tree clauses)
{
  gimple p = gimple_alloc (GIMPLE_OMP_TEAMS, 0);
  if (body)
    gimple_omp_set_body (p, body);
  gimple_omp_teams_set_clauses (p, clauses);

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

/* Build a GIMPLE_TRANSACTION statement.  */

gimple
gimple_build_transaction (gimple_seq body, tree label)
{
  gimple p = gimple_alloc (GIMPLE_TRANSACTION, 0);
  gimple_transaction_set_body (p, body);
  gimple_transaction_set_label (p, label);
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
		  get_tree_code_name (subcode),
		  gimple_code_name[gimple_code (gs)],
		  gs->subcode > 0
		    ? get_tree_code_name ((enum tree_code) gs->subcode)
		    : "",
		  function, trim_filename (file), line);
}
#endif /* ENABLE_GIMPLE_CHECKING */


/* Link gimple statement GS to the end of the sequence *SEQ_P.  If
   *SEQ_P is NULL, a new sequence is allocated.  */

void
gimple_seq_add_stmt (gimple_seq *seq_p, gimple gs)
{
  gimple_stmt_iterator si;
  if (gs == NULL)
    return;

  si = gsi_last (*seq_p);
  gsi_insert_after (&si, gs, GSI_NEW_STMT);
}

/* Link gimple statement GS to the end of the sequence *SEQ_P.  If
   *SEQ_P is NULL, a new sequence is allocated.  This function is
   similar to gimple_seq_add_stmt, but does not scan the operands.
   During gimplification, we need to manipulate statement sequences
   before the def/use vectors have been constructed.  */

void
gimple_seq_add_stmt_without_update (gimple_seq *seq_p, gimple gs)
{
  gimple_stmt_iterator si;

  if (gs == NULL)
    return;

  si = gsi_last (*seq_p);
  gsi_insert_after_without_update (&si, gs, GSI_NEW_STMT);
}

/* Append sequence SRC to the end of sequence *DST_P.  If *DST_P is
   NULL, a new sequence is allocated.  */

void
gimple_seq_add_seq (gimple_seq *dst_p, gimple_seq src)
{
  gimple_stmt_iterator si;
  if (src == NULL)
    return;

  si = gsi_last (*dst_p);
  gsi_insert_seq_after (&si, src, GSI_NEW_STMT);
}

/* Determine whether to assign a location to the statement GS.  */

static bool
should_carry_location_p (gimple gs)
{
  /* Don't emit a line note for a label.  We particularly don't want to
     emit one for the break label, since it doesn't actually correspond
     to the beginning of the loop/switch.  */
  if (gimple_code (gs) == GIMPLE_LABEL)
    return false;

  return true;
}

/* Set the location for gimple statement GS to LOCATION.  */

static void
annotate_one_with_location (gimple gs, location_t location)
{
  if (!gimple_has_location (gs)
      && !gimple_do_not_emit_location_p (gs)
      && should_carry_location_p (gs))
    gimple_set_location (gs, location);
}

/* Set LOCATION for all the statements after iterator GSI in sequence
   SEQ.  If GSI is pointing to the end of the sequence, start with the
   first statement in SEQ.  */

void
annotate_all_with_location_after (gimple_seq seq, gimple_stmt_iterator gsi,
				  location_t location)
{
  if (gsi_end_p (gsi))
    gsi = gsi_start (seq);
  else
    gsi_next (&gsi);

  for (; !gsi_end_p (gsi); gsi_next (&gsi))
    annotate_one_with_location (gsi_stmt (gsi), location);
}

/* Set the location for all the statements in a sequence STMT_P to LOCATION.  */

void
annotate_all_with_location (gimple_seq stmt_p, location_t location)
{
  gimple_stmt_iterator i;

  if (gimple_seq_empty_p (stmt_p))
    return;

  for (i = gsi_start (stmt_p); !gsi_end_p (i); gsi_next (&i))
    {
      gimple gs = gsi_stmt (i);
      annotate_one_with_location (gs, location);
    }
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
  gimple_seq new_seq = NULL;
  gimple stmt;

  for (gsi = gsi_start (src); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gimple_copy (gsi_stmt (gsi));
      gimple_seq_add_stmt (&new_seq, stmt);
    }

  return new_seq;
}



/* Return true if calls C1 and C2 are known to go to the same function.  */

bool
gimple_call_same_target_p (const_gimple c1, const_gimple c2)
{
  if (gimple_call_internal_p (c1))
    return (gimple_call_internal_p (c2)
	    && gimple_call_internal_fn (c1) == gimple_call_internal_fn (c2));
  else
    return (gimple_call_fn (c1) == gimple_call_fn (c2)
	    || (gimple_call_fndecl (c1)
		&& gimple_call_fndecl (c1) == gimple_call_fndecl (c2)));
}

/* Detect flags from a GIMPLE_CALL.  This is just like
   call_expr_flags, but for gimple tuples.  */

int
gimple_call_flags (const_gimple stmt)
{
  int flags;
  tree decl = gimple_call_fndecl (stmt);

  if (decl)
    flags = flags_from_decl_or_type (decl);
  else if (gimple_call_internal_p (stmt))
    flags = internal_fn_flags (gimple_call_internal_fn (stmt));
  else
    flags = flags_from_decl_or_type (gimple_call_fntype (stmt));

  if (stmt->subcode & GF_CALL_NOTHROW)
    flags |= ECF_NOTHROW;

  return flags;
}

/* Return the "fn spec" string for call STMT.  */

static tree
gimple_call_fnspec (const_gimple stmt)
{
  tree type, attr;

  type = gimple_call_fntype (stmt);
  if (!type)
    return NULL_TREE;

  attr = lookup_attribute ("fn spec", TYPE_ATTRIBUTES (type));
  if (!attr)
    return NULL_TREE;

  return TREE_VALUE (TREE_VALUE (attr));
}

/* Detects argument flags for argument number ARG on call STMT.  */

int
gimple_call_arg_flags (const_gimple stmt, unsigned arg)
{
  tree attr = gimple_call_fnspec (stmt);

  if (!attr || 1 + arg >= (unsigned) TREE_STRING_LENGTH (attr))
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
  tree attr;

  if (gimple_call_flags (stmt) & ECF_MALLOC)
    return ERF_NOALIAS;

  attr = gimple_call_fnspec (stmt);
  if (!attr || TREE_STRING_LENGTH (attr) < 1)
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
  stmt->bb = bb;

  if (gimple_code (stmt) != GIMPLE_LABEL)
    return;

  /* If the statement is a label, add the label to block-to-labels map
     so that we can speed up edge creation for GIMPLE_GOTOs.  */
  if (cfun->cfg)
    {
      tree t;
      int uid;

      t = gimple_label_label (stmt);
      uid = LABEL_DECL_UID (t);
      if (uid == -1)
	{
	  unsigned old_len =
	    vec_safe_length (label_to_block_map_for_fn (cfun));
	  LABEL_DECL_UID (t) = uid = cfun->cfg->last_label_uid++;
	  if (old_len <= (unsigned) uid)
	    {
	      unsigned new_len = 3 * uid / 2 + 1;

	      vec_safe_grow_cleared (label_to_block_map_for_fn (cfun),
				     new_len);
	    }
	}

      (*label_to_block_map_for_fn (cfun))[uid] = bb;
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
      gimple_init_singleton (new_stmt);
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
    gcc_unreachable ();
}


/* Return a deep copy of statement STMT.  All the operands from STMT
   are reallocated and copied using unshare_expr.  The DEF, USE, VDEF
   and VUSE operand arrays are set to empty in the new copy.  The new
   copy isn't part of any sequence.  */

gimple
gimple_copy (gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);
  unsigned num_ops = gimple_num_ops (stmt);
  gimple copy = gimple_alloc (code, num_ops);
  unsigned i;

  /* Shallow copy all the fields from STMT.  */
  memcpy (copy, stmt, gimple_size (code));
  gimple_init_singleton (copy);

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

	case GIMPLE_EH_ELSE:
	  new_seq = gimple_seq_copy (gimple_eh_else_n_body (stmt));
	  gimple_eh_else_set_n_body (copy, new_seq);
	  new_seq = gimple_seq_copy (gimple_eh_else_e_body (stmt));
	  gimple_eh_else_set_e_body (copy, new_seq);
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
	  {
	    gimple_statement_omp_for *omp_for_copy =
	      as_a <gimple_statement_omp_for *> (copy);
	    omp_for_copy->iter =
	      static_cast <struct gimple_omp_for_iter *> (
		  ggc_internal_vec_alloc_stat (sizeof (struct gimple_omp_for_iter),
					       gimple_omp_for_collapse (stmt)
					       MEM_STAT_INFO));
          }
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
	case GIMPLE_OMP_TARGET:
	case GIMPLE_OMP_TEAMS:
	case GIMPLE_OMP_SECTION:
	case GIMPLE_OMP_MASTER:
	case GIMPLE_OMP_TASKGROUP:
	case GIMPLE_OMP_ORDERED:
	copy_omp_body:
	  new_seq = gimple_seq_copy (gimple_omp_body (stmt));
	  gimple_omp_set_body (copy, new_seq);
	  break;

	case GIMPLE_TRANSACTION:
	  new_seq = gimple_seq_copy (gimple_transaction_body (stmt));
	  gimple_transaction_set_body (copy, new_seq);
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
  for (i = 0; i < num_ops; i++)
    gimple_set_op (copy, i, unshare_expr (gimple_op (stmt, i)));

  if (gimple_has_mem_ops (stmt))
    {
      gimple_set_vdef (copy, gimple_vdef (stmt));
      gimple_set_vuse (copy, gimple_vuse (stmt));
    }

  /* Clear out SSA operand vectors on COPY.  */
  if (gimple_has_ops (stmt))
    {
      gimple_set_use_ops (copy, NULL);

      /* SSA operands need to be updated.  */
      gimple_set_modified (copy, true);
    }

  return copy;
}


/* Return true if statement S has side-effects.  We consider a
   statement to have side effects if:

   - It is a GIMPLE_CALL not marked with ECF_PURE or ECF_CONST.
   - Any of its operands are marked TREE_THIS_VOLATILE or TREE_SIDE_EFFECTS.  */

bool
gimple_has_side_effects (const_gimple s)
{
  if (is_gimple_debug (s))
    return false;

  /* We don't have to scan the arguments to check for
     volatile arguments, though, at present, we still
     do a scan to check for TREE_SIDE_EFFECTS.  */
  if (gimple_has_volatile_ops (s))
    return true;

  if (gimple_code (s) == GIMPLE_ASM
      && gimple_asm_volatile_p (s))
    return true;

  if (is_gimple_call (s))
    {
      int flags = gimple_call_flags (s);

      /* An infinite loop is considered a side effect.  */
      if (!(flags & (ECF_CONST | ECF_PURE))
	  || (flags & ECF_LOOPING_CONST_OR_PURE))
	return true;

      return false;
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
  int i, total_tuples = 0, total_bytes = 0;

  if (! GATHER_STATISTICS)
    {
      fprintf (stderr, "No gimple statistics\n");
      return;
    }

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
   : ((SYM) == COND_EXPR						    \
      || (SYM) == WIDEN_MULT_PLUS_EXPR					    \
      || (SYM) == WIDEN_MULT_MINUS_EXPR					    \
      || (SYM) == DOT_PROD_EXPR						    \
      || (SYM) == REALIGN_LOAD_EXPR					    \
      || (SYM) == VEC_COND_EXPR						    \
      || (SYM) == VEC_PERM_EXPR                                             \
      || (SYM) == FMA_EXPR) ? GIMPLE_TERNARY_RHS			    \
   : ((SYM) == CONSTRUCTOR						    \
      || (SYM) == OBJ_TYPE_REF						    \
      || (SYM) == ASSERT_EXPR						    \
      || (SYM) == ADDR_EXPR						    \
      || (SYM) == WITH_SIZE_EXPR					    \
      || (SYM) == SSA_NAME) ? GIMPLE_SINGLE_RHS				    \
   : GIMPLE_INVALID_RHS),
#define END_OF_BASE_TREE_CODES (unsigned char) GIMPLE_INVALID_RHS,

const unsigned char gimple_rhs_class_table[] = {
#include "all-tree.def"
};

#undef DEFTREECODE
#undef END_OF_BASE_TREE_CODES

/* Canonicalize a tree T for use in a COND_EXPR as conditional.  Returns
   a canonicalized tree that is valid for a COND_EXPR or NULL_TREE, if
   we failed to create one.  */

tree
canonicalize_cond_expr_cond (tree t)
{
  /* Strip conversions around boolean operations.  */
  if (CONVERT_EXPR_P (t)
      && (truth_value_p (TREE_CODE (TREE_OPERAND (t, 0)))
          || TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0)))
	     == BOOLEAN_TYPE))
    t = TREE_OPERAND (t, 0);

  /* For !x use x == 0.  */
  if (TREE_CODE (t) == TRUTH_NOT_EXPR)
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
  /* For x ^ y use x != y.  */
  else if (TREE_CODE (t) == BIT_XOR_EXPR)
    t = build2 (NE_EXPR, TREE_TYPE (t),
		TREE_OPERAND (t, 0), TREE_OPERAND (t, 1));
  
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
  int nargs = gimple_call_num_args (stmt);
  auto_vec<tree> vargs (nargs);
  gimple new_stmt;

  for (i = 0; i < nargs; i++)
    if (!bitmap_bit_p (args_to_skip, i))
      vargs.quick_push (gimple_call_arg (stmt, i));

  if (gimple_call_internal_p (stmt))
    new_stmt = gimple_build_call_internal_vec (gimple_call_internal_fn (stmt),
					       vargs);
  else
    new_stmt = gimple_build_call_vec (gimple_call_fn (stmt), vargs);

  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
  gimple_set_vdef (new_stmt, gimple_vdef (stmt));

  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));

  gimple_set_modified (new_stmt, true);

  return new_stmt;
}



/* Return true if the field decls F1 and F2 are at the same offset.

   This is intended to be used on GIMPLE types only.  */

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
  if (tree_fits_shwi_p (DECL_FIELD_OFFSET (f1))
      && tree_fits_shwi_p (DECL_FIELD_OFFSET (f2)))
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


/* Helper for gimple_ior_addresses_taken_1.  */

static bool
gimple_ior_addresses_taken_1 (gimple, tree addr, tree, void *data)
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


/* Return true if TYPE1 and TYPE2 are compatible enough for builtin
   processing.  */

static bool
validate_type (tree type1, tree type2)
{
  if (INTEGRAL_TYPE_P (type1)
      && INTEGRAL_TYPE_P (type2))
    ;
  else if (POINTER_TYPE_P (type1)
	   && POINTER_TYPE_P (type2))
    ;
  else if (TREE_CODE (type1)
	   != TREE_CODE (type2))
    return false;
  return true;
}

/* Return true when STMTs arguments and return value match those of FNDECL,
   a decl of a builtin function.  */

bool
gimple_builtin_call_types_compatible_p (gimple stmt, tree fndecl)
{
  gcc_checking_assert (DECL_BUILT_IN_CLASS (fndecl) != NOT_BUILT_IN);

  tree ret = gimple_call_lhs (stmt);
  if (ret
      && !validate_type (TREE_TYPE (ret), TREE_TYPE (TREE_TYPE (fndecl))))
    return false;

  tree targs = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  unsigned nargs = gimple_call_num_args (stmt);
  for (unsigned i = 0; i < nargs; ++i)
    {
      /* Variadic args follow.  */
      if (!targs)
	return true;
      tree arg = gimple_call_arg (stmt, i);
      if (!validate_type (TREE_TYPE (arg), TREE_VALUE (targs)))
	return false;
      targs = TREE_CHAIN (targs);
    }
  if (targs && !VOID_TYPE_P (TREE_VALUE (targs)))
    return false;
  return true;
}

/* Return true when STMT is builtins call.  */

bool
gimple_call_builtin_p (gimple stmt)
{
  tree fndecl;
  if (is_gimple_call (stmt)
      && (fndecl = gimple_call_fndecl (stmt)) != NULL_TREE
      && DECL_BUILT_IN_CLASS (fndecl) != NOT_BUILT_IN)
    return gimple_builtin_call_types_compatible_p (stmt, fndecl);
  return false;
}

/* Return true when STMT is builtins call to CLASS.  */

bool
gimple_call_builtin_p (gimple stmt, enum built_in_class klass)
{
  tree fndecl;
  if (is_gimple_call (stmt)
      && (fndecl = gimple_call_fndecl (stmt)) != NULL_TREE
      && DECL_BUILT_IN_CLASS (fndecl) == klass)
    return gimple_builtin_call_types_compatible_p (stmt, fndecl);
  return false;
}

/* Return true when STMT is builtins call to CODE of CLASS.  */

bool
gimple_call_builtin_p (gimple stmt, enum built_in_function code)
{
  tree fndecl;
  if (is_gimple_call (stmt)
      && (fndecl = gimple_call_fndecl (stmt)) != NULL_TREE
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL 
      && DECL_FUNCTION_CODE (fndecl) == code)
    return gimple_builtin_call_types_compatible_p (stmt, fndecl);
  return false;
}

/* Return true if STMT clobbers memory.  STMT is required to be a
   GIMPLE_ASM.  */

bool
gimple_asm_clobbers_memory_p (const_gimple stmt)
{
  unsigned i;

  for (i = 0; i < gimple_asm_nclobbers (stmt); i++)
    {
      tree op = gimple_asm_clobber_op (stmt, i);
      if (strcmp (TREE_STRING_POINTER (TREE_VALUE (op)), "memory") == 0)
	return true;
    }

  return false;
}

/* Dump bitmap SET (assumed to contain VAR_DECLs) to FILE.  */

void
dump_decl_set (FILE *file, bitmap set)
{
  if (set)
    {
      bitmap_iterator bi;
      unsigned i;

      fprintf (file, "{ ");

      EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
	{
	  fprintf (file, "D.%u", i);
	  fprintf (file, " ");
	}

      fprintf (file, "}");
    }
  else
    fprintf (file, "NIL");
}

/* Return true when CALL is a call stmt that definitely doesn't
   free any memory or makes it unavailable otherwise.  */
bool
nonfreeing_call_p (gimple call)
{
  if (gimple_call_builtin_p (call, BUILT_IN_NORMAL)
      && gimple_call_flags (call) & ECF_LEAF)
    switch (DECL_FUNCTION_CODE (gimple_call_fndecl (call)))
      {
	/* Just in case these become ECF_LEAF in the future.  */
	case BUILT_IN_FREE:
	case BUILT_IN_TM_FREE:
	case BUILT_IN_REALLOC:
	case BUILT_IN_STACK_RESTORE:
	  return false;
	default:
	  return true;
      }

  return false;
}

/* Callback for walk_stmt_load_store_ops.
 
   Return TRUE if OP will dereference the tree stored in DATA, FALSE
   otherwise.

   This routine only makes a superficial check for a dereference.  Thus
   it must only be used if it is safe to return a false negative.  */
static bool
check_loadstore (gimple, tree op, tree, void *data)
{
  if ((TREE_CODE (op) == MEM_REF || TREE_CODE (op) == TARGET_MEM_REF)
      && operand_equal_p (TREE_OPERAND (op, 0), (tree)data, 0))
    return true;
  return false;
}

/* If OP can be inferred to be non-NULL after STMT executes, return true.

   DEREFERENCE is TRUE if we can use a pointer dereference to infer a
   non-NULL range, FALSE otherwise.

   ATTRIBUTE is TRUE if we can use attributes to infer a non-NULL range
   for function arguments and return values.  FALSE otherwise.  */

bool
infer_nonnull_range (gimple stmt, tree op, bool dereference, bool attribute)
{
  /* We can only assume that a pointer dereference will yield
     non-NULL if -fdelete-null-pointer-checks is enabled.  */
  if (!flag_delete_null_pointer_checks
      || !POINTER_TYPE_P (TREE_TYPE (op))
      || gimple_code (stmt) == GIMPLE_ASM)
    return false;

  if (dereference
      && walk_stmt_load_store_ops (stmt, (void *)op,
				   check_loadstore, check_loadstore))
    return true;

  if (attribute
      && is_gimple_call (stmt) && !gimple_call_internal_p (stmt))
    {
      tree fntype = gimple_call_fntype (stmt);
      tree attrs = TYPE_ATTRIBUTES (fntype);
      for (; attrs; attrs = TREE_CHAIN (attrs))
	{
	  attrs = lookup_attribute ("nonnull", attrs);

	  /* If "nonnull" wasn't specified, we know nothing about
	     the argument.  */
	  if (attrs == NULL_TREE)
	    return false;

	  /* If "nonnull" applies to all the arguments, then ARG
	     is non-null if it's in the argument list.  */
	  if (TREE_VALUE (attrs) == NULL_TREE)
	    {
	      for (unsigned int i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (stmt, i)))
		      && operand_equal_p (op, gimple_call_arg (stmt, i), 0))
		    return true;
		}
	      return false;
	    }

	  /* Now see if op appears in the nonnull list.  */
	  for (tree t = TREE_VALUE (attrs); t; t = TREE_CHAIN (t))
	    {
	      int idx = TREE_INT_CST_LOW (TREE_VALUE (t)) - 1;
	      tree arg = gimple_call_arg (stmt, idx);
	      if (operand_equal_p (op, arg, 0))
		return true;
	    }
	}
    }

  /* If this function is marked as returning non-null, then we can
     infer OP is non-null if it is used in the return statement.  */
  if (attribute
      && gimple_code (stmt) == GIMPLE_RETURN
      && gimple_return_retval (stmt)
      && operand_equal_p (gimple_return_retval (stmt), op, 0)
      && lookup_attribute ("returns_nonnull",
			   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return true;

  return false;
}

/* Compare two case labels.  Because the front end should already have
   made sure that case ranges do not overlap, it is enough to only compare
   the CASE_LOW values of each case label.  */

static int
compare_case_labels (const void *p1, const void *p2)
{
  const_tree const case1 = *(const_tree const*)p1;
  const_tree const case2 = *(const_tree const*)p2;

  /* The 'default' case label always goes first.  */
  if (!CASE_LOW (case1))
    return -1;
  else if (!CASE_LOW (case2))
    return 1;
  else
    return tree_int_cst_compare (CASE_LOW (case1), CASE_LOW (case2));
}

/* Sort the case labels in LABEL_VEC in place in ascending order.  */

void
sort_case_labels (vec<tree> label_vec)
{
  label_vec.qsort (compare_case_labels);
}

/* Prepare a vector of case labels to be used in a GIMPLE_SWITCH statement.

   LABELS is a vector that contains all case labels to look at.

   INDEX_TYPE is the type of the switch index expression.  Case labels
   in LABELS are discarded if their values are not in the value range
   covered by INDEX_TYPE.  The remaining case label values are folded
   to INDEX_TYPE.

   If a default case exists in LABELS, it is removed from LABELS and
   returned in DEFAULT_CASEP.  If no default case exists, but the
   case labels already cover the whole range of INDEX_TYPE, a default
   case is returned pointing to one of the existing case labels.
   Otherwise DEFAULT_CASEP is set to NULL_TREE.

   DEFAULT_CASEP may be NULL, in which case the above comment doesn't
   apply and no action is taken regardless of whether a default case is
   found or not.  */

void
preprocess_case_label_vec_for_gimple (vec<tree> labels,
				      tree index_type,
				      tree *default_casep)
{
  tree min_value, max_value;
  tree default_case = NULL_TREE;
  size_t i, len;

  i = 0;
  min_value = TYPE_MIN_VALUE (index_type);
  max_value = TYPE_MAX_VALUE (index_type);
  while (i < labels.length ())
    {
      tree elt = labels[i];
      tree low = CASE_LOW (elt);
      tree high = CASE_HIGH (elt);
      bool remove_element = FALSE;

      if (low)
	{
	  gcc_checking_assert (TREE_CODE (low) == INTEGER_CST);
	  gcc_checking_assert (!high || TREE_CODE (high) == INTEGER_CST);

	  /* This is a non-default case label, i.e. it has a value.

	     See if the case label is reachable within the range of
	     the index type.  Remove out-of-range case values.  Turn
	     case ranges into a canonical form (high > low strictly)
	     and convert the case label values to the index type.

	     NB: The type of gimple_switch_index() may be the promoted
	     type, but the case labels retain the original type.  */

	  if (high)
	    {
	      /* This is a case range.  Discard empty ranges.
		 If the bounds or the range are equal, turn this
		 into a simple (one-value) case.  */
	      int cmp = tree_int_cst_compare (high, low);
	      if (cmp < 0)
		remove_element = TRUE;
	      else if (cmp == 0)
		high = NULL_TREE;
	    }

	  if (! high)
	    {
	      /* If the simple case value is unreachable, ignore it.  */
	      if ((TREE_CODE (min_value) == INTEGER_CST
		   && tree_int_cst_compare (low, min_value) < 0)
		  || (TREE_CODE (max_value) == INTEGER_CST
		      && tree_int_cst_compare (low, max_value) > 0))
		remove_element = TRUE;
	      else
		low = fold_convert (index_type, low);
	    }
	  else
	    {
	      /* If the entire case range is unreachable, ignore it.  */
	      if ((TREE_CODE (min_value) == INTEGER_CST
		   && tree_int_cst_compare (high, min_value) < 0)
		  || (TREE_CODE (max_value) == INTEGER_CST
		      && tree_int_cst_compare (low, max_value) > 0))
		remove_element = TRUE;
	      else
		{
		  /* If the lower bound is less than the index type's
		     minimum value, truncate the range bounds.  */
		  if (TREE_CODE (min_value) == INTEGER_CST
		      && tree_int_cst_compare (low, min_value) < 0)
		    low = min_value;
		  low = fold_convert (index_type, low);

		  /* If the upper bound is greater than the index type's
		     maximum value, truncate the range bounds.  */
		  if (TREE_CODE (max_value) == INTEGER_CST
		      && tree_int_cst_compare (high, max_value) > 0)
		    high = max_value;
		  high = fold_convert (index_type, high);

		  /* We may have folded a case range to a one-value case.  */
		  if (tree_int_cst_equal (low, high))
		    high = NULL_TREE;
		}
	    }

	  CASE_LOW (elt) = low;
	  CASE_HIGH (elt) = high;
	}
      else
	{
	  gcc_assert (!default_case);
	  default_case = elt;
	  /* The default case must be passed separately to the
	     gimple_build_switch routine.  But if DEFAULT_CASEP
	     is NULL, we do not remove the default case (it would
	     be completely lost).  */
	  if (default_casep)
	    remove_element = TRUE;
	}

      if (remove_element)
	labels.ordered_remove (i);
      else
	i++;
    }
  len = i;

  if (!labels.is_empty ())
    sort_case_labels (labels);

  if (default_casep && !default_case)
    {
      /* If the switch has no default label, add one, so that we jump
	 around the switch body.  If the labels already cover the whole
	 range of the switch index_type, add the default label pointing
	 to one of the existing labels.  */
      if (len
	  && TYPE_MIN_VALUE (index_type)
	  && TYPE_MAX_VALUE (index_type)
	  && tree_int_cst_equal (CASE_LOW (labels[0]),
				 TYPE_MIN_VALUE (index_type)))
	{
	  tree low, high = CASE_HIGH (labels[len - 1]);
	  if (!high)
	    high = CASE_LOW (labels[len - 1]);
	  if (tree_int_cst_equal (high, TYPE_MAX_VALUE (index_type)))
	    {
	      for (i = 1; i < len; i++)
		{
		  high = CASE_LOW (labels[i]);
		  low = CASE_HIGH (labels[i - 1]);
		  if (!low)
		    low = CASE_LOW (labels[i - 1]);
		  if ((TREE_INT_CST_LOW (low) + 1
		       != TREE_INT_CST_LOW (high))
		      || (TREE_INT_CST_HIGH (low)
			  + (TREE_INT_CST_LOW (high) == 0)
			  != TREE_INT_CST_HIGH (high)))
		    break;
		}
	      if (i == len)
		{
		  tree label = CASE_LABEL (labels[0]);
		  default_case = build_case_label (NULL_TREE, NULL_TREE,
						   label);
		}
	    }
	}
    }

  if (default_casep)
    *default_casep = default_case;
}

/* Set the location of all statements in SEQ to LOC.  */

void
gimple_seq_set_location (gimple_seq seq, location_t loc)
{
  for (gimple_stmt_iterator i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    gimple_set_location (gsi_stmt (i), loc);
}
