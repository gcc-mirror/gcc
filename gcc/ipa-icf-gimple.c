/* Interprocedural Identical Code Folding pass
   Copyright (C) 2014-2020 Free Software Foundation, Inc.

   Contributed by Jan Hubicka <hubicka@ucw.cz> and Martin Liska <mliska@suse.cz>

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "data-streamer.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "ipa-utils.h"
#include "tree-eh.h"
#include "builtins.h"
#include "cfgloop.h"
#include "attribs.h"

#include "ipa-icf-gimple.h"

namespace ipa_icf_gimple {

/* Initialize internal structures for a given SOURCE_FUNC_DECL and
   TARGET_FUNC_DECL. Strict polymorphic comparison is processed if
   an option COMPARE_POLYMORPHIC is true. For special cases, one can
   set IGNORE_LABELS to skip label comparison.
   Similarly, IGNORE_SOURCE_DECLS and IGNORE_TARGET_DECLS are sets
   of declarations that can be skipped.  */

func_checker::func_checker (tree source_func_decl, tree target_func_decl,
			    bool ignore_labels,
			    hash_set<symtab_node *> *ignored_source_nodes,
			    hash_set<symtab_node *> *ignored_target_nodes)
  : m_source_func_decl (source_func_decl), m_target_func_decl (target_func_decl),
    m_ignored_source_nodes (ignored_source_nodes),
    m_ignored_target_nodes (ignored_target_nodes),
    m_ignore_labels (ignore_labels)
{
  function *source_func = DECL_STRUCT_FUNCTION (source_func_decl);
  function *target_func = DECL_STRUCT_FUNCTION (target_func_decl);

  unsigned ssa_source = SSANAMES (source_func)->length ();
  unsigned ssa_target = SSANAMES (target_func)->length ();

  m_source_ssa_names.create (ssa_source);
  m_target_ssa_names.create (ssa_target);

  for (unsigned i = 0; i < ssa_source; i++)
    m_source_ssa_names.safe_push (-1);

  for (unsigned i = 0; i < ssa_target; i++)
    m_target_ssa_names.safe_push (-1);
}

/* Memory release routine.  */

func_checker::~func_checker ()
{
  m_source_ssa_names.release();
  m_target_ssa_names.release();
}

/* Verifies that trees T1 and T2 are equivalent from perspective of ICF.  */

bool
func_checker::compare_ssa_name (const_tree t1, const_tree t2)
{
  gcc_assert (TREE_CODE (t1) == SSA_NAME);
  gcc_assert (TREE_CODE (t2) == SSA_NAME);

  unsigned i1 = SSA_NAME_VERSION (t1);
  unsigned i2 = SSA_NAME_VERSION (t2);

  if (m_source_ssa_names[i1] == -1)
    m_source_ssa_names[i1] = i2;
  else if (m_source_ssa_names[i1] != (int) i2)
    return false;

  if(m_target_ssa_names[i2] == -1)
    m_target_ssa_names[i2] = i1;
  else if (m_target_ssa_names[i2] != (int) i1)
    return false;

  if (SSA_NAME_IS_DEFAULT_DEF (t1))
    {
      tree b1 = SSA_NAME_VAR (t1);
      tree b2 = SSA_NAME_VAR (t2);

      return compare_operand (b1, b2);
    }

  return true;
}

/* Verification function for edges E1 and E2.  */

bool
func_checker::compare_edge (edge e1, edge e2)
{
  if (e1->flags != e2->flags)
    return false;

  bool existed_p;

  edge &slot = m_edge_map.get_or_insert (e1, &existed_p);
  if (existed_p)
    return return_with_debug (slot == e2);
  else
    slot = e2;

  /* TODO: filter edge probabilities for profile feedback match.  */

  return true;
}

/* Verification function for declaration trees T1 and T2 that
   come from functions FUNC1 and FUNC2.  */

bool
func_checker::compare_decl (const_tree t1, const_tree t2)
{
  if (!auto_var_in_fn_p (t1, m_source_func_decl)
      || !auto_var_in_fn_p (t2, m_target_func_decl))
    return return_with_debug (t1 == t2);

  tree_code t = TREE_CODE (t1);
  if ((t == VAR_DECL || t == PARM_DECL || t == RESULT_DECL)
      && DECL_BY_REFERENCE (t1) != DECL_BY_REFERENCE (t2))
    return return_false_with_msg ("DECL_BY_REFERENCE flags are different");

  if (!compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return return_false ();

  bool existed_p;
  const_tree &slot = m_decl_map.get_or_insert (t1, &existed_p);
  if (existed_p)
    return return_with_debug (slot == t2);
  else
    slot = t2;

  return true;
}

/* Return true if T1 and T2 are same for purposes of ipa-polymorphic-call
   analysis.  COMPARE_PTR indicates if types of pointers needs to be
   considered.  */

bool
func_checker::compatible_polymorphic_types_p (tree t1, tree t2,
					      bool compare_ptr)
{
  gcc_assert (TREE_CODE (t1) != FUNCTION_TYPE && TREE_CODE (t1) != METHOD_TYPE);

  /* Pointer types generally give no information.  */
  if (POINTER_TYPE_P (t1))
    {
      if (!compare_ptr)
	return true;
      return func_checker::compatible_polymorphic_types_p (TREE_TYPE (t1),
							   TREE_TYPE (t2),
							   false);
    }

  /* If types contain a polymorphic types, match them.  */
  bool c1 = contains_polymorphic_type_p (t1);
  bool c2 = contains_polymorphic_type_p (t2);
  if (!c1 && !c2)
    return true;
  if (!c1 || !c2)
    return return_false_with_msg ("one type is not polymorphic");
  if (!types_must_be_same_for_odr (t1, t2))
    return return_false_with_msg ("types are not same for ODR");
  return true;
}

/* Return true if types are compatible from perspective of ICF.  */
bool
func_checker::compatible_types_p (tree t1, tree t2)
{
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return return_false_with_msg ("different tree types");

  if (TYPE_RESTRICT (t1) != TYPE_RESTRICT (t2))
    return return_false_with_msg ("restrict flags are different");

  if (!types_compatible_p (t1, t2))
    return return_false_with_msg ("types are not compatible");

  return true;
}

/* Function compare for equality given trees T1 and T2 which
   can be either a constant or a declaration type.  */

void
func_checker::hash_operand (const_tree arg, inchash::hash &hstate,
			    unsigned int flags)
{
  if (arg == NULL_TREE)
    {
      hstate.merge_hash (0);
      return;
    }

  switch (TREE_CODE (arg))
    {
    case FUNCTION_DECL:
    case VAR_DECL:
    case LABEL_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
    case SSA_NAME:
      return;
    case FIELD_DECL:
      inchash::add_expr (DECL_FIELD_OFFSET (arg), hstate, flags);
      inchash::add_expr (DECL_FIELD_BIT_OFFSET (arg), hstate, flags);
      return;
    default:
      break;
    }

  return operand_compare::hash_operand (arg, hstate, flags);
}

bool
func_checker::operand_equal_p (const_tree t1, const_tree t2,
			       unsigned int flags)
{
  bool r;
  if (verify_hash_value (t1, t2, flags, &r))
    return r;

  if (t1 == t2)
    return true;
  else if (!t1 || !t2)
    return false;

  if (TREE_CODE (t1) != TREE_CODE (t2))
    return return_false ();

  switch (TREE_CODE (t1))
    {
    case FUNCTION_DECL:
      /* All function decls are in the symbol table and known to match
	 before we start comparing bodies.  */
      return true;
    case VAR_DECL:
      return return_with_debug (compare_variable_decl (t1, t2));
    case LABEL_DECL:
      {
	int *bb1 = m_label_bb_map.get (t1);
	int *bb2 = m_label_bb_map.get (t2);
	/* Labels can point to another function (non-local GOTOs).  */
	return return_with_debug (bb1 != NULL && bb2 != NULL && *bb1 == *bb2);
      }

    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
      return compare_decl (t1, t2);
    case SSA_NAME:
      return compare_ssa_name (t1, t2);
    default:
      break;
    }

  return operand_compare::operand_equal_p (t1, t2, flags);
}

/* Function responsible for comparison of various operands T1 and T2.
   If these components, from functions FUNC1 and FUNC2, are equal, true
   is returned.  */

bool
func_checker::compare_operand (tree t1, tree t2)
{
  if (!t1 && !t2)
    return true;
  else if (!t1 || !t2)
    return false;
  if (operand_equal_p (t1, t2, OEP_MATCH_SIDE_EFFECTS))
    return true;
  return return_false_with_msg ("operand_equal_p failed");
}

bool
func_checker::compare_asm_inputs_outputs (tree t1, tree t2)
{
  gcc_assert (TREE_CODE (t1) == TREE_LIST);
  gcc_assert (TREE_CODE (t2) == TREE_LIST);

  for (; t1; t1 = TREE_CHAIN (t1))
    {
      if (!t2)
	return false;

      if (!compare_operand (TREE_VALUE (t1), TREE_VALUE (t2)))
	return return_false ();

      tree p1 = TREE_PURPOSE (t1);
      tree p2 = TREE_PURPOSE (t2);

      gcc_assert (TREE_CODE (p1) == TREE_LIST);
      gcc_assert (TREE_CODE (p2) == TREE_LIST);

      if (strcmp (TREE_STRING_POINTER (TREE_VALUE (p1)),
		  TREE_STRING_POINTER (TREE_VALUE (p2))) != 0)
	return return_false ();

      t2 = TREE_CHAIN (t2);
    }

  if (t2)
    return return_false ();

  return true;
}

/* Verifies that trees T1 and T2 do correspond.  */

bool
func_checker::compare_variable_decl (const_tree t1, const_tree t2)
{
  bool ret = false;

  if (t1 == t2)
    return true;

  if (DECL_ALIGN (t1) != DECL_ALIGN (t2))
    return return_false_with_msg ("alignments are different");

  if (DECL_HARD_REGISTER (t1) != DECL_HARD_REGISTER (t2))
    return return_false_with_msg ("DECL_HARD_REGISTER are different");

  if (DECL_HARD_REGISTER (t1)
      && DECL_ASSEMBLER_NAME_RAW (t1) != DECL_ASSEMBLER_NAME_RAW (t2))
    return return_false_with_msg ("HARD REGISTERS are different");

  /* Symbol table variables are known to match before we start comparing
     bodies.  */
  if (decl_in_symtab_p (t1))
    return decl_in_symtab_p (t2);
  ret = compare_decl (t1, t2);

  return return_with_debug (ret);
}

/* Compare loop information for basic blocks BB1 and BB2.  */

bool
func_checker::compare_loops (basic_block bb1, basic_block bb2)
{
  if ((bb1->loop_father == NULL) != (bb2->loop_father == NULL))
    return return_false ();

  class loop *l1 = bb1->loop_father;
  class loop *l2 = bb2->loop_father;
  if (l1 == NULL)
    return true;

  if ((bb1 == l1->header) != (bb2 == l2->header))
    return return_false_with_msg ("header");
  if ((bb1 == l1->latch) != (bb2 == l2->latch))
    return return_false_with_msg ("latch");
  if (l1->simdlen != l2->simdlen)
    return return_false_with_msg ("simdlen");
  if (l1->safelen != l2->safelen)
    return return_false_with_msg ("safelen");
  if (l1->can_be_parallel != l2->can_be_parallel)
    return return_false_with_msg ("can_be_parallel");
  if (l1->dont_vectorize != l2->dont_vectorize)
    return return_false_with_msg ("dont_vectorize");
  if (l1->force_vectorize != l2->force_vectorize)
    return return_false_with_msg ("force_vectorize");
  if (l1->finite_p != l2->finite_p)
    return return_false_with_msg ("finite_p");
  if (l1->unroll != l2->unroll)
    return return_false_with_msg ("unroll");
  if (!compare_variable_decl (l1->simduid, l2->simduid))
    return return_false_with_msg ("simduid");

  return true;
}

/* Function visits all gimple labels and creates corresponding
   mapping between basic blocks and labels.  */

void
func_checker::parse_labels (sem_bb *bb)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb->bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	{
	  const_tree t = gimple_label_label (label_stmt);
	  gcc_assert (TREE_CODE (t) == LABEL_DECL);

	  m_label_bb_map.put (t, bb->bb->index);
	}
    }
}

/* Basic block equivalence comparison function that returns true if
   basic blocks BB1 and BB2 (from functions FUNC1 and FUNC2) correspond.

   In general, a collection of equivalence dictionaries is built for types
   like SSA names, declarations (VAR_DECL, PARM_DECL, ..). This infrastructure
   is utilized by every statement-by-statement comparison function.  */

bool
func_checker::compare_bb (sem_bb *bb1, sem_bb *bb2)
{
  gimple_stmt_iterator gsi1, gsi2;
  gimple *s1, *s2;

  gsi1 = gsi_start_nondebug_bb (bb1->bb);
  gsi2 = gsi_start_nondebug_bb (bb2->bb);

  while (!gsi_end_p (gsi1))
    {
      if (gsi_end_p (gsi2))
	return return_false ();

      s1 = gsi_stmt (gsi1);
      s2 = gsi_stmt (gsi2);

      int eh1 = lookup_stmt_eh_lp_fn
		(DECL_STRUCT_FUNCTION (m_source_func_decl), s1);
      int eh2 = lookup_stmt_eh_lp_fn
		(DECL_STRUCT_FUNCTION (m_target_func_decl), s2);

      if (eh1 != eh2)
	return return_false_with_msg ("EH regions are different");

      if (gimple_code (s1) != gimple_code (s2))
	return return_false_with_msg ("gimple codes are different");

      switch (gimple_code (s1))
	{
	case GIMPLE_CALL:
	  if (!compare_gimple_call (as_a <gcall *> (s1),
				    as_a <gcall *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_CALL");
	  break;
	case GIMPLE_ASSIGN:
	  if (!compare_gimple_assign (s1, s2))
	    return return_different_stmts (s1, s2, "GIMPLE_ASSIGN");
	  break;
	case GIMPLE_COND:
	  if (!compare_gimple_cond (s1, s2))
	    return return_different_stmts (s1, s2, "GIMPLE_COND");
	  break;
	case GIMPLE_SWITCH:
	  if (!compare_gimple_switch (as_a <gswitch *> (s1),
				      as_a <gswitch *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_SWITCH");
	  break;
	case GIMPLE_DEBUG:
	  break;
	case GIMPLE_EH_DISPATCH:
	  if (gimple_eh_dispatch_region (as_a <geh_dispatch *> (s1))
	      != gimple_eh_dispatch_region (as_a <geh_dispatch *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_EH_DISPATCH");
	  break;
	case GIMPLE_RESX:
	  if (!compare_gimple_resx (as_a <gresx *> (s1),
				    as_a <gresx *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_RESX");
	  break;
	case GIMPLE_LABEL:
	  if (!compare_gimple_label (as_a <glabel *> (s1),
				     as_a <glabel *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_LABEL");
	  break;
	case GIMPLE_RETURN:
	  if (!compare_gimple_return (as_a <greturn *> (s1),
				      as_a <greturn *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_RETURN");
	  break;
	case GIMPLE_GOTO:
	  if (!compare_gimple_goto (s1, s2))
	    return return_different_stmts (s1, s2, "GIMPLE_GOTO");
	  break;
	case GIMPLE_ASM:
	  if (!compare_gimple_asm (as_a <gasm *> (s1),
				   as_a <gasm *> (s2)))
	    return return_different_stmts (s1, s2, "GIMPLE_ASM");
	  break;
	case GIMPLE_PREDICT:
	case GIMPLE_NOP:
	  break;
	default:
	  return return_false_with_msg ("Unknown GIMPLE code reached");
	}

      gsi_next_nondebug (&gsi1);
      gsi_next_nondebug (&gsi2);
    }

  if (!gsi_end_p (gsi2))
    return return_false ();

  if (!compare_loops (bb1->bb, bb2->bb))
    return return_false ();

  return true;
}

/* Verifies for given GIMPLEs S1 and S2 that
   call statements are semantically equivalent.  */

bool
func_checker::compare_gimple_call (gcall *s1, gcall *s2)
{
  unsigned i;
  tree t1, t2;

  if (gimple_call_num_args (s1) != gimple_call_num_args (s2))
    return false;

  t1 = gimple_call_fn (s1);
  t2 = gimple_call_fn (s2);
  if (!compare_operand (t1, t2))
    return return_false ();

  /* Compare flags.  */
  if (gimple_call_internal_p (s1) != gimple_call_internal_p (s2)
      || gimple_call_ctrl_altering_p (s1) != gimple_call_ctrl_altering_p (s2)
      || gimple_call_tail_p (s1) != gimple_call_tail_p (s2)
      || gimple_call_return_slot_opt_p (s1) != gimple_call_return_slot_opt_p (s2)
      || gimple_call_from_thunk_p (s1) != gimple_call_from_thunk_p (s2)
      || gimple_call_va_arg_pack_p (s1) != gimple_call_va_arg_pack_p (s2)
      || gimple_call_alloca_for_var_p (s1) != gimple_call_alloca_for_var_p (s2))
    return false;

  if (gimple_call_internal_p (s1)
      && gimple_call_internal_fn (s1) != gimple_call_internal_fn (s2))
    return false;

  tree fntype1 = gimple_call_fntype (s1);
  tree fntype2 = gimple_call_fntype (s2);
  if ((fntype1 && !fntype2)
      || (!fntype1 && fntype2)
      || (fntype1 && !types_compatible_p (fntype1, fntype2)))
    return return_false_with_msg ("call function types are not compatible");

  if (fntype1 && fntype2 && comp_type_attributes (fntype1, fntype2) != 1)
    return return_false_with_msg ("different fntype attributes");

  tree chain1 = gimple_call_chain (s1);
  tree chain2 = gimple_call_chain (s2);
  if ((chain1 && !chain2)
      || (!chain1 && chain2)
      || !compare_operand (chain1, chain2))
    return return_false_with_msg ("static call chains are different");

  /* Checking of argument.  */
  for (i = 0; i < gimple_call_num_args (s1); ++i)
    {
      t1 = gimple_call_arg (s1, i);
      t2 = gimple_call_arg (s2, i);

      if (!compare_operand (t1, t2))
	return return_false_with_msg ("GIMPLE call operands are different");
    }

  /* Return value checking.  */
  t1 = gimple_get_lhs (s1);
  t2 = gimple_get_lhs (s2);

  /* For internal calls, lhs types need to be verified, as neither fntype nor
     callee comparisons can catch that.  */
  if (gimple_call_internal_p (s1)
      && t1
      && t2
      && !compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return return_false_with_msg ("GIMPLE internal call LHS type mismatch");

  return compare_operand (t1, t2);
}


/* Verifies for given GIMPLEs S1 and S2 that
   assignment statements are semantically equivalent.  */

bool
func_checker::compare_gimple_assign (gimple *s1, gimple *s2)
{
  tree arg1, arg2;
  tree_code code1, code2;
  unsigned i;

  code1 = gimple_expr_code (s1);
  code2 = gimple_expr_code (s2);

  if (code1 != code2)
    return false;

  code1 = gimple_assign_rhs_code (s1);
  code2 = gimple_assign_rhs_code (s2);

  if (code1 != code2)
    return false;

  for (i = 0; i < gimple_num_ops (s1); i++)
    {
      arg1 = gimple_op (s1, i);
      arg2 = gimple_op (s2, i);

      /* Compare types for LHS.  */
      if (i == 0)
	{
	  if (!compatible_types_p (TREE_TYPE (arg1), TREE_TYPE (arg2)))
	    return return_false_with_msg ("GIMPLE NOP LHS type mismatch");
	}

      if (!compare_operand (arg1, arg2))
	return return_false_with_msg ("GIMPLE assignment operands "
				      "are different");
    }


  return true;
}

/* Verifies for given GIMPLEs S1 and S2 that
   condition statements are semantically equivalent.  */

bool
func_checker::compare_gimple_cond (gimple *s1, gimple *s2)
{
  tree t1, t2;
  tree_code code1, code2;

  code1 = gimple_expr_code (s1);
  code2 = gimple_expr_code (s2);

  if (code1 != code2)
    return false;

  t1 = gimple_cond_lhs (s1);
  t2 = gimple_cond_lhs (s2);

  if (!compare_operand (t1, t2))
    return false;

  t1 = gimple_cond_rhs (s1);
  t2 = gimple_cond_rhs (s2);

  return compare_operand (t1, t2);
}

/* Verifies for given GIMPLE_LABEL stmts S1 and S2 that
   label statements are semantically equivalent.  */

bool
func_checker::compare_gimple_label (const glabel *g1, const glabel *g2)
{
  if (m_ignore_labels)
    return true;

  tree t1 = gimple_label_label (g1);
  tree t2 = gimple_label_label (g2);

  if (FORCED_LABEL (t1) || FORCED_LABEL (t2))
    return return_false_with_msg ("FORCED_LABEL");

  /* As the pass build BB to label mapping, no further check is needed.  */
  return true;
}

/* Verifies for given GIMPLE_SWITCH stmts S1 and S2 that
   switch statements are semantically equivalent.  */

bool
func_checker::compare_gimple_switch (const gswitch *g1, const gswitch *g2)
{
  unsigned lsize1, lsize2, i;

  lsize1 = gimple_switch_num_labels (g1);
  lsize2 = gimple_switch_num_labels (g2);

  if (lsize1 != lsize2)
    return false;

  tree t1 = gimple_switch_index (g1);
  tree t2 = gimple_switch_index (g2);

  if (!compare_operand (t1, t2))
    return false;

  for (i = 0; i < lsize1; i++)
    {
      tree label1 = gimple_switch_label (g1, i);
      tree label2 = gimple_switch_label (g2, i);

      /* Label LOW and HIGH comparison.  */
      tree low1 = CASE_LOW (label1);
      tree low2 = CASE_LOW (label2);

      if (!tree_int_cst_equal (low1, low2))
	return return_false_with_msg ("case low values are different");

      tree high1 = CASE_HIGH (label1);
      tree high2 = CASE_HIGH (label2);

      if (!tree_int_cst_equal (high1, high2))
	return return_false_with_msg ("case high values are different");

      if (TREE_CODE (label1) == CASE_LABEL_EXPR
	  && TREE_CODE (label2) == CASE_LABEL_EXPR)
	{
	  label1 = CASE_LABEL (label1);
	  label2 = CASE_LABEL (label2);

	  if (!compare_operand (label1, label2))
	    return return_false_with_msg ("switch label_exprs are different");
	}
      else if (!tree_int_cst_equal (label1, label2))
	return return_false_with_msg ("switch labels are different");
    }

  return true;
}

/* Verifies for given GIMPLE_RETURN stmts S1 and S2 that
   return statements are semantically equivalent.  */

bool
func_checker::compare_gimple_return (const greturn *g1, const greturn *g2)
{
  tree t1, t2;

  t1 = gimple_return_retval (g1);
  t2 = gimple_return_retval (g2);

  /* Void return type.  */
  if (t1 == NULL && t2 == NULL)
    return true;
  else
    return compare_operand (t1, t2);
}

/* Verifies for given GIMPLEs S1 and S2 that
   goto statements are semantically equivalent.  */

bool
func_checker::compare_gimple_goto (gimple *g1, gimple *g2)
{
  tree dest1, dest2;

  dest1 = gimple_goto_dest (g1);
  dest2 = gimple_goto_dest (g2);

  if (TREE_CODE (dest1) != TREE_CODE (dest2) || TREE_CODE (dest1) != SSA_NAME)
    return false;

  return compare_operand (dest1, dest2);
}

/* Verifies for given GIMPLE_RESX stmts S1 and S2 that
   resx statements are semantically equivalent.  */

bool
func_checker::compare_gimple_resx (const gresx *g1, const gresx *g2)
{
  return gimple_resx_region (g1) == gimple_resx_region (g2);
}

/* Verifies for given GIMPLEs S1 and S2 that ASM statements are equivalent.
   For the beginning, the pass only supports equality for
   '__asm__ __volatile__ ("", "", "", "memory")'.  */

bool
func_checker::compare_gimple_asm (const gasm *g1, const gasm *g2)
{
  if (gimple_asm_volatile_p (g1) != gimple_asm_volatile_p (g2))
    return false;

  if (gimple_asm_input_p (g1) != gimple_asm_input_p (g2))
    return false;

  if (gimple_asm_inline_p (g1) != gimple_asm_inline_p (g2))
    return false;

  if (gimple_asm_ninputs (g1) != gimple_asm_ninputs (g2))
    return false;

  if (gimple_asm_noutputs (g1) != gimple_asm_noutputs (g2))
    return false;

  /* We do not suppport goto ASM statement comparison.  */
  if (gimple_asm_nlabels (g1) || gimple_asm_nlabels (g2))
    return false;

  if (gimple_asm_nclobbers (g1) != gimple_asm_nclobbers (g2))
    return false;

  if (strcmp (gimple_asm_string (g1), gimple_asm_string (g2)) != 0)
    return return_false_with_msg ("ASM strings are different");

  for (unsigned i = 0; i < gimple_asm_ninputs (g1); i++)
    {
      tree input1 = gimple_asm_input_op (g1, i);
      tree input2 = gimple_asm_input_op (g2, i);

      if (!compare_asm_inputs_outputs (input1, input2))
	return return_false_with_msg ("ASM input is different");
    }

  for (unsigned i = 0; i < gimple_asm_noutputs (g1); i++)
    {
      tree output1 = gimple_asm_output_op (g1, i);
      tree output2 = gimple_asm_output_op (g2, i);

      if (!compare_asm_inputs_outputs (output1, output2))
	return return_false_with_msg ("ASM output is different");
    }

  for (unsigned i = 0; i < gimple_asm_nclobbers (g1); i++)
    {
      tree clobber1 = gimple_asm_clobber_op (g1, i);
      tree clobber2 = gimple_asm_clobber_op (g2, i);

      if (!operand_equal_p (TREE_VALUE (clobber1), TREE_VALUE (clobber2),
			    OEP_ONLY_CONST))
	return return_false_with_msg ("ASM clobber is different");
    }

  return true;
}

} // ipa_icf_gimple namespace
