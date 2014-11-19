/* Interprocedural Identical Code Folding pass
   Copyright (C) 2014 Free Software Foundation, Inc.

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
#include "tree.h"
#include "predict.h"
#include "vec.h"
#include "hashtab.h"
#include "hash-set.h"
#include "machmode.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "expr.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "stringpool.h"
#include "tree-dfa.h"
#include "tree-pass.h"
#include "gimple-pretty-print.h"
#include "cfgloop.h"
#include "except.h"
#include "hash-map.h"
#include "plugin-api.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "data-streamer.h"
#include "ipa-utils.h"
#include <list>
#include "tree-ssanames.h"
#include "tree-eh.h"

#include "ipa-icf-gimple.h"
#include "ipa-icf.h"

namespace ipa_icf_gimple {

/* Initialize internal structures for a given SOURCE_FUNC_DECL and
   TARGET_FUNC_DECL. Strict polymorphic comparison is processed if
   an option COMPARE_POLYMORPHIC is true. For special cases, one can
   set IGNORE_LABELS to skip label comparison.
   Similarly, IGNORE_SOURCE_DECLS and IGNORE_TARGET_DECLS are sets
   of declarations that can be skipped.  */

func_checker::func_checker (tree source_func_decl, tree target_func_decl,
			    bool compare_polymorphic,
			    bool ignore_labels,
			    hash_set<symtab_node *> *ignored_source_nodes,
			    hash_set<symtab_node *> *ignored_target_nodes)
  : m_source_func_decl (source_func_decl), m_target_func_decl (target_func_decl),
    m_ignored_source_nodes (ignored_source_nodes),
    m_ignored_target_nodes (ignored_target_nodes),
    m_compare_polymorphic (compare_polymorphic),
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
func_checker::compare_ssa_name (tree t1, tree t2)
{
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
func_checker::compare_decl (tree t1, tree t2)
{
  if (!auto_var_in_fn_p (t1, m_source_func_decl)
      || !auto_var_in_fn_p (t2, m_target_func_decl))
    return return_with_debug (t1 == t2);

  tree_code t = TREE_CODE (t1);
  if ((t == VAR_DECL || t == PARM_DECL || t == RESULT_DECL)
      && DECL_BY_REFERENCE (t1) != DECL_BY_REFERENCE (t2))
    return return_false_with_msg ("DECL_BY_REFERENCE flags are different");

  if (!compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2),
			   m_compare_polymorphic))
    return return_false ();

  bool existed_p;

  tree &slot = m_decl_map.get_or_insert (t1, &existed_p);
  if (existed_p)
    return return_with_debug (slot == t2);
  else
    slot = t2;

  return true;
}

/* Return true if types are compatible from perspective of ICF.  */
bool func_checker::compatible_types_p (tree t1, tree t2,
				       bool compare_polymorphic,
				       bool first_argument)
{
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return return_false_with_msg ("different tree types");

  if (!types_compatible_p (t1, t2))
    return return_false_with_msg ("types are not compatible");

  if (get_alias_set (t1) != get_alias_set (t2))
    return return_false_with_msg ("alias sets are different");

  /* We call contains_polymorphic_type_p with this pointer type.  */
  if (first_argument && TREE_CODE (t1) == POINTER_TYPE)
    {
      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
    }

  if (compare_polymorphic)
    if (contains_polymorphic_type_p (t1) || contains_polymorphic_type_p (t2))
      {
	if (!contains_polymorphic_type_p (t1) || !contains_polymorphic_type_p (t2))
	  return return_false_with_msg ("one type is not polymorphic");

	if (!types_must_be_same_for_odr (t1, t2))
	  return return_false_with_msg ("types are not same for ODR");
      }

  return true;
}

/* Function responsible for comparison of handled components T1 and T2.
   If these components, from functions FUNC1 and FUNC2, are equal, true
   is returned.  */

bool
func_checker::compare_operand (tree t1, tree t2)
{
  tree base1, base2, x1, x2, y1, y2, z1, z2;
  HOST_WIDE_INT offset1 = 0, offset2 = 0;
  bool ret;

  if (!t1 && !t2)
    return true;
  else if (!t1 || !t2)
    return false;

  tree tt1 = TREE_TYPE (t1);
  tree tt2 = TREE_TYPE (t2);

  if (!func_checker::compatible_types_p (tt1, tt2))
    return false;

  base1 = get_addr_base_and_unit_offset (t1, &offset1);
  base2 = get_addr_base_and_unit_offset (t2, &offset2);

  if (base1 && base2)
    {
      if (offset1 != offset2)
	return return_false_with_msg ("base offsets are different");

      t1 = base1;
      t2 = base2;
    }

  if (TREE_CODE (t1) != TREE_CODE (t2))
    return return_false ();

  switch (TREE_CODE (t1))
    {
    case CONSTRUCTOR:
      {
	unsigned length1 = vec_safe_length (CONSTRUCTOR_ELTS (t1));
	unsigned length2 = vec_safe_length (CONSTRUCTOR_ELTS (t2));

	if (length1 != length2)
	  return return_false ();

	for (unsigned i = 0; i < length1; i++)
	  if (!compare_operand (CONSTRUCTOR_ELT (t1, i)->value,
				CONSTRUCTOR_ELT (t2, i)->value))
	    return return_false();

	return true;
      }
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      x1 = TREE_OPERAND (t1, 0);
      x2 = TREE_OPERAND (t2, 0);
      y1 = TREE_OPERAND (t1, 1);
      y2 = TREE_OPERAND (t2, 1);

      if (!compare_operand (array_ref_low_bound (t1),
			    array_ref_low_bound (t2)))
	return return_false_with_msg ("");
      if (!compare_operand (array_ref_element_size (t1),
			    array_ref_element_size (t2)))
	return return_false_with_msg ("");
      if (!compare_operand (x1, x2))
	return return_false_with_msg ("");
      return compare_operand (y1, y2);
    case MEM_REF:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);
	y1 = TREE_OPERAND (t1, 1);
	y2 = TREE_OPERAND (t2, 1);

	/* See if operand is an memory access (the test originate from
	 gimple_load_p).

	In this case the alias set of the function being replaced must
	be subset of the alias set of the other function.  At the moment
	we seek for equivalency classes, so simply require inclussion in
	both directions.  */

	if (!func_checker::compatible_types_p (TREE_TYPE (x1), TREE_TYPE (x2)))
	  return return_false ();

	if (!compare_operand (x1, x2))
	  return return_false_with_msg ("");

	if (get_alias_set (TREE_TYPE (y1)) != get_alias_set (TREE_TYPE (y2)))
	  return return_false_with_msg ("alias set for MEM_REF offsets are different");

	ao_ref r1, r2;
	ao_ref_init (&r1, t1);
	ao_ref_init (&r2, t2);
	if (ao_ref_alias_set (&r1) != ao_ref_alias_set (&r2)
	    || ao_ref_base_alias_set (&r1) != ao_ref_base_alias_set (&r2))
	  return return_false_with_msg ("ao alias sets are different");

	/* Type of the offset on MEM_REF does not matter.  */
	return wi::to_offset  (y1) == wi::to_offset  (y2);
      }
    case COMPONENT_REF:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);
	y1 = TREE_OPERAND (t1, 1);
	y2 = TREE_OPERAND (t2, 1);

	ret = compare_operand (x1, x2)
	      && compare_operand (y1, y2);

	return return_with_debug (ret);
      }
    /* Virtual table call.  */
    case OBJ_TYPE_REF:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);
	y1 = TREE_OPERAND (t1, 1);
	y2 = TREE_OPERAND (t2, 1);
	z1 = TREE_OPERAND (t1, 2);
	z2 = TREE_OPERAND (t2, 2);

	ret = compare_operand (x1, x2)
	      && compare_operand (y1, y2)
	      && compare_operand (z1, z2);

	return return_with_debug (ret);
      }
    case ADDR_EXPR:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);

	ret = compare_operand (x1, x2);
	return return_with_debug (ret);
      }
    case SSA_NAME:
      {
	ret = compare_ssa_name (t1, t2);

	if (!ret)
	  return return_with_debug (ret);

	if (SSA_NAME_IS_DEFAULT_DEF (t1))
	  {
	    tree b1 = SSA_NAME_VAR (t1);
	    tree b2 = SSA_NAME_VAR (t2);

	    if (b1 == NULL && b2 == NULL)
	      return true;

	    if (b1 == NULL || b2 == NULL || TREE_CODE (b1) != TREE_CODE (b2))
	      return return_false ();

	    switch (TREE_CODE (b1))
	      {
	      case VAR_DECL:
		return return_with_debug (compare_variable_decl (t1, t2));
	      case PARM_DECL:
	      case RESULT_DECL:
		ret = compare_decl (b1, b2);
		return return_with_debug (ret);
	      default:
		return return_false_with_msg ("Unknown TREE code reached");
	      }
	  }
	else
	  return true;
      }
    case INTEGER_CST:
      {
	ret = compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2))
	      && wi::to_offset  (t1) == wi::to_offset  (t2);

	return return_with_debug (ret);
      }
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
    case REAL_CST:
      {
	ret = operand_equal_p (t1, t2, OEP_ONLY_CONST);
	return return_with_debug (ret);
      }
    case FUNCTION_DECL:
      {
	ret = compare_function_decl (t1, t2);
	return return_with_debug (ret);
      }
    case VAR_DECL:
      return return_with_debug (compare_variable_decl (t1, t2));
    case FIELD_DECL:
      {
	tree offset1 = DECL_FIELD_OFFSET (t1);
	tree offset2 = DECL_FIELD_OFFSET (t2);

	tree bit_offset1 = DECL_FIELD_BIT_OFFSET (t1);
	tree bit_offset2 = DECL_FIELD_BIT_OFFSET (t2);

	ret = compare_operand (offset1, offset2)
	      && compare_operand (bit_offset1, bit_offset2);

	return return_with_debug (ret);
      }
    case LABEL_DECL:
      {
	int *bb1 = m_label_bb_map.get (t1);
	int *bb2 = m_label_bb_map.get (t2);

	return return_with_debug (*bb1 == *bb2);
      }
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
    case BIT_FIELD_REF:
      {
	ret = compare_decl (t1, t2);
	return return_with_debug (ret);
      }
    default:
      return return_false_with_msg ("Unknown TREE code reached");
    }
}

/* Compares two tree list operands T1 and T2 and returns true if these
   two trees are semantically equivalent.  */

bool
func_checker::compare_tree_list_operand (tree t1, tree t2)
{
  gcc_assert (TREE_CODE (t1) == TREE_LIST);
  gcc_assert (TREE_CODE (t2) == TREE_LIST);

  for (; t1; t1 = TREE_CHAIN (t1))
    {
      if (!t2)
	return false;

      if (!compare_operand (TREE_VALUE (t1), TREE_VALUE (t2)))
	return return_false ();

      t2 = TREE_CHAIN (t2);
    }

  if (t2)
    return return_false ();

  return true;
}

/* Verifies that trees T1 and T2, representing function declarations
   are equivalent from perspective of ICF.  */

bool
func_checker::compare_function_decl (tree t1, tree t2)
{
  bool ret = false;

  if (t1 == t2)
    return true;

  symtab_node *n1 = symtab_node::get (t1);
  symtab_node *n2 = symtab_node::get (t2);

  if (m_ignored_source_nodes != NULL && m_ignored_target_nodes != NULL)
    {
      ret = m_ignored_source_nodes->contains (n1)
	    && m_ignored_target_nodes->contains (n2);

      if (ret)
	return true;
    }

  /* If function decl is WEAKREF, we compare targets.  */
  cgraph_node *f1 = cgraph_node::get (t1);
  cgraph_node *f2 = cgraph_node::get (t2);

  if(f1 && f2 && f1->weakref && f2->weakref)
    ret = f1->alias_target == f2->alias_target;

  return ret;
}

/* Verifies that trees T1 and T2 do correspond.  */

bool
func_checker::compare_variable_decl (tree t1, tree t2)
{
  bool ret = false;

  if (t1 == t2)
    return true;

  if (TREE_CODE (t1) == VAR_DECL && (DECL_EXTERNAL (t1) || TREE_STATIC (t1)))
    {
      symtab_node *n1 = symtab_node::get (t1);
      symtab_node *n2 = symtab_node::get (t2);

      if (m_ignored_source_nodes != NULL && m_ignored_target_nodes != NULL)
	{
	  ret = m_ignored_source_nodes->contains (n1)
		&& m_ignored_target_nodes->contains (n2);

	  if (ret)
	    return true;
	}
    }
  ret = compare_decl (t1, t2);

  return return_with_debug (ret);
}


/* Function visits all gimple labels and creates corresponding
   mapping between basic blocks and labels.  */

void
func_checker::parse_labels (sem_bb *bb)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb->bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	{
	  tree t = gimple_label_label (label_stmt);
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
  unsigned i;
  gimple_stmt_iterator gsi1, gsi2;
  gimple s1, s2;

  if (bb1->nondbg_stmt_count != bb2->nondbg_stmt_count
      || bb1->edge_count != bb2->edge_count)
    return return_false ();

  gsi1 = gsi_start_bb (bb1->bb);
  gsi2 = gsi_start_bb (bb2->bb);

  for (i = 0; i < bb1->nondbg_stmt_count; i++)
    {
      if (is_gimple_debug (gsi_stmt (gsi1)))
	gsi_next_nondebug (&gsi1);

      if (is_gimple_debug (gsi_stmt (gsi2)))
	gsi_next_nondebug (&gsi2);

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
	case GIMPLE_EH_DISPATCH:
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
	  return true;
	default:
	  return return_false_with_msg ("Unknown GIMPLE code reached");
	}

      gsi_next (&gsi1);
      gsi_next (&gsi2);
    }

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
      || gimple_call_alloca_for_var_p (s1) != gimple_call_alloca_for_var_p (s2)
      || gimple_call_with_bounds_p (s1) != gimple_call_with_bounds_p (s2))
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
	return false;
    }

  /* Return value checking.  */
  t1 = gimple_get_lhs (s1);
  t2 = gimple_get_lhs (s2);

  return compare_operand (t1, t2);
}


/* Verifies for given GIMPLEs S1 and S2 that
   assignment statements are semantically equivalent.  */

bool
func_checker::compare_gimple_assign (gimple s1, gimple s2)
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

      if (!compare_operand (arg1, arg2))
	return false;
    }


  return true;
}

/* Verifies for given GIMPLEs S1 and S2 that
   condition statements are semantically equivalent.  */

bool
func_checker::compare_gimple_cond (gimple s1, gimple s2)
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

/* Verifies that tree labels T1 and T2 correspond in FUNC1 and FUNC2.  */

bool
func_checker::compare_tree_ssa_label (tree t1, tree t2)
{
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
func_checker::compare_gimple_goto (gimple g1, gimple g2)
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

      if (!compare_tree_list_operand (input1, input2))
	return return_false_with_msg ("ASM input is different");
    }

  for (unsigned i = 0; i < gimple_asm_noutputs (g1); i++)
    {
      tree output1 = gimple_asm_output_op (g1, i);
      tree output2 = gimple_asm_output_op (g2, i);

      if (!compare_tree_list_operand (output1, output2))
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
