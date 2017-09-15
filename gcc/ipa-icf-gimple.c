/* Interprocedural Identical Code Folding pass
   Copyright (C) 2014-2015 Free Software Foundation, Inc.

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
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "hashtab.h"
#include "rtl.h"
#include "flags.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
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
#include "builtins.h"

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

      if (b1 == NULL && b2 == NULL)
	return true;

      if (b1 == NULL || b2 == NULL || TREE_CODE (b1) != TREE_CODE (b2))
	return return_false ();

      return compare_cst_or_decl (b1, b2);
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
func_checker::compare_decl (tree t1, tree t2)
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

  /* TODO: we are actually too strict here.  We only need to compare if
     T1 can be used in polymorphic call.  */
  if (TREE_ADDRESSABLE (t1)
      && m_compare_polymorphic
      && !compatible_polymorphic_types_p (TREE_TYPE (t1), TREE_TYPE (t2),
					  false))
    return return_false ();

  if ((t == VAR_DECL || t == PARM_DECL || t == RESULT_DECL)
      && DECL_BY_REFERENCE (t1)
      && m_compare_polymorphic
      && !compatible_polymorphic_types_p (TREE_TYPE (t1), TREE_TYPE (t2),
					  true))
    return return_false ();

  bool existed_p;

  tree &slot = m_decl_map.get_or_insert (t1, &existed_p);
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

  if (get_alias_set (t1) != get_alias_set (t2))
    return return_false_with_msg ("alias sets are different");

  return true;
}

/* Function compare for equality given memory operands T1 and T2.  */

bool
func_checker::compare_memory_operand (tree t1, tree t2)
{
  if (!t1 && !t2)
    return true;
  else if (!t1 || !t2)
    return false;

  ao_ref r1, r2;
  ao_ref_init (&r1, t1);
  ao_ref_init (&r2, t2);

  tree b1 = ao_ref_base (&r1);
  tree b2 = ao_ref_base (&r2);

  bool source_is_memop = DECL_P (b1) || INDIRECT_REF_P (b1)
			 || TREE_CODE (b1) == MEM_REF
			 || TREE_CODE (b1) == TARGET_MEM_REF;

  bool target_is_memop = DECL_P (b2) || INDIRECT_REF_P (b2)
			 || TREE_CODE (b2) == MEM_REF
			 || TREE_CODE (b2) == TARGET_MEM_REF;

  /* Compare alias sets for memory operands.  */
  if (source_is_memop && target_is_memop)
    {
      if (TREE_THIS_VOLATILE (t1) != TREE_THIS_VOLATILE (t2))
	return return_false_with_msg ("different operand volatility");

      if (ao_ref_alias_set (&r1) != ao_ref_alias_set (&r2)
	  || ao_ref_base_alias_set (&r1) != ao_ref_base_alias_set (&r2))
	return return_false_with_msg ("ao alias sets are different");

      /* We can't simply use get_object_alignment_1 on the full
         reference as for accesses with variable indexes this reports
	 too conservative alignment.  We also can't use the ao_ref_base
	 base objects as ao_ref_base happily strips MEM_REFs around
	 decls even though that may carry alignment info.  */
      b1 = t1;
      while (handled_component_p (b1))
	b1 = TREE_OPERAND (b1, 0);
      b2 = t2;
      while (handled_component_p (b2))
	b2 = TREE_OPERAND (b2, 0);
      unsigned int align1, align2;
      unsigned HOST_WIDE_INT tem;
      get_object_alignment_1 (b1, &align1, &tem);
      get_object_alignment_1 (b2, &align2, &tem);
      if (align1 != align2)
	return return_false_with_msg ("different access alignment");

      /* Similarly we have to compare dependence info where equality
         tells us we are safe (even some unequal values would be safe
	 but then we have to maintain a map of bases and cliques).  */
      unsigned short clique1 = 0, base1 = 0, clique2 = 0, base2 = 0;
      if (TREE_CODE (b1) == MEM_REF)
	{
	  clique1 = MR_DEPENDENCE_CLIQUE (b1);
	  base1 = MR_DEPENDENCE_BASE (b1);
	}
      if (TREE_CODE (b2) == MEM_REF)
	{
	  clique2 = MR_DEPENDENCE_CLIQUE (b2);
	  base2 = MR_DEPENDENCE_BASE (b2);
	}
      if (clique1 != clique2 || base1 != base2)
	return return_false_with_msg ("different dependence info");
    }

  return compare_operand (t1, t2);
}

/* Function compare for equality given trees T1 and T2 which
   can be either a constant or a declaration type.  */

bool
func_checker::compare_cst_or_decl (tree t1, tree t2)
{
  bool ret;

  switch (TREE_CODE (t1))
    {
    case INTEGER_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
    case REAL_CST:
      {
	ret = compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2))
	      && operand_equal_p (t1, t2, OEP_ONLY_CONST);
	return return_with_debug (ret);
      }
    case FUNCTION_DECL:
      /* All function decls are in the symbol table and known to match
	 before we start comparing bodies.  */
      return true;
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
	if (t1 == t2)
	  return true;

	int *bb1 = m_label_bb_map.get (t1);
	int *bb2 = m_label_bb_map.get (t2);

	/* Labels can point to another function (non-local GOTOs).  */
	return return_with_debug (bb1 != NULL && bb2 != NULL && *bb1 == *bb2);
      }
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
      {
	ret = compare_decl (t1, t2);
	return return_with_debug (ret);
      }
    default:
      gcc_unreachable ();
    }
}

/* Function responsible for comparison of various operands T1 and T2.
   If these components, from functions FUNC1 and FUNC2, are equal, true
   is returned.  */

bool
func_checker::compare_operand (tree t1, tree t2)
{
  tree x1, x2, y1, y2, z1, z2;
  bool ret;

  if (!t1 && !t2)
    return true;
  else if (!t1 || !t2)
    return false;

  tree tt1 = TREE_TYPE (t1);
  tree tt2 = TREE_TYPE (t2);

  if (!func_checker::compatible_types_p (tt1, tt2))
    return false;

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
      /* First argument is the array, second is the index.  */
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
	      && compare_cst_or_decl (y1, y2);

	return return_with_debug (ret);
      }
    /* Virtual table call.  */
    case OBJ_TYPE_REF:
      {
	if (!compare_ssa_name (OBJ_TYPE_REF_EXPR (t1), OBJ_TYPE_REF_EXPR (t2)))
	  return return_false ();
	if (opt_for_fn (m_source_func_decl, flag_devirtualize)
	    && virtual_method_call_p (t1))
	  {
	    if (tree_to_uhwi (OBJ_TYPE_REF_TOKEN (t1))
		!= tree_to_uhwi (OBJ_TYPE_REF_TOKEN (t2)))
	      return return_false_with_msg ("OBJ_TYPE_REF token mismatch");
	    if (!types_same_for_odr (obj_type_ref_class (t1),
				     obj_type_ref_class (t2)))
	      return return_false_with_msg ("OBJ_TYPE_REF OTR type mismatch");
	    if (!compare_operand (OBJ_TYPE_REF_OBJECT (t1),
				  OBJ_TYPE_REF_OBJECT (t2)))
	      return return_false_with_msg ("OBJ_TYPE_REF object mismatch");
	  }

	return return_with_debug (true);
      }
    case IMAGPART_EXPR:
    case REALPART_EXPR:
    case ADDR_EXPR:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);

	ret = compare_operand (x1, x2);
	return return_with_debug (ret);
      }
    case BIT_FIELD_REF:
      {
	x1 = TREE_OPERAND (t1, 0);
	x2 = TREE_OPERAND (t2, 0);
	y1 = TREE_OPERAND (t1, 1);
	y2 = TREE_OPERAND (t2, 1);
	z1 = TREE_OPERAND (t1, 2);
	z2 = TREE_OPERAND (t2, 2);

	ret = compare_operand (x1, x2)
	      && compare_cst_or_decl (y1, y2)
	      && compare_cst_or_decl (z1, z2);

	return return_with_debug (ret);
      }
    case SSA_NAME:
	return compare_ssa_name (t1, t2);
    case INTEGER_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
    case REAL_CST:
    case FUNCTION_DECL:
    case VAR_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
      return compare_cst_or_decl (t1, t2);
    default:
      return return_false_with_msg ("Unknown TREE code reached");
    }
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
func_checker::compare_variable_decl (tree t1, tree t2)
{
  bool ret = false;

  if (t1 == t2)
    return true;

  if (DECL_ALIGN (t1) != DECL_ALIGN (t2))
    return return_false_with_msg ("alignments are different");

  if (DECL_HARD_REGISTER (t1) != DECL_HARD_REGISTER (t2))
    return return_false_with_msg ("DECL_HARD_REGISTER are different");

  if (DECL_HARD_REGISTER (t1)
      && DECL_ASSEMBLER_NAME (t1) != DECL_ASSEMBLER_NAME (t2))
    return return_false_with_msg ("HARD REGISTERS are different");

  /* Symbol table variables are known to match before we start comparing
     bodies.  */
  if (decl_in_symtab_p (t1))
    return decl_in_symtab_p (t2);
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
  gimple_stmt_iterator gsi1, gsi2;
  gimple s1, s2;

  gsi1 = gsi_start_bb_nondebug (bb1->bb);
  gsi2 = gsi_start_bb_nondebug (bb2->bb);

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

      if (!compare_memory_operand (t1, t2))
	return return_false_with_msg ("memory operands are different");
    }

  /* Return value checking.  */
  t1 = gimple_get_lhs (s1);
  t2 = gimple_get_lhs (s2);

  return compare_memory_operand (t1, t2);
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

      if (!compare_memory_operand (arg1, arg2))
	return return_false_with_msg ("memory operands are different");
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
