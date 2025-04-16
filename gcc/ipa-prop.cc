/* Interprocedural analyses.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.

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
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "calls.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-inline.h"
#include "ipa-fnsummary.h"
#include "gimple-pretty-print.h"
#include "ipa-utils.h"
#include "dbgcnt.h"
#include "domwalk.h"
#include "builtins.h"
#include "tree-cfgcleanup.h"
#include "options.h"
#include "symtab-clones.h"
#include "attr-fnspec.h"
#include "gimple-range.h"
#include "value-range-storage.h"
#include "vr-values.h"
#include "lto-streamer.h"

/* Function summary where the parameter infos are actually stored. */
ipa_node_params_t *ipa_node_params_sum = NULL;

function_summary <ipcp_transformation *> *ipcp_transformation_sum = NULL;

/* Edge summary for IPA-CP edge information.  */
ipa_edge_args_sum_t *ipa_edge_args_sum;

/* Traits for a hash table for reusing ranges.  */

struct ipa_vr_ggc_hash_traits : public ggc_cache_remove <ipa_vr *>
{
  typedef ipa_vr *value_type;
  typedef const vrange *compare_type;
  static hashval_t
  hash (const ipa_vr *p)
    {
      // This never get called, except in the verification code, as
      // ipa_get_value_range() calculates the hash itself.  This
      // function is mostly here for completness' sake.
      value_range vr;
      p->get_vrange (vr);
      inchash::hash hstate;
      add_vrange (vr, hstate);
      return hstate.end ();
    }
  static bool
  equal (const ipa_vr *a, const vrange *b)
    {
      return a->equal_p (*b);
    }
  static const bool empty_zero_p = true;
  static void
  mark_empty (ipa_vr *&p)
    {
      p = NULL;
    }
  static bool
  is_empty (const ipa_vr *p)
    {
      return p == NULL;
    }
  static bool
  is_deleted (const ipa_vr *p)
    {
      return p == reinterpret_cast<const ipa_vr *> (1);
    }
  static void
  mark_deleted (ipa_vr *&p)
    {
      p = reinterpret_cast<ipa_vr *> (1);
    }
};

/* Hash table for avoid repeated allocations of equal ranges.  */
static GTY ((cache)) hash_table<ipa_vr_ggc_hash_traits> *ipa_vr_hash_table;

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;

/* Description of a reference to an IPA constant.  */
struct ipa_cst_ref_desc
{
  /* Edge that corresponds to the statement which took the reference.  */
  struct cgraph_edge *cs;
  /* Linked list of duplicates created when call graph edges are cloned.  */
  struct ipa_cst_ref_desc *next_duplicate;
  /* Number of references in IPA structures, IPA_UNDESCRIBED_USE if the value
     if out of control.  */
  int refcount;
};

/* Allocation pool for reference descriptions.  */

static object_allocator<ipa_cst_ref_desc> ipa_refdesc_pool
  ("IPA-PROP ref descriptions");

ipa_vr::ipa_vr ()
  : m_storage (NULL),
    m_type (NULL)
{
}

ipa_vr::ipa_vr (const vrange &r)
  : m_storage (ggc_alloc_vrange_storage (r)),
    m_type (r.type ())
{
}

bool
ipa_vr::equal_p (const vrange &r) const
{
  gcc_checking_assert (!r.undefined_p ());
  return (types_compatible_p (m_type, r.type ()) && m_storage->equal_p (r));
}

bool
ipa_vr::equal_p (const ipa_vr &o) const
{
  if (!known_p ())
    return !o.known_p ();

  if (!types_compatible_p (m_type, o.m_type))
    return false;

  value_range r;
  o.get_vrange (r);
  return m_storage->equal_p (r);
}

void
ipa_vr::get_vrange (value_range &r) const
{
  r.set_type (m_type);
  m_storage->get_vrange (r, m_type);
}

void
ipa_vr::set_unknown ()
{
  if (m_storage)
    ggc_free (m_storage);

  m_storage = NULL;
}

void
ipa_vr::streamer_read (lto_input_block *ib, data_in *data_in)
{
  struct bitpack_d bp = streamer_read_bitpack (ib);
  bool known = bp_unpack_value (&bp, 1);
  if (known)
    {
      value_range vr;
      streamer_read_value_range (ib, data_in, vr);
      if (!m_storage || !m_storage->fits_p (vr))
	{
	  if (m_storage)
	    ggc_free (m_storage);
	  m_storage = ggc_alloc_vrange_storage (vr);
	}
      m_storage->set_vrange (vr);
      m_type = vr.type ();
    }
  else
    {
      m_storage = NULL;
      m_type = NULL;
    }
}

void
ipa_vr::streamer_write (output_block *ob) const
{
  struct bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, !!m_storage, 1);
  streamer_write_bitpack (&bp);
  if (m_storage)
    {
      value_range vr (m_type);
      m_storage->get_vrange (vr, m_type);
      streamer_write_vrange (ob, vr);
    }
}

void
ipa_vr::dump (FILE *out) const
{
  if (known_p ())
    {
      value_range vr (m_type);
      m_storage->get_vrange (vr, m_type);
      vr.dump (out);
    }
  else
    fprintf (out, "NO RANGE");
}

// These stubs are because we use an ipa_vr in a hash_traits and
// hash-traits.h defines an extern of gt_ggc_mx (T &) instead of
// picking up the gt_ggc_mx (T *) version.
void
gt_pch_nx (ipa_vr *&x)
{
  return gt_pch_nx ((ipa_vr *) x);
}

void
gt_ggc_mx (ipa_vr *&x)
{
  return gt_ggc_mx ((ipa_vr *) x);
}

/* Analysis summery of function call return value.  */
struct GTY(()) ipa_return_value_summary
{
  /* Known value range.
     This needs to be wrapped in struccture due to specific way
     we allocate ipa_vr. */
  ipa_vr *vr;
};

/* Function summary for return values.  */
class ipa_return_value_sum_t : public function_summary <ipa_return_value_summary *>
{
public:
  ipa_return_value_sum_t (symbol_table *table, bool ggc):
    function_summary <ipa_return_value_summary *> (table, ggc) { }

  /* Hook that is called by summary when a node is duplicated.  */
  void duplicate (cgraph_node *,
		  cgraph_node *,
		  ipa_return_value_summary *data,
		  ipa_return_value_summary *data2) final override
  {
    *data2=*data;
  }
};

/* Variable hoding the return value summary.  */
static GTY(()) function_summary <ipa_return_value_summary *> *ipa_return_value_sum;


/* Return true if DECL_FUNCTION_SPECIFIC_OPTIMIZATION of the decl associated
   with NODE should prevent us from analyzing it for the purposes of IPA-CP.  */

static bool
ipa_func_spec_opts_forbid_analysis_p (struct cgraph_node *node)
{
  tree fs_opts = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (node->decl);

  if (!fs_opts)
    return false;
  return !opt_for_fn (node->decl, optimize) || !opt_for_fn (node->decl, flag_ipa_cp);
}

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

static int
ipa_get_param_decl_index_1 (vec<ipa_param_descriptor, va_gc> *descriptors,
			    tree ptree)
{
  int i, count;

  count = vec_safe_length (descriptors);
  for (i = 0; i < count; i++)
    if ((*descriptors)[i].decl_or_type == ptree)
      return i;

  return -1;
}

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

int
ipa_get_param_decl_index (class ipa_node_params *info, tree ptree)
{
  return ipa_get_param_decl_index_1 (info->descriptors, ptree);
}

/* Populate the param_decl field in parameter DESCRIPTORS that correspond to
   NODE.  */

static void
ipa_populate_param_decls (struct cgraph_node *node,
			  vec<ipa_param_descriptor, va_gc> &descriptors)
{
  tree fndecl;
  tree fnargs;
  tree parm;
  int param_num;

  fndecl = node->decl;
  gcc_assert (gimple_has_body_p (fndecl));
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = DECL_CHAIN (parm))
    {
      descriptors[param_num].decl_or_type = parm;
      unsigned int cost = estimate_move_cost (TREE_TYPE (parm), true);
      descriptors[param_num].move_cost = cost;
      /* Watch overflow, move_cost is a bitfield.  */
      gcc_checking_assert (cost == descriptors[param_num].move_cost);
      param_num++;
    }
}

/* Return how many formal parameters FNDECL has.  */

int
count_formal_params (tree fndecl)
{
  tree parm;
  int count = 0;
  gcc_assert (gimple_has_body_p (fndecl));

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    count++;

  return count;
}

/* Return the declaration of Ith formal parameter of the function corresponding
   to INFO.  Note there is no setter function as this array is built just once
   using ipa_initialize_node_params. */

void
ipa_dump_param (FILE *file, class ipa_node_params *info, int i)
{
  fprintf (file, "param #%i", i);
  if ((*info->descriptors)[i].decl_or_type)
    {
      fprintf (file, " ");
      print_generic_expr (file, (*info->descriptors)[i].decl_or_type);
    }
}

/* If necessary, allocate vector of parameter descriptors in info of NODE.
   Return true if they were allocated, false if not.  */

static bool
ipa_alloc_node_params (struct cgraph_node *node, int param_count)
{
  ipa_node_params *info = ipa_node_params_sum->get_create (node);

  if (!info->descriptors && param_count)
    {
      vec_safe_grow_cleared (info->descriptors, param_count, true);
      return true;
    }
  else
    return false;
}

/* Initialize the ipa_node_params structure associated with NODE by counting
   the function parameters, creating the descriptors and populating their
   param_decls.  */

void
ipa_initialize_node_params (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get_create (node);

  if (!info->descriptors
      && ipa_alloc_node_params (node, count_formal_params (node->decl)))
    ipa_populate_param_decls (node, *info->descriptors);
}

/* Print VAL which is extracted from a jump function to F.  */

void
ipa_print_constant_value (FILE *f, tree val)
{
  print_generic_expr (f, val);

  /* This is in keeping with values_equal_for_ipcp_p.  */
  if (TREE_CODE (val) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (val, 0)) == CONST_DECL
	  || (TREE_CODE (TREE_OPERAND (val, 0)) == VAR_DECL
	      && DECL_IN_CONSTANT_POOL (TREE_OPERAND (val, 0)))))
    {
      fputs (" -> ", f);
      print_generic_expr (f, DECL_INITIAL (TREE_OPERAND (val, 0)));
    }
}

/* Print contents of JFUNC to F.  If CTX is non-NULL, dump it too.  */

DEBUG_FUNCTION void
ipa_dump_jump_function (FILE *f, ipa_jump_func *jump_func,
			class ipa_polymorphic_call_context *ctx)
{
  enum jump_func_type type = jump_func->type;

  if (type == IPA_JF_UNKNOWN)
    fprintf (f, "UNKNOWN\n");
  else if (type == IPA_JF_CONST)
    {
      fprintf (f, "CONST: ");
      ipa_print_constant_value (f, jump_func->value.constant.value);
      fprintf (f, "\n");
    }
  else if (type == IPA_JF_PASS_THROUGH)
    {
      fprintf (f, "PASS THROUGH: ");
      fprintf (f, "%d, op %s",
	       jump_func->value.pass_through.formal_id,
	       get_tree_code_name(jump_func->value.pass_through.operation));
      if (jump_func->value.pass_through.operation != NOP_EXPR)
	{
	  fprintf (f, " ");
	  if (jump_func->value.pass_through.operand)
	    print_generic_expr (f, jump_func->value.pass_through.operand);
	  fprintf (f, " (in type ");
	  print_generic_expr (f, jump_func->value.pass_through.op_type);
	  fprintf (f, ")");
	}
      if (jump_func->value.pass_through.agg_preserved)
	fprintf (f, ", agg_preserved");
      if (jump_func->value.pass_through.refdesc_decremented)
	fprintf (f, ", refdesc_decremented");
      fprintf (f, "\n");
    }
  else if (type == IPA_JF_ANCESTOR)
    {
      fprintf (f, "ANCESTOR: ");
      fprintf (f, "%d, offset " HOST_WIDE_INT_PRINT_DEC,
	       jump_func->value.ancestor.formal_id,
	       jump_func->value.ancestor.offset);
      if (jump_func->value.ancestor.agg_preserved)
	fprintf (f, ", agg_preserved");
      if (jump_func->value.ancestor.keep_null)
	fprintf (f, ", keep_null");
      fprintf (f, "\n");
    }

  if (jump_func->agg.items)
    {
      struct ipa_agg_jf_item *item;
      int j;

      fprintf (f, "         Aggregate passed by %s:\n",
	       jump_func->agg.by_ref ? "reference" : "value");
      FOR_EACH_VEC_ELT (*jump_func->agg.items, j, item)
	{
	  fprintf (f, "           offset: " HOST_WIDE_INT_PRINT_DEC ", ",
		   item->offset);
	  fprintf (f, "type: ");
	  print_generic_expr (f, item->type);
	  fprintf (f, ", ");
	  if (item->jftype == IPA_JF_PASS_THROUGH)
	    fprintf (f, "PASS THROUGH: %d,",
		     item->value.pass_through.formal_id);
	  else if (item->jftype == IPA_JF_LOAD_AGG)
	    {
	      fprintf (f, "LOAD AGG: %d",
		       item->value.pass_through.formal_id);
	      fprintf (f, " [offset: " HOST_WIDE_INT_PRINT_DEC ", by %s],",
		       item->value.load_agg.offset,
		       item->value.load_agg.by_ref ? "reference"
		       : "value");
	    }

	  if (item->jftype == IPA_JF_PASS_THROUGH
	      || item->jftype == IPA_JF_LOAD_AGG)
	    {
	      fprintf (f, " op %s",
		       get_tree_code_name (item->value.pass_through.operation));
	      if (item->value.pass_through.operation != NOP_EXPR)
		{
		  fprintf (f, " ");
		  if (item->value.pass_through.operand)
		    print_generic_expr (f, item->value.pass_through.operand);
		  fprintf (f, " (in type ");
		  print_generic_expr (f, jump_func->value.pass_through.op_type);
		  fprintf (f, ")");
		}
	    }
	  else if (item->jftype == IPA_JF_CONST)
	    {
	      fprintf (f, "CONST: ");
	      ipa_print_constant_value (f, item->value.constant);
	    }
	  else if (item->jftype == IPA_JF_UNKNOWN)
	    fprintf (f, "UNKNOWN: " HOST_WIDE_INT_PRINT_DEC " bits",
		     tree_to_uhwi (TYPE_SIZE (item->type)));
	  fprintf (f, "\n");
	}
    }

  if (ctx && !ctx->useless_p ())
    {
      fprintf (f, "         Context: ");
      ctx->dump (dump_file);
    }

  if (jump_func->m_vr)
    {
      jump_func->m_vr->dump (f);
      fprintf (f, "\n");
    }
  else
    fprintf (f, "         Unknown VR\n");
}

/* Print the jump functions associated with call graph edge CS to file F.  */

static void
ipa_print_node_jump_functions_for_edge (FILE *f, struct cgraph_edge *cs)
{
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  int count = ipa_get_cs_argument_count (args);

  for (int i = 0; i < count; i++)
    {
      struct ipa_jump_func *jump_func = ipa_get_ith_jump_func (args, i);
      class ipa_polymorphic_call_context *ctx
	= ipa_get_ith_polymorhic_call_context (args, i);

      fprintf (f, "       param %d: ", i);
      ipa_dump_jump_function (f, jump_func, ctx);
    }
}


/* Print the jump functions of all arguments on all call graph edges going from
   NODE to file F.  */

void
ipa_print_node_jump_functions (FILE *f, struct cgraph_node *node)
{
  struct cgraph_edge *cs;

  fprintf (f, "  Jump functions of caller  %s:\n", node->dump_name ());
  for (cs = node->callees; cs; cs = cs->next_callee)
    {

      fprintf (f, "    callsite  %s -> %s : \n",
	       node->dump_name (),
	       cs->callee->dump_name ());
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	fprintf (f, "       no arg info\n");
      else
        ipa_print_node_jump_functions_for_edge (f, cs);
    }

  for (cs = node->indirect_calls; cs; cs = cs->next_callee)
    {
      class cgraph_indirect_call_info *ii;

      ii = cs->indirect_info;
      if (ii->agg_contents)
	fprintf (f, "    indirect %s callsite, calling param %i, "
		 "offset " HOST_WIDE_INT_PRINT_DEC ", %s",
		 ii->member_ptr ? "member ptr" : "aggregate",
		 ii->param_index, ii->offset,
		 ii->by_ref ? "by reference" : "by_value");
      else
	fprintf (f, "    indirect %s callsite, calling param %i, "
		 "offset " HOST_WIDE_INT_PRINT_DEC,
		 ii->polymorphic ? "polymorphic" : "simple", ii->param_index,
		 ii->offset);

      if (cs->call_stmt)
	{
	  fprintf (f, ", for stmt ");
	  print_gimple_stmt (f, cs->call_stmt, 0, TDF_SLIM);
	}
      else
	fprintf (f, "\n");
      if (ii->polymorphic)
	ii->context.dump (f);
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	fprintf (f, "       no arg info\n");
      else
        ipa_print_node_jump_functions_for_edge (f, cs);
    }
}

/* Print ipa_jump_func data structures of all nodes in the call graph to F.  */

void
ipa_print_all_jump_functions (FILE *f)
{
  struct cgraph_node *node;

  fprintf (f, "\nJump functions:\n");
  FOR_EACH_FUNCTION (node)
    {
      ipa_print_node_jump_functions (f, node);
    }
}

/* Set jfunc to be a know-really nothing jump function.  */

static void
ipa_set_jf_unknown (struct ipa_jump_func *jfunc)
{
  jfunc->type = IPA_JF_UNKNOWN;
}

/* Set JFUNC to be a copy of another jmp (to be used by jump function
   combination code).  The two functions will share their rdesc.  */

static void
ipa_set_jf_cst_copy (struct ipa_jump_func *dst,
		     struct ipa_jump_func *src)

{
  gcc_checking_assert (src->type == IPA_JF_CONST);
  dst->type = IPA_JF_CONST;
  dst->value.constant = src->value.constant;
}

/* Set JFUNC to be a constant jmp function.  */

static void
ipa_set_jf_constant (struct ipa_jump_func *jfunc, tree constant,
		     struct cgraph_edge *cs)
{
  jfunc->type = IPA_JF_CONST;
  jfunc->value.constant.value = unshare_expr_without_location (constant);

  if (TREE_CODE (constant) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (constant, 0)) == FUNCTION_DECL
	  || (VAR_P (TREE_OPERAND (constant, 0))
	      && TREE_STATIC (TREE_OPERAND (constant, 0)))))
    {
      struct ipa_cst_ref_desc *rdesc;

      rdesc = ipa_refdesc_pool.allocate ();
      rdesc->cs = cs;
      rdesc->next_duplicate = NULL;
      rdesc->refcount = 1;
      jfunc->value.constant.rdesc = rdesc;
    }
  else
    jfunc->value.constant.rdesc = NULL;
}

/* Set JFUNC to be a simple pass-through jump function.  */
static void
ipa_set_jf_simple_pass_through (struct ipa_jump_func *jfunc, int formal_id,
				bool agg_preserved)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = NULL_TREE;
  jfunc->value.pass_through.op_type = NULL_TREE;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = NOP_EXPR;
  jfunc->value.pass_through.agg_preserved = agg_preserved;
  jfunc->value.pass_through.refdesc_decremented = false;
}

/* Set JFUNC to be an unary pass through jump function.  */

static void
ipa_set_jf_unary_pass_through (struct ipa_jump_func *jfunc, int formal_id,
			       enum tree_code operation, tree op_type)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = NULL_TREE;
  jfunc->value.pass_through.op_type = op_type;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = operation;
  jfunc->value.pass_through.agg_preserved = false;
  jfunc->value.pass_through.refdesc_decremented = false;
}
/* Set JFUNC to be an arithmetic pass through jump function.  */

static void
ipa_set_jf_arith_pass_through (struct ipa_jump_func *jfunc, int formal_id,
			       tree operand, enum tree_code operation,
			       tree op_type)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = unshare_expr_without_location (operand);
  jfunc->value.pass_through.op_type = op_type;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = operation;
  jfunc->value.pass_through.agg_preserved = false;
  jfunc->value.pass_through.refdesc_decremented = false;
}

/* Set JFUNC to be an ancestor jump function.  */

static void
ipa_set_ancestor_jf (struct ipa_jump_func *jfunc, HOST_WIDE_INT offset,
		     int formal_id, bool agg_preserved, bool keep_null)
{
  jfunc->type = IPA_JF_ANCESTOR;
  jfunc->value.ancestor.formal_id = formal_id;
  jfunc->value.ancestor.offset = offset;
  jfunc->value.ancestor.agg_preserved = agg_preserved;
  jfunc->value.ancestor.keep_null = keep_null;
}

/* Get IPA BB information about the given BB.  FBI is the context of analyzis
   of this function body.  */

static struct ipa_bb_info *
ipa_get_bb_info (struct ipa_func_body_info *fbi, basic_block bb)
{
  gcc_checking_assert (fbi);
  return &fbi->bb_infos[bb->index];
}

/* Structure to be passed in between detect_type_change and
   check_stmt_for_type_change.  */

struct prop_type_change_info
{
  /* Offset into the object where there is the virtual method pointer we are
     looking for.  */
  HOST_WIDE_INT offset;
  /* The declaration or SSA_NAME pointer of the base that we are checking for
     type change.  */
  tree object;
  /* Set to true if dynamic type change has been detected.  */
  bool type_maybe_changed;
};

/* Return true if STMT can modify a virtual method table pointer.

   This function makes special assumptions about both constructors and
   destructors which are all the functions that are allowed to alter the VMT
   pointers.  It assumes that destructors begin with assignment into all VMT
   pointers and that constructors essentially look in the following way:

   1) The very first thing they do is that they call constructors of ancestor
   sub-objects that have them.

   2) Then VMT pointers of this and all its ancestors is set to new values
   corresponding to the type corresponding to the constructor.

   3) Only afterwards, other stuff such as constructor of member sub-objects
   and the code written by the user is run.  Only this may include calling
   virtual functions, directly or indirectly.

   There is no way to call a constructor of an ancestor sub-object in any
   other way.

   This means that we do not have to care whether constructors get the correct
   type information because they will always change it (in fact, if we define
   the type to be given by the VMT pointer, it is undefined).

   The most important fact to derive from the above is that if, for some
   statement in the section 3, we try to detect whether the dynamic type has
   changed, we can safely ignore all calls as we examine the function body
   backwards until we reach statements in section 2 because these calls cannot
   be ancestor constructors or destructors (if the input is not bogus) and so
   do not change the dynamic type (this holds true only for automatically
   allocated objects but at the moment we devirtualize only these).  We then
   must detect that statements in section 2 change the dynamic type and can try
   to derive the new type.  That is enough and we can stop, we will never see
   the calls into constructors of sub-objects in this code.  Therefore we can
   safely ignore all call statements that we traverse.
  */

static bool
stmt_may_be_vtbl_ptr_store (gimple *stmt)
{
  if (is_gimple_call (stmt))
    return false;
  if (gimple_clobber_p (stmt))
    return false;
  else if (is_gimple_assign (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (!AGGREGATE_TYPE_P (TREE_TYPE (lhs)))
	{
	  if (flag_strict_aliasing
	      && !POINTER_TYPE_P (TREE_TYPE (lhs)))
	    return false;

	  if (TREE_CODE (lhs) == COMPONENT_REF
	      && !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
	    return false;
	  /* In the future we might want to use get_ref_base_and_extent to find
	     if there is a field corresponding to the offset and if so, proceed
	     almost like if it was a component ref.  */
	}
    }
  return true;
}

/* Callback of walk_aliased_vdefs and a helper function for detect_type_change
   to check whether a particular statement may modify the virtual table
   pointerIt stores its result into DATA, which points to a
   prop_type_change_info structure.  */

static bool
check_stmt_for_type_change (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  gimple *stmt = SSA_NAME_DEF_STMT (vdef);
  struct prop_type_change_info *tci = (struct prop_type_change_info *) data;

  if (stmt_may_be_vtbl_ptr_store (stmt))
    {
      tci->type_maybe_changed = true;
      return true;
    }
  else
    return false;
}

/* See if ARG is PARAM_DECl describing instance passed by pointer
   or reference in FUNCTION.  Return false if the dynamic type may change
   in between beggining of the function until CALL is invoked.

   Generally functions are not allowed to change type of such instances,
   but they call destructors.  We assume that methods cannot destroy the THIS
   pointer.  Also as a special cases, constructor and destructors may change
   type of the THIS pointer.  */

static bool
param_type_may_change_p (tree function, tree arg, gimple *call)
{
  /* Pure functions cannot do any changes on the dynamic type;
     that require writting to memory.  */
  if (flags_from_decl_or_type (function) & (ECF_PURE | ECF_CONST))
    return false;
  /* We need to check if we are within inlined consturctor
     or destructor (ideally we would have way to check that the
     inline cdtor is actually working on ARG, but we don't have
     easy tie on this, so punt on all non-pure cdtors.
     We may also record the types of cdtors and once we know type
     of the instance match them.

     Also code unification optimizations may merge calls from
     different blocks making return values unreliable.  So
     do nothing during late optimization.  */
  if (DECL_STRUCT_FUNCTION (function)->after_inlining)
    return true;
  if (TREE_CODE (arg) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (arg)
      && TREE_CODE (SSA_NAME_VAR (arg)) == PARM_DECL)
    {
      /* Normal (non-THIS) argument.  */
      if ((SSA_NAME_VAR (arg) != DECL_ARGUMENTS (function)
	   || TREE_CODE (TREE_TYPE (function)) != METHOD_TYPE)
	  /* THIS pointer of an method - here we want to watch constructors
	     and destructors as those definitely may change the dynamic
	     type.  */
	  || (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE
	      && !DECL_CXX_CONSTRUCTOR_P (function)
	      && !DECL_CXX_DESTRUCTOR_P (function)
	      && (SSA_NAME_VAR (arg) == DECL_ARGUMENTS (function))))
	{
	  /* Walk the inline stack and watch out for ctors/dtors.  */
	  for (tree block = gimple_block (call); block && TREE_CODE (block) == BLOCK;
	       block = BLOCK_SUPERCONTEXT (block))
	    if (inlined_polymorphic_ctor_dtor_block_p (block, false))
	      return true;
	  return false;
	}
    }
  return true;
}

/* Detect whether the dynamic type of ARG of COMP_TYPE has changed (before
   callsite CALL) by looking for assignments to its virtual table pointer.  If
   it is, return true.  ARG is the object itself (not a pointer
   to it, unless dereferenced).  BASE is the base of the memory access as
   returned by get_ref_base_and_extent, as is the offset.

   This is helper function for detect_type_change and detect_type_change_ssa
   that does the heavy work which is usually unnecesary.  */

static bool
detect_type_change_from_memory_writes (ipa_func_body_info *fbi, tree arg,
				       tree base, tree comp_type, gcall *call,
				       HOST_WIDE_INT offset)
{
  struct prop_type_change_info tci;
  ao_ref ao;

  gcc_checking_assert (DECL_P (arg)
		       || TREE_CODE (arg) == MEM_REF
		       || handled_component_p (arg));

  comp_type = TYPE_MAIN_VARIANT (comp_type);

  /* Const calls cannot call virtual methods through VMT and so type changes do
     not matter.  */
  if (!flag_devirtualize || !gimple_vuse (call)
      /* Be sure expected_type is polymorphic.  */
      || !comp_type
      || TREE_CODE (comp_type) != RECORD_TYPE
      || !TYPE_BINFO (TYPE_MAIN_VARIANT (comp_type))
      || !BINFO_VTABLE (TYPE_BINFO (TYPE_MAIN_VARIANT (comp_type))))
    return true;

  if (fbi->aa_walk_budget == 0)
    return false;

  ao_ref_init (&ao, arg);
  ao.base = base;
  ao.offset = offset;
  ao.size = POINTER_SIZE;
  ao.max_size = ao.size;

  tci.offset = offset;
  tci.object = get_base_address (arg);
  tci.type_maybe_changed = false;

  int walked
    = walk_aliased_vdefs (&ao, gimple_vuse (call), check_stmt_for_type_change,
			  &tci, NULL, NULL, fbi->aa_walk_budget);
  if (walked >= 0)
    fbi->aa_walk_budget -= walked;
  else
    fbi->aa_walk_budget = 0;

  if (walked >= 0 && !tci.type_maybe_changed)
    return false;

  return true;
}

/* Detect whether the dynamic type of ARG of COMP_TYPE may have changed.
   If it is, return true.  ARG is the object itself (not a pointer
   to it, unless dereferenced).  BASE is the base of the memory access as
   returned by get_ref_base_and_extent, as is the offset.  */

static bool
detect_type_change (ipa_func_body_info *fbi, tree arg, tree base,
		    tree comp_type, gcall *call,
		    HOST_WIDE_INT offset)
{
  if (!flag_devirtualize)
    return false;

  if (TREE_CODE	(base) == MEM_REF
      && !param_type_may_change_p (current_function_decl,
				   TREE_OPERAND (base, 0),
				   call))
    return false;
  return detect_type_change_from_memory_writes (fbi, arg, base, comp_type,
						call, offset);
}

/* Like detect_type_change but ARG is supposed to be a non-dereferenced pointer
   SSA name (its dereference will become the base and the offset is assumed to
   be zero).  */

static bool
detect_type_change_ssa (ipa_func_body_info *fbi, tree arg, tree comp_type,
			gcall *call)
{
  gcc_checking_assert (TREE_CODE (arg) == SSA_NAME);
  if (!flag_devirtualize
      || !POINTER_TYPE_P (TREE_TYPE (arg)))
    return false;

  if (!param_type_may_change_p (current_function_decl, arg, call))
    return false;

  arg = build2 (MEM_REF, ptr_type_node, arg,
		build_int_cst (ptr_type_node, 0));

  return detect_type_change_from_memory_writes (fbi, arg, arg, comp_type,
						call, 0);
}

/* Callback of walk_aliased_vdefs.  Flags that it has been invoked to the
   boolean variable pointed to by DATA.  */

static bool
mark_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef ATTRIBUTE_UNUSED,
		     void *data)
{
  bool *b = (bool *) data;
  *b = true;
  return true;
}

/* Find the nearest valid aa status for parameter specified by INDEX that
   dominates BB.  */

static struct ipa_param_aa_status *
find_dominating_aa_status (struct ipa_func_body_info *fbi, basic_block bb,
			   int index)
{
  while (true)
    {
      bb = get_immediate_dominator (CDI_DOMINATORS, bb);
      if (!bb)
	return NULL;
      struct ipa_bb_info *bi = ipa_get_bb_info (fbi, bb);
      if (!bi->param_aa_statuses.is_empty ()
	  && bi->param_aa_statuses[index].valid)
	return &bi->param_aa_statuses[index];
    }
}

/* Get AA status structure for the given BB and parameter with INDEX.  Allocate
   structures and/or intialize the result with a dominating description as
   necessary.  */

static struct ipa_param_aa_status *
parm_bb_aa_status_for_bb (struct ipa_func_body_info *fbi, basic_block bb,
			  int index)
{
  gcc_checking_assert (fbi);
  struct ipa_bb_info *bi = ipa_get_bb_info (fbi, bb);
  if (bi->param_aa_statuses.is_empty ())
    bi->param_aa_statuses.safe_grow_cleared (fbi->param_count, true);
  struct ipa_param_aa_status *paa = &bi->param_aa_statuses[index];
  if (!paa->valid)
    {
      gcc_checking_assert (!paa->parm_modified
			   && !paa->ref_modified
			   && !paa->pt_modified);
      struct ipa_param_aa_status *dom_paa;
      dom_paa = find_dominating_aa_status (fbi, bb, index);
      if (dom_paa)
	*paa = *dom_paa;
      else
	paa->valid = true;
    }

  return paa;
}

/* Return true if a load from a formal parameter PARM_LOAD is known to retrieve
   a value known not to be modified in this function before reaching the
   statement STMT.  FBI holds information about the function we have so far
   gathered but do not survive the summary building stage.  */

static bool
parm_preserved_before_stmt_p (struct ipa_func_body_info *fbi, int index,
			      gimple *stmt, tree parm_load)
{
  struct ipa_param_aa_status *paa;
  bool modified = false;
  ao_ref refd;

  tree base = get_base_address (parm_load);
  gcc_assert (TREE_CODE (base) == PARM_DECL);
  if (TREE_READONLY (base))
    return true;

  gcc_checking_assert (fbi);
  paa = parm_bb_aa_status_for_bb (fbi, gimple_bb (stmt), index);
  if (paa->parm_modified || fbi->aa_walk_budget == 0)
    return false;

  gcc_checking_assert (gimple_vuse (stmt) != NULL_TREE);
  ao_ref_init (&refd, parm_load);
  int walked = walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified,
				   &modified, NULL, NULL,
				   fbi->aa_walk_budget);
  if (walked < 0)
    {
      modified = true;
      fbi->aa_walk_budget = 0;
    }
  else
    fbi->aa_walk_budget -= walked;
  if (paa && modified)
    paa->parm_modified = true;
  return !modified;
}

/* If STMT is an assignment that loads a value from an parameter declaration,
   return the index of the parameter in ipa_node_params which has not been
   modified.  Otherwise return -1.  */

static int
load_from_unmodified_param (struct ipa_func_body_info *fbi,
			    vec<ipa_param_descriptor, va_gc> *descriptors,
			    gimple *stmt)
{
  int index;
  tree op1;

  if (!gimple_assign_single_p (stmt))
    return -1;

  op1 = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (op1) != PARM_DECL)
    return -1;

  index = ipa_get_param_decl_index_1 (descriptors, op1);
  if (index < 0
      || !parm_preserved_before_stmt_p (fbi, index, stmt, op1))
    return -1;

  return index;
}

/* Return true if memory reference REF (which must be a load through parameter
   with INDEX) loads data that are known to be unmodified in this function
   before reaching statement STMT.  */

static bool
parm_ref_data_preserved_p (struct ipa_func_body_info *fbi,
			   int index, gimple *stmt, tree ref)
{
  struct ipa_param_aa_status *paa;
  bool modified = false;
  ao_ref refd;

  gcc_checking_assert (fbi);
  paa = parm_bb_aa_status_for_bb (fbi, gimple_bb (stmt), index);
  if (paa->ref_modified || fbi->aa_walk_budget == 0)
    return false;

  gcc_checking_assert (gimple_vuse (stmt));
  ao_ref_init (&refd, ref);
  int walked = walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified,
				   &modified, NULL, NULL,
				   fbi->aa_walk_budget);
  if (walked < 0)
    {
      modified = true;
      fbi->aa_walk_budget = 0;
    }
  else
    fbi->aa_walk_budget -= walked;
  if (modified)
    paa->ref_modified = true;
  return !modified;
}

/* Return true if the data pointed to by PARM (which is a parameter with INDEX)
   is known to be unmodified in this function before reaching call statement
   CALL into which it is passed.  FBI describes the function body.  */

static bool
parm_ref_data_pass_through_p (struct ipa_func_body_info *fbi, int index,
			      gimple *call, tree parm)
{
  bool modified = false;
  ao_ref refd;

  /* It's unnecessary to calculate anything about memory contnets for a const
     function because it is not goin to use it.  But do not cache the result
     either.  Also, no such calculations for non-pointers.  */
  if (!gimple_vuse (call)
      || !POINTER_TYPE_P (TREE_TYPE (parm)))
    return false;

  struct ipa_param_aa_status *paa = parm_bb_aa_status_for_bb (fbi,
							      gimple_bb (call),
							      index);
  if (paa->pt_modified || fbi->aa_walk_budget == 0)
    return false;

  ao_ref_init_from_ptr_and_size (&refd, parm, NULL_TREE);
  int walked = walk_aliased_vdefs (&refd, gimple_vuse (call), mark_modified,
				   &modified, NULL, NULL,
				   fbi->aa_walk_budget);
  if (walked < 0)
    {
      fbi->aa_walk_budget = 0;
      modified = true;
    }
  else
    fbi->aa_walk_budget -= walked;
  if (modified)
    paa->pt_modified = true;
  return !modified;
}

/* Return true if we can prove that OP is a memory reference loading
   data from an aggregate passed as a parameter.

   The function works in two modes.  If GUARANTEED_UNMODIFIED is NULL, it return
   false if it cannot prove that the value has not been modified before the
   load in STMT.  If GUARANTEED_UNMODIFIED is not NULL, it will return true even
   if it cannot prove the value has not been modified, in that case it will
   store false to *GUARANTEED_UNMODIFIED, otherwise it will store true there.

   INFO and PARMS_AINFO describe parameters of the current function (but the
   latter can be NULL), STMT is the load statement.  If function returns true,
   *INDEX_P, *OFFSET_P and *BY_REF is filled with the parameter index, offset
   within the aggregate and whether it is a load from a value passed by
   reference respectively.

   Return false if the offset divided by BITS_PER_UNIT would not fit into an
   unsigned int.  */

bool
ipa_load_from_parm_agg (struct ipa_func_body_info *fbi,
			vec<ipa_param_descriptor, va_gc> *descriptors,
			gimple *stmt, tree op, int *index_p,
			HOST_WIDE_INT *offset_p, poly_int64 *size_p,
			bool *by_ref_p, bool *guaranteed_unmodified)
{
  int index;
  HOST_WIDE_INT size;
  bool reverse;
  tree base = get_ref_base_and_extent_hwi (op, offset_p, &size, &reverse);

  if (!base
      || (*offset_p / BITS_PER_UNIT) > UINT_MAX)
    return false;

  /* We can not propagate across volatile loads.  */
  if (TREE_THIS_VOLATILE (op))
    return false;

  if (DECL_P (base))
    {
      int index = ipa_get_param_decl_index_1 (descriptors, base);
      if (index >= 0
	  && parm_preserved_before_stmt_p (fbi, index, stmt, op))
	{
	  *index_p = index;
	  *by_ref_p = false;
	  if (size_p)
	    *size_p = size;
	  if (guaranteed_unmodified)
	    *guaranteed_unmodified = true;
	  return true;
	}
      return false;
    }

  if (TREE_CODE (base) != MEM_REF
	   || TREE_CODE (TREE_OPERAND (base, 0)) != SSA_NAME
	   || !integer_zerop (TREE_OPERAND (base, 1)))
    return false;

  if (SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0)))
    {
      tree parm = SSA_NAME_VAR (TREE_OPERAND (base, 0));
      index = ipa_get_param_decl_index_1 (descriptors, parm);
    }
  else
    {
      /* This branch catches situations where a pointer parameter is not a
	 gimple register, for example:

	 void hip7(S*) (struct S * p)
	 {
	 void (*<T2e4>) (struct S *) D.1867;
	 struct S * p.1;

	 <bb 2>:
	 p.1_1 = p;
	 D.1867_2 = p.1_1->f;
	 D.1867_2 ();
	 gdp = &p;
      */

      gimple *def = SSA_NAME_DEF_STMT (TREE_OPERAND (base, 0));
      index = load_from_unmodified_param (fbi, descriptors, def);
    }

  if (index >= 0)
    {
      bool data_preserved = parm_ref_data_preserved_p (fbi, index, stmt, op);
      if (!data_preserved && !guaranteed_unmodified)
	return false;

      *index_p = index;
      *by_ref_p = true;
      if (size_p)
	*size_p = size;
      if (guaranteed_unmodified)
	*guaranteed_unmodified = data_preserved;
      return true;
    }
  return false;
}

/* If STMT is an assignment that loads a value from a parameter declaration,
   or from an aggregate passed as the parameter either by value or reference,
   return the index of the parameter in ipa_node_params.  Otherwise return -1.

   FBI holds gathered information about the function.  INFO describes
   parameters of the function, STMT is the assignment statement.  If it is a
   memory load from an aggregate, *OFFSET_P is filled with offset within the
   aggregate, and *BY_REF_P specifies whether the aggregate is passed by
   reference.  */

static int
load_from_unmodified_param_or_agg (struct ipa_func_body_info *fbi,
				   class ipa_node_params *info,
				   gimple *stmt,
				   HOST_WIDE_INT *offset_p,
				   bool *by_ref_p)
{
  int index = load_from_unmodified_param (fbi, info->descriptors, stmt);
  poly_int64 size;

  /* Load value from a parameter declaration.  */
  if (index >= 0)
    {
      *offset_p = -1;
      return index;
    }

  if (!gimple_assign_load_p (stmt))
    return -1;

  tree rhs = gimple_assign_rhs1 (stmt);

  /* Skip memory reference containing VIEW_CONVERT_EXPR.  */
  for (tree t = rhs; handled_component_p (t); t = TREE_OPERAND (t, 0))
    if (TREE_CODE (t) == VIEW_CONVERT_EXPR)
      return -1;

  /* Skip memory reference containing bit-field.  */
  if (TREE_CODE (rhs) == BIT_FIELD_REF
      || contains_bitfld_component_ref_p (rhs))
    return -1;

  if (!ipa_load_from_parm_agg (fbi, info->descriptors, stmt, rhs, &index,
			       offset_p, &size, by_ref_p))
    return -1;

  gcc_assert (!maybe_ne (tree_to_poly_int64 (TYPE_SIZE (TREE_TYPE (rhs))),
			 size));
  if (!*by_ref_p)
    {
      tree param_type = ipa_get_type (info, index);

      if (!param_type || !AGGREGATE_TYPE_P (param_type))
	return -1;
    }
  else if (TREE_THIS_VOLATILE (rhs))
    return -1;

  return index;
}

/* Walk pointer adjustemnts from OP (such as POINTER_PLUS and ADDR_EXPR)
   to find original pointer.  Initialize RET to the pointer which results from
   the walk.
   If offset is known return true and initialize OFFSET_RET.  */

bool
unadjusted_ptr_and_unit_offset (tree op, tree *ret, poly_int64 *offset_ret)
{
  poly_int64 offset = 0;
  bool offset_known = true;
  int i;

  for (i = 0; i < param_ipa_jump_function_lookups; i++)
    {
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  poly_int64 extra_offset;
	  tree base = get_addr_base_and_unit_offset (TREE_OPERAND (op, 0),
						     &extra_offset);
	  if (!base)
	    {
	      base = get_base_address (TREE_OPERAND (op, 0));
	      if (TREE_CODE (base) != MEM_REF)
		break;
	      offset_known = false;
	    }
	  else
	    {
	      if (TREE_CODE (base) != MEM_REF)
		break;
	      offset += extra_offset;
	    }
	  op = TREE_OPERAND (base, 0);
	  if (mem_ref_offset (base).to_shwi (&extra_offset))
	    offset += extra_offset;
	  else
	    offset_known = false;
	}
      else if (TREE_CODE (op) == SSA_NAME
	       && !SSA_NAME_IS_DEFAULT_DEF (op))
	{
	  gimple *pstmt = SSA_NAME_DEF_STMT (op);

	  if (gimple_assign_single_p (pstmt))
	    op = gimple_assign_rhs1 (pstmt);
	  else if (is_gimple_assign (pstmt)
		   && gimple_assign_rhs_code (pstmt) == POINTER_PLUS_EXPR)
	    {
	      poly_int64 extra_offset = 0;
	      if (ptrdiff_tree_p (gimple_assign_rhs2 (pstmt),
		  &extra_offset))
		offset += extra_offset;
	      else
		offset_known = false;
	      op = gimple_assign_rhs1 (pstmt);
	    }
	  else
	    break;
	}
      else
	break;
    }
  *ret = op;
  *offset_ret = offset;
  return offset_known;
}

/* Given that an actual argument is an SSA_NAME (given in NAME) and is a result
   of an assignment statement STMT, try to determine whether we are actually
   handling any of the following cases and construct an appropriate jump
   function into JFUNC if so:

   1) The passed value is loaded from a formal parameter which is not a gimple
   register (most probably because it is addressable, the value has to be
   scalar) and we can guarantee the value has not changed.  This case can
   therefore be described by a simple pass-through jump function.  For example:

      foo (int a)
      {
        int a.0;

        a.0_2 = a;
        bar (a.0_2);

   2) The passed value can be described by a simple arithmetic pass-through
   jump function. E.g.

      foo (int a)
      {
        int D.2064;

        D.2064_4 = a.1(D) + 4;
        bar (D.2064_4);

   This case can also occur in combination of the previous one, e.g.:

      foo (int a, int z)
      {
        int a.0;
        int D.2064;

	a.0_3 = a;
	D.2064_4 = a.0_3 + 4;
	foo (D.2064_4);

   3) The passed value is an address of an object within another one (which
   also passed by reference).  Such situations are described by an ancestor
   jump function and describe situations such as:

     B::foo() (struct B * const this)
     {
       struct A * D.1845;

       D.1845_2 = &this_1(D)->D.1748;
       A::bar (D.1845_2);

   INFO is the structure describing individual parameters access different
   stages of IPA optimizations.  PARMS_AINFO contains the information that is
   only needed for intraprocedural analysis.  */

static void
compute_complex_assign_jump_func (struct ipa_func_body_info *fbi,
				  class ipa_node_params *info,
				  struct ipa_jump_func *jfunc,
				  gcall *call, gimple *stmt, tree name,
				  tree param_type)
{
  HOST_WIDE_INT offset, size;
  tree op1, tc_ssa, base, ssa;
  bool reverse;
  int index;

  op1 = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (op1) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (op1))
	index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op1));
      else
	index = load_from_unmodified_param (fbi, info->descriptors,
					    SSA_NAME_DEF_STMT (op1));
      tc_ssa = op1;
    }
  else
    {
      index = load_from_unmodified_param (fbi, info->descriptors, stmt);
      tc_ssa = gimple_assign_lhs (stmt);
    }

  if (index >= 0)
    {
      if (lto_variably_modified_type_p (TREE_TYPE (name)))
	return;

      switch (gimple_assign_rhs_class (stmt))
	{
	case GIMPLE_BINARY_RHS:
	  {
	    tree op2 = gimple_assign_rhs2 (stmt);
	    if (!is_gimple_ip_invariant (op2)
		|| ((TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
		     != tcc_comparison)
		    && !useless_type_conversion_p (TREE_TYPE (name),
						   TREE_TYPE (op1))))
	      return;

	    ipa_set_jf_arith_pass_through (jfunc, index, op2,
					   gimple_assign_rhs_code (stmt),
					   TREE_TYPE (name));
	    break;
	  }
	case GIMPLE_SINGLE_RHS:
	  {
	    bool agg_p = parm_ref_data_pass_through_p (fbi, index, call,
						       tc_ssa);
	    ipa_set_jf_simple_pass_through (jfunc, index, agg_p);
	    break;
	  }
	case GIMPLE_UNARY_RHS:
	  if (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt)))
	    ipa_set_jf_unary_pass_through (jfunc, index,
					   gimple_assign_rhs_code (stmt),
					   TREE_TYPE (name));
	default:;
	}
      return;
    }

  if (TREE_CODE (op1) != ADDR_EXPR)
    return;
  op1 = TREE_OPERAND (op1, 0);
  base = get_ref_base_and_extent_hwi (op1, &offset, &size, &reverse);
  offset_int mem_offset;
  if (!base
      || TREE_CODE (base) != MEM_REF
      || !mem_ref_offset (base).is_constant (&mem_offset))
    return;
  offset += mem_offset.to_short_addr () * BITS_PER_UNIT;
  ssa = TREE_OPERAND (base, 0);
  if (TREE_CODE (ssa) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (ssa)
      || offset < 0)
    return;

  /* Dynamic types are changed in constructors and destructors.  */
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (ssa));
  if (index >= 0 && param_type && POINTER_TYPE_P (param_type))
    ipa_set_ancestor_jf (jfunc, offset,  index,
			 parm_ref_data_pass_through_p (fbi, index, call, ssa),
			 false);
}

/* Extract the base, offset and MEM_REF expression from a statement ASSIGN if
   it looks like:

   iftmp.1_3 = &obj_2(D)->D.1762;

   The base of the MEM_REF must be a default definition SSA NAME of a
   parameter.  Return NULL_TREE if it looks otherwise.  If case of success, the
   whole MEM_REF expression is returned and the offset calculated from any
   handled components and the MEM_REF itself is stored into *OFFSET.  The whole
   RHS stripped off the ADDR_EXPR is stored into *OBJ_P.  */

static tree
get_ancestor_addr_info (gimple *assign, tree *obj_p, HOST_WIDE_INT *offset)
{
  HOST_WIDE_INT size;
  tree expr, parm, obj;
  bool reverse;

  if (!gimple_assign_single_p (assign))
    return NULL_TREE;
  expr = gimple_assign_rhs1 (assign);

  if (TREE_CODE (expr) != ADDR_EXPR)
    return NULL_TREE;
  expr = TREE_OPERAND (expr, 0);
  obj = expr;
  expr = get_ref_base_and_extent_hwi (expr, offset, &size, &reverse);

  offset_int mem_offset;
  if (!expr
      || TREE_CODE (expr) != MEM_REF
      || !mem_ref_offset (expr).is_constant (&mem_offset))
    return NULL_TREE;
  parm = TREE_OPERAND (expr, 0);
  if (TREE_CODE (parm) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (parm)
      || TREE_CODE (SSA_NAME_VAR (parm)) != PARM_DECL)
    return NULL_TREE;

  *offset += mem_offset.to_short_addr () * BITS_PER_UNIT;
  *obj_p = obj;
  return expr;
}


/* Given that an actual argument is an SSA_NAME that is a result of a phi
   statement PHI, try to find out whether NAME is in fact a
   multiple-inheritance typecast from a descendant into an ancestor of a formal
   parameter and thus can be described by an ancestor jump function and if so,
   write the appropriate function into JFUNC.

   Essentially we want to match the following pattern:

     if (obj_2(D) != 0B)
       goto <bb 3>;
     else
       goto <bb 4>;

   <bb 3>:
     iftmp.1_3 = &obj_2(D)->D.1762;

   <bb 4>:
     # iftmp.1_1 = PHI <iftmp.1_3(3), 0B(2)>
     D.1879_6 = middleman_1 (iftmp.1_1, i_5(D));
     return D.1879_6;  */

static void
compute_complex_ancestor_jump_func (struct ipa_func_body_info *fbi,
				    class ipa_node_params *info,
				    struct ipa_jump_func *jfunc,
				    gcall *call, gphi *phi)
{
  HOST_WIDE_INT offset;
  gimple *assign;
  basic_block phi_bb, assign_bb, cond_bb;
  tree tmp, parm, expr, obj;
  int index, i;

  if (gimple_phi_num_args (phi) != 2)
    return;

  if (integer_zerop (PHI_ARG_DEF (phi, 1)))
    tmp = PHI_ARG_DEF (phi, 0);
  else if (integer_zerop (PHI_ARG_DEF (phi, 0)))
    tmp = PHI_ARG_DEF (phi, 1);
  else
    return;
  if (TREE_CODE (tmp) != SSA_NAME
      || SSA_NAME_IS_DEFAULT_DEF (tmp)
      || !POINTER_TYPE_P (TREE_TYPE (tmp))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (tmp))) != RECORD_TYPE)
    return;

  assign = SSA_NAME_DEF_STMT (tmp);
  assign_bb = gimple_bb (assign);
  if (!single_pred_p (assign_bb))
    return;
  expr = get_ancestor_addr_info (assign, &obj, &offset);
  if (!expr)
    return;
  parm = TREE_OPERAND (expr, 0);
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (parm));
  if (index < 0)
    return;

  cond_bb = single_pred (assign_bb);
  gcond *cond = safe_dyn_cast <gcond *> (*gsi_last_bb (cond_bb));
  if (!cond
      || gimple_cond_code (cond) != NE_EXPR
      || gimple_cond_lhs (cond) != parm
      || !integer_zerop (gimple_cond_rhs (cond)))
    return;

  phi_bb = gimple_bb (phi);
  for (i = 0; i < 2; i++)
    {
      basic_block pred = EDGE_PRED (phi_bb, i)->src;
      if (pred != assign_bb && pred != cond_bb)
	return;
    }

  ipa_set_ancestor_jf (jfunc, offset, index,
		       parm_ref_data_pass_through_p (fbi, index, call, parm),
		       true);
}

/* Inspect the given TYPE and return true iff it has the same structure (the
   same number of fields of the same types) as a C++ member pointer.  If
   METHOD_PTR and DELTA are non-NULL, store the trees representing the
   corresponding fields there.  */

static bool
type_like_member_ptr_p (tree type, tree *method_ptr, tree *delta)
{
  tree fld;

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  fld = TYPE_FIELDS (type);
  if (!fld || !POINTER_TYPE_P (TREE_TYPE (fld))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (fld))) != METHOD_TYPE
      || !tree_fits_uhwi_p (DECL_FIELD_OFFSET (fld)))
    return false;

  if (method_ptr)
    *method_ptr = fld;

  fld = DECL_CHAIN (fld);
  if (!fld || INTEGRAL_TYPE_P (fld)
      || !tree_fits_uhwi_p (DECL_FIELD_OFFSET (fld)))
    return false;
  if (delta)
    *delta = fld;

  if (DECL_CHAIN (fld))
    return false;

  return true;
}

/* If RHS is an SSA_NAME and it is defined by a simple copy assign statement,
   return the rhs of its defining statement, and this statement is stored in
   *RHS_STMT.  Otherwise return RHS as it is.  */

static inline tree
get_ssa_def_if_simple_copy (tree rhs, gimple **rhs_stmt)
{
  while (TREE_CODE (rhs) == SSA_NAME && !SSA_NAME_IS_DEFAULT_DEF (rhs))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);

      if (gimple_assign_single_p (def_stmt))
	rhs = gimple_assign_rhs1 (def_stmt);
      else
	break;
      *rhs_stmt = def_stmt;
    }
  return rhs;
}

/* Simple linked list, describing contents of an aggregate before call.  */

struct ipa_known_agg_contents_list
{
  /* Offset and size of the described part of the aggregate.  */
  HOST_WIDE_INT offset, size;

  /* Type of the described part of the aggregate.  */
  tree type;

  /* Known constant value or jump function data describing contents.  */
  struct ipa_load_agg_data value;

  /* Pointer to the next structure in the list.  */
  struct ipa_known_agg_contents_list *next;
};

/* Add an aggregate content item into a linked list of
   ipa_known_agg_contents_list structure, in which all elements
   are sorted ascendingly by offset.  */

static inline void
add_to_agg_contents_list (struct ipa_known_agg_contents_list **plist,
			  struct ipa_known_agg_contents_list *item)
{
  struct ipa_known_agg_contents_list *list = *plist;

  for (; list; list = list->next)
    {
      if (list->offset >= item->offset)
	break;

      plist = &list->next;
    }

  item->next = list;
  *plist = item;
}

/* Check whether a given aggregate content is clobbered by certain element in
   a linked list of ipa_known_agg_contents_list.  */

static inline bool
clobber_by_agg_contents_list_p (struct ipa_known_agg_contents_list *list,
				struct ipa_known_agg_contents_list *item)
{
  for (; list; list = list->next)
    {
      if (list->offset >= item->offset)
	return list->offset < item->offset + item->size;

      if (list->offset + list->size > item->offset)
	return true;
    }

  return false;
}

/* Build aggregate jump function from LIST, assuming there are exactly
   VALUE_COUNT entries there and that offset of the passed argument
   is ARG_OFFSET and store it into JFUNC.  */

static void
build_agg_jump_func_from_list (struct ipa_known_agg_contents_list *list,
			       int value_count, HOST_WIDE_INT arg_offset,
			       struct ipa_jump_func *jfunc)
{
  vec_safe_reserve (jfunc->agg.items, value_count, true);
  for (; list; list = list->next)
    {
      struct ipa_agg_jf_item item;
      tree operand = list->value.pass_through.operand;

      if (list->value.pass_through.formal_id >= 0)
	{
	  /* Content value is derived from some formal paramerter.  */
	  if (list->value.offset >= 0)
	    item.jftype = IPA_JF_LOAD_AGG;
	  else
	    item.jftype = IPA_JF_PASS_THROUGH;

	  item.value.load_agg = list->value;
	  if (operand)
	    item.value.pass_through.operand
	      = unshare_expr_without_location (operand);
	}
      else if (operand)
	{
	  /* Content value is known constant.  */
	  item.jftype = IPA_JF_CONST;
	  item.value.constant = unshare_expr_without_location (operand);
	}
      else
	continue;

      item.type = list->type;
      gcc_assert (tree_to_shwi (TYPE_SIZE (list->type)) == list->size);

      item.offset = list->offset - arg_offset;
      gcc_assert ((item.offset % BITS_PER_UNIT) == 0);

      jfunc->agg.items->quick_push (item);
    }
}

/* Given an assignment statement STMT, try to collect information into
   AGG_VALUE that will be used to construct jump function for RHS of the
   assignment, from which content value of an aggregate part comes.

   Besides constant and simple pass-through jump functions, also try to
   identify whether it matches the following pattern that can be described by
   a load-value-from-aggregate jump function, which is a derivative of simple
   pass-through jump function.

     foo (int *p)
     {
       ...

       *(q_5 + 4) = *(p_3(D) + 28) op 1;
       bar (q_5);
     }

   Here IPA_LOAD_AGG_DATA data structure is informative enough to describe
   constant, simple pass-through and load-vale-from-aggregate. If value
   is constant, it will be kept in field OPERAND, and field FORMAL_ID is
   set to -1. For simple pass-through and load-value-from-aggregate, field
   FORMAL_ID specifies the related formal parameter index, and field
   OFFSET can be used to distinguish them, -1 means simple pass-through,
   otherwise means load-value-from-aggregate.  */

static void
analyze_agg_content_value (struct ipa_func_body_info *fbi,
			   struct ipa_load_agg_data *agg_value,
			   gimple *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  enum tree_code code;
  int index = -1;

  /* Initialize jump function data for the aggregate part.  */
  memset (agg_value, 0, sizeof (*agg_value));
  agg_value->pass_through.operation = NOP_EXPR;
  agg_value->pass_through.formal_id = -1;
  agg_value->offset = -1;

  if (AGGREGATE_TYPE_P (TREE_TYPE (lhs))  /* TODO: Support aggregate type.  */
      || TREE_THIS_VOLATILE (lhs)
      || TREE_CODE (lhs) == BIT_FIELD_REF
      || contains_bitfld_component_ref_p (lhs))
    return;

  /* Skip SSA copies.  */
  while (gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
    {
      if (TREE_CODE (rhs1) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (rhs1))
	break;

      stmt = SSA_NAME_DEF_STMT (rhs1);
      if (!is_gimple_assign (stmt))
	break;

      lhs = gimple_assign_lhs (stmt);
      rhs1 = gimple_assign_rhs1 (stmt);
    }

  if (gphi *phi = dyn_cast<gphi *> (stmt))
    {
      /* Also special case like the following (a is a formal parameter):

	   _12 = *a_11(D).dim[0].stride;
	   ...
	   # iftmp.22_9 = PHI <_12(2), 1(3)>
	   ...
	   parm.6.dim[0].stride = iftmp.22_9;
	   ...
	   __x_MOD_foo (&parm.6, b_31(D));

	 The aggregate function describing parm.6.dim[0].stride is encoded as a
	 PASS-THROUGH jump function with ASSERT_EXPR operation whith operand 1
	 (the constant from the PHI node).  */

      if (gimple_phi_num_args (phi) != 2
	  || lto_variably_modified_type_p (TREE_TYPE (lhs)))
	return;
      tree arg0 = gimple_phi_arg_def (phi, 0);
      tree arg1 = gimple_phi_arg_def (phi, 1);
      tree operand;

      if (is_gimple_ip_invariant (arg1))
	{
	  operand = arg1;
	  rhs1 = arg0;
	}
      else if (is_gimple_ip_invariant (arg0))
	{
	  operand = arg0;
	  rhs1 = arg1;
	}
      else
	return;

      rhs1 = get_ssa_def_if_simple_copy (rhs1, &stmt);
      if (!is_gimple_assign (stmt))
	return;

      code = ASSERT_EXPR;
      agg_value->pass_through.operand = operand;
      agg_value->pass_through.op_type = TREE_TYPE (lhs);
    }
  else if (is_gimple_assign (stmt))
    {
      code = gimple_assign_rhs_code (stmt);
      switch (gimple_assign_rhs_class (stmt))
	{
	case GIMPLE_SINGLE_RHS:
	  if (is_gimple_ip_invariant (rhs1))
	    {
	      agg_value->pass_through.operand = rhs1;
	      return;
	    }
	  code = NOP_EXPR;
	  break;

	case GIMPLE_UNARY_RHS:
	  /* NOTE: A GIMPLE_UNARY_RHS operation might not be tcc_unary
	     (truth_not_expr is example), GIMPLE_BINARY_RHS does not imply
	     tcc_binary, this subtleness is somewhat misleading.

	     Since tcc_unary is widely used in IPA-CP code to check an operation
	     with one operand, here we only allow tc_unary operation to avoid
	     possible problem.  Then we can use (opclass == tc_unary) or not to
	     distinguish unary and binary.  */
	  if (TREE_CODE_CLASS (code) != tcc_unary || CONVERT_EXPR_CODE_P (code)
	      || lto_variably_modified_type_p (TREE_TYPE (lhs)))
	    return;

	  rhs1 = get_ssa_def_if_simple_copy (rhs1, &stmt);
	  agg_value->pass_through.op_type = TREE_TYPE (lhs);
	  break;

	case GIMPLE_BINARY_RHS:
	  {
	    gimple *rhs1_stmt = stmt;
	    gimple *rhs2_stmt = stmt;
	    tree rhs2 = gimple_assign_rhs2 (stmt);

	    if (lto_variably_modified_type_p (TREE_TYPE (lhs)))
	      return;

	    rhs1 = get_ssa_def_if_simple_copy (rhs1, &rhs1_stmt);
	    rhs2 = get_ssa_def_if_simple_copy (rhs2, &rhs2_stmt);

	    if (is_gimple_ip_invariant (rhs2))
	      {
		agg_value->pass_through.operand = rhs2;
		agg_value->pass_through.op_type = TREE_TYPE (lhs);
		stmt = rhs1_stmt;
	      }
	    else if (is_gimple_ip_invariant (rhs1))
	      {
		if (TREE_CODE_CLASS (code) == tcc_comparison)
		  code = swap_tree_comparison (code);
		else if (!commutative_tree_code (code))
		  return;

		agg_value->pass_through.operand = rhs1;
		agg_value->pass_through.op_type = TREE_TYPE (lhs);
		stmt = rhs2_stmt;
		rhs1 = rhs2;
	      }
	    else
	      return;

	    if (TREE_CODE_CLASS (code) != tcc_comparison
		&& !useless_type_conversion_p (TREE_TYPE (lhs),
					       TREE_TYPE (rhs1)))
	      return;
	  }
	  break;

	default:
	  return;
	}
    }
  else
    return;

  if (TREE_CODE (rhs1) != SSA_NAME)
    index = load_from_unmodified_param_or_agg (fbi, fbi->info, stmt,
					       &agg_value->offset,
					       &agg_value->by_ref);
  else if (SSA_NAME_IS_DEFAULT_DEF (rhs1))
    index = ipa_get_param_decl_index (fbi->info, SSA_NAME_VAR (rhs1));

  if (index >= 0)
    {
      if (agg_value->offset >= 0)
	agg_value->type = TREE_TYPE (rhs1);
      agg_value->pass_through.formal_id = index;
      agg_value->pass_through.operation = code;
    }
  else
    agg_value->pass_through.operand = NULL_TREE;
}

/* If STMT is a memory store to the object whose address is BASE, extract
   information (offset, size, and value) into CONTENT, and return true,
   otherwise we conservatively assume the whole object is modified with
   unknown content, and return false.  CHECK_REF means that access to object
   is expected to be in form of MEM_REF expression.  */

static bool
extract_mem_content (struct ipa_func_body_info *fbi,
		     gimple *stmt, tree base, bool check_ref,
		     struct ipa_known_agg_contents_list *content)
{
  HOST_WIDE_INT lhs_offset, lhs_size;
  bool reverse;

  if (!is_gimple_assign (stmt))
    return false;

  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_base = get_ref_base_and_extent_hwi (lhs, &lhs_offset, &lhs_size,
					       &reverse);
  if (!lhs_base)
    return false;

  if (check_ref)
    {
      if (TREE_CODE (lhs_base) != MEM_REF
	  || TREE_OPERAND (lhs_base, 0) != base
	  || !integer_zerop (TREE_OPERAND (lhs_base, 1)))
	return false;
    }
  else if (lhs_base != base)
    return false;

  content->offset = lhs_offset;
  content->size = lhs_size;
  content->type = TREE_TYPE (lhs);
  content->next = NULL;

  analyze_agg_content_value (fbi, &content->value, stmt);
  return true;
}

/* Traverse statements from CALL backwards, scanning whether an aggregate given
   in ARG is filled in constants or values that are derived from caller's
   formal parameter in the way described by some kinds of jump functions.  FBI
   is the context of the caller function for interprocedural analysis.  ARG can
   either be an aggregate expression or a pointer to an aggregate.  ARG_TYPE is
   the type of the aggregate, JFUNC is the jump function for the aggregate.  */

static void
determine_known_aggregate_parts (struct ipa_func_body_info *fbi,
				 gcall *call, tree arg,
				 tree arg_type,
				 struct ipa_jump_func *jfunc)
{
  struct ipa_known_agg_contents_list *list = NULL, *all_list = NULL;
  bitmap visited = NULL;
  int item_count = 0, value_count = 0;
  HOST_WIDE_INT arg_offset, arg_size;
  tree arg_base;
  bool check_ref, by_ref;
  ao_ref r;
  int max_agg_items = opt_for_fn (fbi->node->decl, param_ipa_max_agg_items);

  if (max_agg_items == 0)
    return;

  /* The function operates in three stages.  First, we prepare check_ref, r,
     arg_base and arg_offset based on what is actually passed as an actual
     argument.  */

  if (POINTER_TYPE_P (arg_type))
    {
      by_ref = true;
      if (TREE_CODE (arg) == SSA_NAME)
	{
	  tree type_size;
          if (!tree_fits_uhwi_p (TYPE_SIZE (TREE_TYPE (arg_type)))
	      || !POINTER_TYPE_P (TREE_TYPE (arg)))
            return;
	  check_ref = true;
	  arg_base = arg;
	  arg_offset = 0;
	  type_size = TYPE_SIZE (TREE_TYPE (arg_type));
	  arg_size = tree_to_uhwi (type_size);
	  ao_ref_init_from_ptr_and_size (&r, arg_base, NULL_TREE);
	}
      else if (TREE_CODE (arg) == ADDR_EXPR)
	{
	  bool reverse;

	  arg = TREE_OPERAND (arg, 0);
	  arg_base = get_ref_base_and_extent_hwi (arg, &arg_offset,
						  &arg_size, &reverse);
	  if (!arg_base)
	    return;
	  if (DECL_P (arg_base))
	    {
	      check_ref = false;
	      ao_ref_init (&r, arg_base);
	    }
	  else
	    return;
	}
      else
	return;
    }
  else
    {
      bool reverse;

      gcc_checking_assert (AGGREGATE_TYPE_P (TREE_TYPE (arg)));

      by_ref = false;
      check_ref = false;
      arg_base = get_ref_base_and_extent_hwi (arg, &arg_offset,
					      &arg_size, &reverse);
      if (!arg_base)
	return;

      ao_ref_init (&r, arg);
    }

  /* Second stage traverses virtual SSA web backwards starting from the call
     statement, only looks at individual dominating virtual operand (its
     definition dominates the call), as long as it is confident that content
     of the aggregate is affected by definition of the virtual operand, it
     builds a sorted linked list of ipa_agg_jf_list describing that.  */

  for (tree dom_vuse = gimple_vuse (call);
       dom_vuse && fbi->aa_walk_budget > 0;)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (dom_vuse);

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  dom_vuse = get_continuation_for_phi (stmt, &r, true,
					       fbi->aa_walk_budget,
					       &visited, false, NULL, NULL);
	  continue;
	}

      fbi->aa_walk_budget--;
      if (stmt_may_clobber_ref_p_1 (stmt, &r))
	{
	  struct ipa_known_agg_contents_list *content
			= XALLOCA (struct ipa_known_agg_contents_list);

	  if (!extract_mem_content (fbi, stmt, arg_base, check_ref, content))
	    break;

	  /* Now we get a dominating virtual operand, and need to check
	     whether its value is clobbered any other dominating one.  */
	  if ((content->value.pass_through.formal_id >= 0
	       || content->value.pass_through.operand)
	      && !clobber_by_agg_contents_list_p (all_list, content)
	      /* Since IPA-CP stores results with unsigned int offsets, we can
		 discard those which would not fit now before we stream them to
		 WPA.  */
	      && (content->offset + content->size - arg_offset
		  <= (HOST_WIDE_INT) UINT_MAX * BITS_PER_UNIT))
	    {
	      struct ipa_known_agg_contents_list *copy
			= XALLOCA (struct ipa_known_agg_contents_list);

	      /* Add to the list consisting of only dominating virtual
		 operands, whose definitions can finally reach the call.  */
	      add_to_agg_contents_list (&list, (*copy = *content, copy));

	      if (++value_count == max_agg_items)
		break;
	    }

	  /* Add to the list consisting of all dominating virtual operands.  */
	  add_to_agg_contents_list (&all_list, content);

	  if (++item_count == 2 * max_agg_items)
	    break;
	}
      dom_vuse = gimple_vuse (stmt);
   }

  if (visited)
    BITMAP_FREE (visited);

  /* Third stage just goes over the list and creates an appropriate vector of
     ipa_agg_jf_item structures out of it, of course only if there are
     any meaningful items to begin with.  */

  if (value_count)
    {
      jfunc->agg.by_ref = by_ref;
      build_agg_jump_func_from_list (list, value_count, arg_offset, jfunc);
    }
}


/* Return the Ith param type of callee associated with call graph
   edge E.  */

tree
ipa_get_callee_param_type (struct cgraph_edge *e, int i)
{
  int n;
  tree type = (e->callee
	       ? TREE_TYPE (e->callee->decl)
	       : gimple_call_fntype (e->call_stmt));
  tree t = TYPE_ARG_TYPES (type);

  for (n = 0; n < i; n++)
    {
      if (!t)
        break;
      t = TREE_CHAIN (t);
    }
  if (t && t != void_list_node)
    return TREE_VALUE (t);
  if (!e->callee)
    return NULL;
  t = DECL_ARGUMENTS (e->callee->decl);
  for (n = 0; n < i; n++)
    {
      if (!t)
	return NULL;
      t = TREE_CHAIN (t);
    }
  if (t)
    return TREE_TYPE (t);
  return NULL;
}

/* Return a pointer to an ipa_vr just like TMP, but either find it in
   ipa_vr_hash_table or allocate it in GC memory.  */

static ipa_vr *
ipa_get_value_range (const vrange &tmp)
{
  inchash::hash hstate;
  inchash::add_vrange (tmp, hstate);
  hashval_t hash = hstate.end ();
  ipa_vr **slot = ipa_vr_hash_table->find_slot_with_hash (&tmp, hash, INSERT);
  if (*slot)
    return *slot;

  ipa_vr *vr = new (ggc_alloc<ipa_vr> ()) ipa_vr (tmp);
  *slot = vr;
  return vr;
}

/* Assign to JF a pointer to a range just like TMP but either fetch a
   copy from ipa_vr_hash_table or allocate a new on in GC memory.  */

static void
ipa_set_jfunc_vr (ipa_jump_func *jf, const vrange &tmp)
{
  jf->m_vr = ipa_get_value_range (tmp);
}

static void
ipa_set_jfunc_vr (ipa_jump_func *jf, const ipa_vr &vr)
{
  value_range tmp;
  vr.get_vrange (tmp);
  ipa_set_jfunc_vr (jf, tmp);
}

/* Given VAL that conforms to is_gimple_ip_invariant, produce a VRANGE that
   represents it as a range.  CONTEXT_NODE is the call graph node representing
   the function for which optimization flags should be evaluated.  */

void
ipa_get_range_from_ip_invariant (vrange &r, tree val, cgraph_node *context_node)
{
  if (TREE_CODE (val) == ADDR_EXPR)
    {
      symtab_node *symbol;
      tree base = TREE_OPERAND (val, 0);
      if (!DECL_P (base))
	{
	  r.set_varying (TREE_TYPE (val));
	  return;
	}
      if (!decl_in_symtab_p (base))
	{
	  r.set_nonzero (TREE_TYPE (val));
	  return;
	}
      if (!(symbol = symtab_node::get (base)))
	{
	  r.set_varying (TREE_TYPE (val));
	  return;
	}

      bool delete_null_pointer_checks
	= opt_for_fn (context_node->decl, flag_delete_null_pointer_checks);
      if (symbol->nonzero_address (delete_null_pointer_checks))
	r.set_nonzero (TREE_TYPE (val));
      else
	r.set_varying (TREE_TYPE (val));
    }
  else
    r.set (val, val);
}

/* If T is an SSA_NAME that is the result of a simple type conversion statement
   from an integer type to another integer type which is known to be able to
   represent the values the operand of the conversion can hold, return the
   operand of that conversion, otherwise return T.  */

static tree
skip_a_safe_conversion_op (tree t)
{
  if (TREE_CODE (t) != SSA_NAME
      || SSA_NAME_IS_DEFAULT_DEF (t))
    return t;

  gimple *def = SSA_NAME_DEF_STMT (t);
  if (!is_gimple_assign (def)
      || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def))
      || !INTEGRAL_TYPE_P (TREE_TYPE (t))
      || !INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (def))))
    return t;

  tree rhs1 = gimple_assign_rhs1 (def);
  if (TYPE_PRECISION (TREE_TYPE (t))
      >= TYPE_PRECISION (TREE_TYPE (rhs1)))
    return gimple_assign_rhs1 (def);

  value_range vr (TREE_TYPE (rhs1));
  if (!get_range_query (cfun)->range_of_expr (vr, rhs1, def)
      || vr.undefined_p ())
    return t;

  irange &ir = as_a <irange> (vr);
  if (range_fits_type_p (&ir, TYPE_PRECISION (TREE_TYPE (t)),
			 TYPE_SIGN (TREE_TYPE (t))))
      return gimple_assign_rhs1 (def);

  return t;
}

/* Compute jump function for all arguments of callsite CS and insert the
   information in the jump_functions array in the ipa_edge_args corresponding
   to this callsite.  */

static void
ipa_compute_jump_functions_for_edge (struct ipa_func_body_info *fbi,
				     struct cgraph_edge *cs)
{
  ipa_node_params *info = ipa_node_params_sum->get (cs->caller);
  ipa_edge_args *args = ipa_edge_args_sum->get_create (cs);
  gcall *call = cs->call_stmt;
  int n, arg_num = gimple_call_num_args (call);
  bool useful_context = false;

  if (arg_num == 0 || args->jump_functions)
    return;
  vec_safe_grow_cleared (args->jump_functions, arg_num, true);
  if (flag_devirtualize)
    vec_safe_grow_cleared (args->polymorphic_call_contexts, arg_num, true);

  if (gimple_call_internal_p (call))
    return;
  if (ipa_func_spec_opts_forbid_analysis_p (cs->caller))
    return;

  for (n = 0; n < arg_num; n++)
    {
      struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, n);
      tree arg = gimple_call_arg (call, n);
      tree param_type = ipa_get_callee_param_type (cs, n);
      if (flag_devirtualize && POINTER_TYPE_P (TREE_TYPE (arg)))
	{
	  tree instance;
	  class ipa_polymorphic_call_context context (cs->caller->decl,
						       arg, cs->call_stmt,
						       &instance);
	  context.get_dynamic_type (instance, arg, NULL, cs->call_stmt,
				    &fbi->aa_walk_budget);
	  *ipa_get_ith_polymorhic_call_context (args, n) = context;
	  if (!context.useless_p ())
	    useful_context = true;
	}

      value_range vr (TREE_TYPE (arg));
      if (POINTER_TYPE_P (TREE_TYPE (arg)))
	{
	  if (!get_range_query (cfun)->range_of_expr (vr, arg, cs->call_stmt)
	      || vr.varying_p ()
	      || vr.undefined_p ())
	    {
	      bool strict_overflow = false;
	      if (tree_single_nonzero_warnv_p (arg, &strict_overflow))
		vr.set_nonzero (TREE_TYPE (arg));
	      else
		vr.set_varying (TREE_TYPE (arg));
	    }
	  gcc_assert (!vr.undefined_p ());
	  unsigned HOST_WIDE_INT bitpos;
	  unsigned align = BITS_PER_UNIT;

	  if (!vr.singleton_p ())
	    get_pointer_alignment_1 (arg, &align, &bitpos);

	  if (align > BITS_PER_UNIT
	      && opt_for_fn (cs->caller->decl, flag_ipa_bit_cp))
	    {
	      unsigned prec = TYPE_PRECISION (TREE_TYPE (arg));
	      wide_int mask
		= wi::bit_and_not (wi::mask (prec, false, prec),
				   wide_int::from (align / BITS_PER_UNIT - 1,
						   prec, UNSIGNED));
	      wide_int value = wide_int::from (bitpos / BITS_PER_UNIT, prec,
					       UNSIGNED);
	      irange_bitmask bm (value, mask);
	      vr.update_bitmask (bm);
	      ipa_set_jfunc_vr (jfunc, vr);
	    }
	  else if (!vr.varying_p ())
	    ipa_set_jfunc_vr (jfunc, vr);
	  else
	    gcc_assert (!jfunc->m_vr);
	}
      else
	{
	  if (param_type
	      && ipa_vr_supported_type_p (TREE_TYPE (arg))
	      && ipa_vr_supported_type_p (param_type)
	      && get_range_query (cfun)->range_of_expr (vr, arg, cs->call_stmt)
	      && !vr.undefined_p ())
	    {
	      value_range resvr (vr);
	      range_cast (resvr, param_type);
	      if (!resvr.undefined_p () && !resvr.varying_p ())
		ipa_set_jfunc_vr (jfunc, resvr);
	      else
		gcc_assert (!jfunc->m_vr);
	    }
	  else
	    gcc_assert (!jfunc->m_vr);
	}

      arg = skip_a_safe_conversion_op (arg);
      if (is_gimple_ip_invariant (arg)
	  || (VAR_P (arg)
	      && is_global_var (arg)
	      && TREE_READONLY (arg)))
	ipa_set_jf_constant (jfunc, arg, cs);
      else if (!is_gimple_reg_type (TREE_TYPE (arg))
	       && TREE_CODE (arg) == PARM_DECL)
	{
	  int index = ipa_get_param_decl_index (info, arg);

	  gcc_assert (index >=0);
	  /* Aggregate passed by value, check for pass-through, otherwise we
	     will attempt to fill in aggregate contents later in this
	     for cycle.  */
	  if (parm_preserved_before_stmt_p (fbi, index, call, arg))
	    {
	      ipa_set_jf_simple_pass_through (jfunc, index, false);
	      continue;
	    }
	}
      else if (TREE_CODE (arg) == SSA_NAME)
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    {
	      int index = ipa_get_param_decl_index (info, SSA_NAME_VAR (arg));
	      if (index >= 0)
		{
		  bool agg_p;
		  agg_p = parm_ref_data_pass_through_p (fbi, index, call, arg);
		  ipa_set_jf_simple_pass_through (jfunc, index, agg_p);
		}
	    }
	  else
	    {
	      gimple *stmt = SSA_NAME_DEF_STMT (arg);
	      if (is_gimple_assign (stmt))
		compute_complex_assign_jump_func (fbi, info, jfunc,
						  call, stmt, arg, param_type);
	      else if (gimple_code (stmt) == GIMPLE_PHI)
		compute_complex_ancestor_jump_func (fbi, info, jfunc,
						    call,
						    as_a <gphi *> (stmt));
	    }
	}

      /* If ARG is pointer, we cannot use its type to determine the type of aggregate
	 passed (because type conversions are ignored in gimple).  Usually we can
	 safely get type from function declaration, but in case of K&R prototypes or
	 variadic functions we can try our luck with type of the pointer passed.
	 TODO: Since we look for actual initialization of the memory object, we may better
	 work out the type based on the memory stores we find.  */
      if (!param_type)
	param_type = TREE_TYPE (arg);

      if ((jfunc->type != IPA_JF_PASS_THROUGH
	      || !ipa_get_jf_pass_through_agg_preserved (jfunc))
	  && (jfunc->type != IPA_JF_ANCESTOR
	      || !ipa_get_jf_ancestor_agg_preserved (jfunc))
	  && (AGGREGATE_TYPE_P (TREE_TYPE (arg))
	      || POINTER_TYPE_P (param_type)))
	determine_known_aggregate_parts (fbi, call, arg, param_type, jfunc);
    }
  if (!useful_context)
    vec_free (args->polymorphic_call_contexts);
}

/* Compute jump functions for all edges - both direct and indirect - outgoing
   from BB.  */

static void
ipa_compute_jump_functions_for_bb (struct ipa_func_body_info *fbi, basic_block bb)
{
  struct ipa_bb_info *bi = ipa_get_bb_info (fbi, bb);
  int i;
  struct cgraph_edge *cs;

  FOR_EACH_VEC_ELT_REVERSE (bi->cg_edges, i, cs)
    {
      struct cgraph_node *callee = cs->callee;

      if (callee)
	{
	  callee = callee->ultimate_alias_target ();
	  /* We do not need to bother analyzing calls to unknown functions
	     unless they may become known during lto/whopr.  */
	  if (!callee->definition && !flag_lto
	      && !gimple_call_fnspec (cs->call_stmt).known_p ())
	    continue;
	}
      ipa_compute_jump_functions_for_edge (fbi, cs);
    }
}

/* If STMT looks like a statement loading a value from a member pointer formal
   parameter, return that parameter and store the offset of the field to
   *OFFSET_P, if it is non-NULL.  Otherwise return NULL (but *OFFSET_P still
   might be clobbered).  If USE_DELTA, then we look for a use of the delta
   field rather than the pfn.  */

static tree
ipa_get_stmt_member_ptr_load_param (gimple *stmt, bool use_delta,
				    HOST_WIDE_INT *offset_p)
{
  tree rhs, fld, ptr_field, delta_field;
  tree ref_field = NULL_TREE;
  tree ref_offset = NULL_TREE;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (rhs) == COMPONENT_REF)
    {
      ref_field = TREE_OPERAND (rhs, 1);
      rhs = TREE_OPERAND (rhs, 0);
    }

  if (TREE_CODE (rhs) == MEM_REF)
    {
      ref_offset = TREE_OPERAND (rhs, 1);
      if (ref_field && integer_nonzerop (ref_offset))
	return NULL_TREE;
    }
  else if (!ref_field)
    return NULL_TREE;

  if (TREE_CODE (rhs) == MEM_REF
      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (rhs, 0)))
    {
      rhs = TREE_OPERAND (rhs, 0);
      if (TREE_CODE (SSA_NAME_VAR (rhs)) != PARM_DECL
	  || !type_like_member_ptr_p (TREE_TYPE (TREE_TYPE (rhs)), &ptr_field,
				      &delta_field))
	return NULL_TREE;
    }
  else
    {
      if (TREE_CODE (rhs) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (rhs, 0)) == ADDR_EXPR)
	rhs = TREE_OPERAND (TREE_OPERAND (rhs, 0), 0);
      if (TREE_CODE (rhs) != PARM_DECL
	  || !type_like_member_ptr_p (TREE_TYPE (rhs), &ptr_field,
				      &delta_field))
	return NULL_TREE;
    }

  if (use_delta)
    fld = delta_field;
  else
    fld = ptr_field;

  if (ref_field)
    {
      if (ref_field != fld)
	return NULL_TREE;
    }
  else if (!tree_int_cst_equal (byte_position (fld), ref_offset))
    return NULL_TREE;

  if (offset_p)
    *offset_p = int_bit_position (fld);
  return rhs;
}

/* Returns true iff T is an SSA_NAME defined by a statement.  */

static bool
ipa_is_ssa_with_stmt_def (tree t)
{
  if (TREE_CODE (t) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (t))
    return true;
  else
    return false;
}

/* Find the indirect call graph edge corresponding to STMT and mark it as a
   call to a parameter number PARAM_INDEX.  NODE is the caller.  Return the
   indirect call graph edge.
   If POLYMORPHIC is true record is as a destination of polymorphic call.  */

static struct cgraph_edge *
ipa_note_param_call (struct cgraph_node *node, int param_index,
		     gcall *stmt, bool polymorphic)
{
  struct cgraph_edge *cs;

  cs = node->get_edge (stmt);
  cs->indirect_info->param_index = param_index;
  cs->indirect_info->agg_contents = 0;
  cs->indirect_info->member_ptr = 0;
  cs->indirect_info->guaranteed_unmodified = 0;
  ipa_node_params *info = ipa_node_params_sum->get (node);
  ipa_set_param_used_by_indirect_call (info, param_index, true);
  if (cs->indirect_info->polymorphic || polymorphic)
    ipa_set_param_used_by_polymorphic_call (info, param_index, true);
  return cs;
}

/* Analyze the CALL and examine uses of formal parameters of the caller NODE
   (described by INFO).  PARMS_AINFO is a pointer to a vector containing
   intermediate information about each formal parameter.  Currently it checks
   whether the call calls a pointer that is a formal parameter and if so, the
   parameter is marked with the called flag and an indirect call graph edge
   describing the call is created.  This is very simple for ordinary pointers
   represented in SSA but not-so-nice when it comes to member pointers.  The
   ugly part of this function does nothing more than trying to match the
   pattern of such a call.  Look up the documentation of macro
   TARGET_PTRMEMFUNC_VBIT_LOCATION for details.  An example of such a pattern
   is the gimple dump below, the call is on the last line:

     <bb 2>:
       f$__delta_5 = f.__delta;
       f$__pfn_24 = f.__pfn;

   or
     <bb 2>:
       f$__delta_5 = MEM[(struct  *)&f];
       f$__pfn_24 = MEM[(struct  *)&f + 4B];

   and a few lines below:

     <bb 5>
       D.2496_3 = (int) f$__pfn_24;
       D.2497_4 = D.2496_3 & 1;
       if (D.2497_4 != 0)
         goto <bb 3>;
       else
         goto <bb 4>;

     <bb 6>:
       D.2500_7 = (unsigned int) f$__delta_5;
       D.2501_8 = &S + D.2500_7;
       D.2502_9 = (int (*__vtbl_ptr_type) (void) * *) D.2501_8;
       D.2503_10 = *D.2502_9;
       D.2504_12 = f$__pfn_24 + -1;
       D.2505_13 = (unsigned int) D.2504_12;
       D.2506_14 = D.2503_10 + D.2505_13;
       D.2507_15 = *D.2506_14;
       iftmp.11_16 = (String:: *) D.2507_15;

     <bb 7>:
       # iftmp.11_1 = PHI <iftmp.11_16(3), f$__pfn_24(2)>
       D.2500_19 = (unsigned int) f$__delta_5;
       D.2508_20 = &S + D.2500_19;
       D.2493_21 = iftmp.11_1 (D.2508_20, 4);

   Such patterns are results of simple calls to a member pointer:

     int doprinting (int (MyString::* f)(int) const)
     {
       MyString S ("somestring");

       return (S.*f)(4);
     }

   Moreover, the function also looks for called pointers loaded from aggregates
   passed by value or reference.  */

static void
ipa_analyze_indirect_call_uses (struct ipa_func_body_info *fbi, gcall *call,
				tree target)
{
  class ipa_node_params *info = fbi->info;
  HOST_WIDE_INT offset;
  bool by_ref;

  if (SSA_NAME_IS_DEFAULT_DEF (target))
    {
      tree var = SSA_NAME_VAR (target);
      int index = ipa_get_param_decl_index (info, var);
      if (index >= 0)
	ipa_note_param_call (fbi->node, index, call, false);
      return;
    }

  int index;
  gimple *def = SSA_NAME_DEF_STMT (target);
  bool guaranteed_unmodified;
  if (gimple_assign_single_p (def)
      && ipa_load_from_parm_agg (fbi, info->descriptors, def,
				 gimple_assign_rhs1 (def), &index, &offset,
				 NULL, &by_ref, &guaranteed_unmodified))
    {
      struct cgraph_edge *cs = ipa_note_param_call (fbi->node, index,
	 					    call, false);
      cs->indirect_info->offset = offset;
      cs->indirect_info->agg_contents = 1;
      cs->indirect_info->by_ref = by_ref;
      cs->indirect_info->guaranteed_unmodified = guaranteed_unmodified;
      return;
    }

  /* Now we need to try to match the complex pattern of calling a member
     pointer. */
  if (gimple_code (def) != GIMPLE_PHI
      || gimple_phi_num_args (def) != 2
      || !POINTER_TYPE_P (TREE_TYPE (target))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (target))) != METHOD_TYPE)
    return;

  /* First, we need to check whether one of these is a load from a member
     pointer that is a parameter to this function. */
  tree n1 = PHI_ARG_DEF (def, 0);
  tree n2 = PHI_ARG_DEF (def, 1);
  if (!ipa_is_ssa_with_stmt_def (n1) || !ipa_is_ssa_with_stmt_def (n2))
    return;
  gimple *d1 = SSA_NAME_DEF_STMT (n1);
  gimple *d2 = SSA_NAME_DEF_STMT (n2);

  tree rec;
  basic_block bb, virt_bb;
  basic_block join = gimple_bb (def);
  if ((rec = ipa_get_stmt_member_ptr_load_param (d1, false, &offset)))
    {
      if (ipa_get_stmt_member_ptr_load_param (d2, false, NULL))
	return;

      bb = EDGE_PRED (join, 0)->src;
      virt_bb = gimple_bb (d2);
    }
  else if ((rec = ipa_get_stmt_member_ptr_load_param (d2, false, &offset)))
    {
      bb = EDGE_PRED (join, 1)->src;
      virt_bb = gimple_bb (d1);
    }
  else
    return;

  /* Second, we need to check that the basic blocks are laid out in the way
     corresponding to the pattern. */

  if (!single_pred_p (virt_bb) || !single_succ_p (virt_bb)
      || single_succ (virt_bb) != join)
    return;


  if (single_pred (virt_bb) != bb)
    {
      /* In cases when the distinction between a normal and a virtual
	 function is encoded in the delta field, the load of the
	 actual non-virtual function pointer can be in its own BB.  */

      if (!single_pred_p (bb) || !single_succ_p (bb))
	return;
      bb = single_pred (bb);
      if (bb != single_pred (virt_bb))
	return;
    }

  /* Third, let's see that the branching is done depending on the least
     significant bit of the pfn. */

  gcond *branch = safe_dyn_cast <gcond *> (*gsi_last_bb (bb));
  if (!branch)
    return;

  if ((gimple_cond_code (branch) != NE_EXPR
       && gimple_cond_code (branch) != EQ_EXPR)
      || !integer_zerop (gimple_cond_rhs (branch)))
    return;

  tree cond = gimple_cond_lhs (branch);
  if (!ipa_is_ssa_with_stmt_def (cond))
    return;

  def = SSA_NAME_DEF_STMT (cond);
  if (!is_gimple_assign (def)
      || gimple_assign_rhs_code (def) != BIT_AND_EXPR
      || !integer_onep (gimple_assign_rhs2 (def)))
    return;

  cond = gimple_assign_rhs1 (def);
  if (!ipa_is_ssa_with_stmt_def (cond))
    return;

  def = SSA_NAME_DEF_STMT (cond);

  if (is_gimple_assign (def)
      && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def)))
    {
      cond = gimple_assign_rhs1 (def);
      if (!ipa_is_ssa_with_stmt_def (cond))
	return;
      def = SSA_NAME_DEF_STMT (cond);
    }

  tree rec2;
  rec2 = ipa_get_stmt_member_ptr_load_param (def,
					     (TARGET_PTRMEMFUNC_VBIT_LOCATION
					      == ptrmemfunc_vbit_in_delta),
					     NULL);
  if (rec != rec2)
    return;

  if (TREE_CODE (rec) == SSA_NAME)
    {
      index = ipa_get_param_decl_index (info, SSA_NAME_VAR (rec));
      if (index < 0
	  || !parm_ref_data_preserved_p (fbi, index, call,
					 gimple_assign_rhs1 (def)))
	return;
      by_ref = true;
    }
  else
    {
      index = ipa_get_param_decl_index (info, rec);
      if (index < 0
	  || !parm_preserved_before_stmt_p (fbi, index, call, rec))
	return;
      by_ref = false;
    }

  struct cgraph_edge *cs = ipa_note_param_call (fbi->node, index,
						call, false);
  cs->indirect_info->offset = offset;
  cs->indirect_info->agg_contents = 1;
  cs->indirect_info->member_ptr = 1;
  cs->indirect_info->by_ref = by_ref;
  cs->indirect_info->guaranteed_unmodified = 1;

  return;
}

/* Analyze a CALL to an OBJ_TYPE_REF which is passed in TARGET and if the
   object referenced in the expression is a formal parameter of the caller
   FBI->node (described by FBI->info), create a call note for the
   statement.  */

static void
ipa_analyze_virtual_call_uses (struct ipa_func_body_info *fbi,
			       gcall *call, tree target)
{
  tree obj = OBJ_TYPE_REF_OBJECT (target);
  int index;
  HOST_WIDE_INT anc_offset;

  if (!flag_devirtualize)
    return;

  if (TREE_CODE (obj) != SSA_NAME)
    return;

  class ipa_node_params *info = fbi->info;
  if (SSA_NAME_IS_DEFAULT_DEF (obj))
    {
      if (TREE_CODE (SSA_NAME_VAR (obj)) != PARM_DECL)
	return;

      anc_offset = 0;
      index = ipa_get_param_decl_index (info, SSA_NAME_VAR (obj));
      gcc_assert (index >= 0);
      if (detect_type_change_ssa (fbi, obj, obj_type_ref_class (target),
				  call))
	return;
    }
  else
    {
      gimple *stmt = SSA_NAME_DEF_STMT (obj);
      tree expr;

      expr = get_ancestor_addr_info (stmt, &obj, &anc_offset);
      if (!expr)
	return;
      index = ipa_get_param_decl_index (info,
					SSA_NAME_VAR (TREE_OPERAND (expr, 0)));
      gcc_assert (index >= 0);
      if (detect_type_change (fbi, obj, expr, obj_type_ref_class (target),
			      call, anc_offset))
	return;
    }

  struct cgraph_edge *cs = ipa_note_param_call (fbi->node, index,
     						call, true);
  class cgraph_indirect_call_info *ii = cs->indirect_info;
  ii->offset = anc_offset;
  ii->otr_token = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (target));
  ii->otr_type = obj_type_ref_class (target);
  ii->polymorphic = 1;
}

/* Analyze a call statement CALL whether and how it utilizes formal parameters
   of the caller (described by INFO).  PARMS_AINFO is a pointer to a vector
   containing intermediate information about each formal parameter.  */

static void
ipa_analyze_call_uses (struct ipa_func_body_info *fbi, gcall *call)
{
  tree target = gimple_call_fn (call);

  if (!target
      || (TREE_CODE (target) != SSA_NAME
          && !virtual_method_call_p (target)))
    return;

  struct cgraph_edge *cs = fbi->node->get_edge (call);
  /* If we previously turned the call into a direct call, there is
     no need to analyze.  */
  if (cs && !cs->indirect_unknown_callee)
    return;

  if (cs->indirect_info->polymorphic && flag_devirtualize)
    {
      tree instance;
      tree target = gimple_call_fn (call);
      ipa_polymorphic_call_context context (current_function_decl,
					    target, call, &instance);

      gcc_checking_assert (cs->indirect_info->otr_type
			   == obj_type_ref_class (target));
      gcc_checking_assert (cs->indirect_info->otr_token
			   == tree_to_shwi (OBJ_TYPE_REF_TOKEN (target)));

      cs->indirect_info->vptr_changed
	= !context.get_dynamic_type (instance,
				     OBJ_TYPE_REF_OBJECT (target),
				     obj_type_ref_class (target), call,
				     &fbi->aa_walk_budget);
      cs->indirect_info->context = context;
    }

  if (TREE_CODE (target) == SSA_NAME)
    ipa_analyze_indirect_call_uses (fbi, call, target);
  else if (virtual_method_call_p (target))
    ipa_analyze_virtual_call_uses (fbi, call, target);
}


/* Analyze the call statement STMT with respect to formal parameters (described
   in INFO) of caller given by FBI->NODE.  Currently it only checks whether
   formal parameters are called.  */

static void
ipa_analyze_stmt_uses (struct ipa_func_body_info *fbi, gimple *stmt)
{
  if (is_gimple_call (stmt))
    ipa_analyze_call_uses (fbi, as_a <gcall *> (stmt));
}

/* Callback of walk_stmt_load_store_addr_ops for the visit_load.
   If OP is a parameter declaration, mark it as used in the info structure
   passed in DATA.  */

static bool
visit_ref_for_mod_analysis (gimple *, tree op, tree, void *data)
{
  class ipa_node_params *info = (class ipa_node_params *) data;

  op = get_base_address (op);
  if (op
      && TREE_CODE (op) == PARM_DECL)
    {
      int index = ipa_get_param_decl_index (info, op);
      gcc_assert (index >= 0);
      ipa_set_param_used (info, index, true);
    }

  return false;
}

/* Scan the statements in BB and inspect the uses of formal parameters.  Store
   the findings in various structures of the associated ipa_node_params
   structure, such as parameter flags, notes etc.  FBI holds various data about
   the function being analyzed.  */

static void
ipa_analyze_params_uses_in_bb (struct ipa_func_body_info *fbi, basic_block bb)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (is_gimple_debug (stmt))
	continue;

      ipa_analyze_stmt_uses (fbi, stmt);
      walk_stmt_load_store_addr_ops (stmt, fbi->info,
				     visit_ref_for_mod_analysis,
				     visit_ref_for_mod_analysis,
				     visit_ref_for_mod_analysis);
    }
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    walk_stmt_load_store_addr_ops (gsi_stmt (gsi), fbi->info,
				   visit_ref_for_mod_analysis,
				   visit_ref_for_mod_analysis,
				   visit_ref_for_mod_analysis);
}

/* Return true EXPR is a load from a dereference of SSA_NAME NAME.  */

static bool
load_from_dereferenced_name (tree expr, tree name)
{
  tree base = get_base_address (expr);
  return (TREE_CODE (base) == MEM_REF
	  && TREE_OPERAND (base, 0) == name);
}

/* Calculate controlled uses of parameters of NODE.  */

static void
ipa_analyze_controlled_uses (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);

  for (int i = 0; i < ipa_get_param_count (info); i++)
    {
      tree parm = ipa_get_param (info, i);
      int call_uses = 0;
      bool load_dereferenced = false;

      /* For SSA regs see if parameter is used.  For non-SSA we compute
	 the flag during modification analysis.  */
      if (is_gimple_reg (parm))
	{
	  tree ddef = ssa_default_def (DECL_STRUCT_FUNCTION (node->decl),
				       parm);
	  if (ddef && !has_zero_uses (ddef))
	    {
	      imm_use_iterator imm_iter;
	      gimple *stmt;

	      ipa_set_param_used (info, i, true);
	      FOR_EACH_IMM_USE_STMT (stmt, imm_iter, ddef)
		{
		  if (is_gimple_debug (stmt))
		    continue;

		  int all_stmt_uses = 0;
		  use_operand_p use_p;
		  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		    all_stmt_uses++;

		  if (is_gimple_call (stmt))
		    {
		      if (gimple_call_internal_p (stmt))
			{
			  call_uses = IPA_UNDESCRIBED_USE;
			  break;
			}
		      int recognized_stmt_uses;
		      if (gimple_call_fn (stmt) == ddef)
			recognized_stmt_uses = 1;
		      else
			recognized_stmt_uses = 0;
		      unsigned arg_count = gimple_call_num_args (stmt);
		      for (unsigned i = 0; i < arg_count; i++)
			{
			  tree arg = gimple_call_arg (stmt, i);
			  if (arg == ddef)
			    recognized_stmt_uses++;
			  else if (load_from_dereferenced_name (arg, ddef))
			    {
			      load_dereferenced = true;
			      recognized_stmt_uses++;
			    }
			}

		      if (recognized_stmt_uses != all_stmt_uses)
			{
			  call_uses = IPA_UNDESCRIBED_USE;
			  break;
			}
		      if (call_uses >= 0)
			call_uses += all_stmt_uses;
		    }
		  else if (gimple_assign_single_p (stmt))
		    {
		      tree rhs = gimple_assign_rhs1 (stmt);
		      if (all_stmt_uses != 1
			  || !load_from_dereferenced_name (rhs, ddef))
			{
			  call_uses = IPA_UNDESCRIBED_USE;
			  break;
			}
		      load_dereferenced = true;
		    }
		  else
		    {
		      call_uses = IPA_UNDESCRIBED_USE;
		      break;
		    }
		}
	    }
	  else
	    call_uses = 0;
	}
      else
	call_uses = IPA_UNDESCRIBED_USE;
      ipa_set_controlled_uses (info, i, call_uses);
      ipa_set_param_load_dereferenced (info, i, load_dereferenced);
    }
}

/* Free stuff in BI.  */

static void
free_ipa_bb_info (struct ipa_bb_info *bi)
{
  bi->cg_edges.release ();
  bi->param_aa_statuses.release ();
}

/* Dominator walker driving the analysis.  */

class analysis_dom_walker : public dom_walker
{
public:
  analysis_dom_walker (struct ipa_func_body_info *fbi)
    : dom_walker (CDI_DOMINATORS), m_fbi (fbi) {}

  edge before_dom_children (basic_block) final override;

private:
  struct ipa_func_body_info *m_fbi;
};

edge
analysis_dom_walker::before_dom_children (basic_block bb)
{
  ipa_analyze_params_uses_in_bb (m_fbi, bb);
  ipa_compute_jump_functions_for_bb (m_fbi, bb);
  return NULL;
}

/* Release body info FBI.  */

void
ipa_release_body_info (struct ipa_func_body_info *fbi)
{
  int i;
  struct ipa_bb_info *bi;

  FOR_EACH_VEC_ELT (fbi->bb_infos, i, bi)
    free_ipa_bb_info (bi);
  fbi->bb_infos.release ();
}

/* Initialize the array describing properties of formal parameters
   of NODE, analyze their uses and compute jump functions associated
   with actual arguments of calls from within NODE.  */

void
ipa_analyze_node (struct cgraph_node *node)
{
  struct ipa_func_body_info fbi;
  class ipa_node_params *info;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  info = ipa_node_params_sum->get_create (node);

  if (info->analysis_done)
    return;
  info->analysis_done = 1;

  if (ipa_func_spec_opts_forbid_analysis_p (node)
      || (count_formal_params (node->decl)
	  >= (1 << IPA_PROP_ARG_INDEX_LIMIT_BITS)))
    {
      gcc_assert (!ipa_get_param_count (info));
      return;
    }

  struct function *func = DECL_STRUCT_FUNCTION (node->decl);
  push_cfun (func);
  calculate_dominance_info (CDI_DOMINATORS);
  ipa_initialize_node_params (node);
  ipa_analyze_controlled_uses (node);

  fbi.node = node;
  fbi.info = info;
  fbi.bb_infos = vNULL;
  fbi.bb_infos.safe_grow_cleared (last_basic_block_for_fn (cfun), true);
  fbi.param_count = ipa_get_param_count (info);
  fbi.aa_walk_budget = opt_for_fn (node->decl, param_ipa_max_aa_steps);

  for (struct cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
    {
      ipa_bb_info *bi = ipa_get_bb_info (&fbi, gimple_bb (cs->call_stmt));
      bi->cg_edges.safe_push (cs);
    }

  for (struct cgraph_edge *cs = node->indirect_calls; cs; cs = cs->next_callee)
    {
      ipa_bb_info *bi = ipa_get_bb_info (&fbi, gimple_bb (cs->call_stmt));
      bi->cg_edges.safe_push (cs);
    }

  enable_ranger (cfun, false);
  analysis_dom_walker (&fbi).walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  disable_ranger (cfun);

  ipa_release_body_info (&fbi);
  free_dominance_info (CDI_DOMINATORS);
  pop_cfun ();
}

/* Update the jump functions associated with call graph edge E when the call
   graph edge CS is being inlined, assuming that E->caller is already (possibly
   indirectly) inlined into CS->callee and that E has not been inlined.  */

static void
update_jump_functions_after_inlining (struct cgraph_edge *cs,
				      struct cgraph_edge *e)
{
  ipa_edge_args *top = ipa_edge_args_sum->get (cs);
  ipa_edge_args *args = ipa_edge_args_sum->get (e);
  if (!args)
    return;
  int count = ipa_get_cs_argument_count (args);
  int i;

  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *dst = ipa_get_ith_jump_func (args, i);
      class ipa_polymorphic_call_context *dst_ctx
	= ipa_get_ith_polymorhic_call_context (args, i);

      if (dst->agg.items)
	{
	  struct ipa_agg_jf_item *item;
	  int j;

	  FOR_EACH_VEC_ELT (*dst->agg.items, j, item)
	    {
	      int dst_fid;
	      struct ipa_jump_func *src;

	      if (item->jftype != IPA_JF_PASS_THROUGH
		  && item->jftype != IPA_JF_LOAD_AGG)
		continue;

	      dst_fid = item->value.pass_through.formal_id;
	      if (!top || dst_fid >= ipa_get_cs_argument_count (top))
		{
		  item->jftype = IPA_JF_UNKNOWN;
		  continue;
		}

	      item->value.pass_through.formal_id = -1;
	      src = ipa_get_ith_jump_func (top, dst_fid);
	      if (src->type == IPA_JF_CONST)
		{
		  if (item->jftype == IPA_JF_PASS_THROUGH
		      && item->value.pass_through.operation == NOP_EXPR)
		    {
		      item->jftype = IPA_JF_CONST;
		      item->value.constant = src->value.constant.value;
		      continue;
		    }
		}
	      else if (src->type == IPA_JF_PASS_THROUGH
		       && src->value.pass_through.operation == NOP_EXPR)
		{
		  if (item->jftype == IPA_JF_PASS_THROUGH
		      || !item->value.load_agg.by_ref
		      || src->value.pass_through.agg_preserved)
		    item->value.pass_through.formal_id
				= src->value.pass_through.formal_id;
		}
	      else if (src->type == IPA_JF_ANCESTOR)
		{
		  if (item->jftype == IPA_JF_PASS_THROUGH)
		    {
		      if (!src->value.ancestor.offset)
			item->value.pass_through.formal_id
				= src->value.ancestor.formal_id;
		    }
		  else if (src->value.ancestor.agg_preserved)
		    {
		      gcc_checking_assert (item->value.load_agg.by_ref);

		      item->value.pass_through.formal_id
				 = src->value.ancestor.formal_id;
		      item->value.load_agg.offset
				+= src->value.ancestor.offset;
		    }
		}

	      if (item->value.pass_through.formal_id < 0)
		item->jftype = IPA_JF_UNKNOWN;
	    }
	}

      if (!top)
	{
	  ipa_set_jf_unknown (dst);
	  continue;
	}

      if (dst->type == IPA_JF_ANCESTOR)
	{
	  struct ipa_jump_func *src;
	  int dst_fid = dst->value.ancestor.formal_id;
	  class ipa_polymorphic_call_context *src_ctx
	    = ipa_get_ith_polymorhic_call_context (top, dst_fid);

	  /* Variable number of arguments can cause havoc if we try to access
	     one that does not exist in the inlined edge.  So make sure we
	     don't.  */
	  if (dst_fid >= ipa_get_cs_argument_count (top))
	    {
	      ipa_set_jf_unknown (dst);
	      continue;
	    }

	  src = ipa_get_ith_jump_func (top, dst_fid);

	  if (src_ctx && !src_ctx->useless_p ())
	    {
	      class ipa_polymorphic_call_context ctx = *src_ctx;

	      /* TODO: Make type preserved safe WRT contexts.  */
	      if (!ipa_get_jf_ancestor_type_preserved (dst))
		ctx.possible_dynamic_type_change (e->in_polymorphic_cdtor);
	      ctx.offset_by (dst->value.ancestor.offset);
	      if (!ctx.useless_p ())
		{
		  if (!dst_ctx)
		    {
		      vec_safe_grow_cleared (args->polymorphic_call_contexts,
					     count, true);
		      dst_ctx = ipa_get_ith_polymorhic_call_context (args, i);
		    }

		  dst_ctx->combine_with (ctx);
		}
	    }

	  /* Parameter and argument in ancestor jump function must be pointer
	     type, which means access to aggregate must be by-reference.  */
	  gcc_assert (!src->agg.items || src->agg.by_ref);

	  if (src->agg.items && dst->value.ancestor.agg_preserved)
	    {
	      struct ipa_agg_jf_item *item;
	      int j;

	      /* Currently we do not produce clobber aggregate jump functions,
		 replace with merging when we do.  */
	      gcc_assert (!dst->agg.items);

	      dst->agg.items = vec_safe_copy (src->agg.items);
	      dst->agg.by_ref = src->agg.by_ref;
	      FOR_EACH_VEC_SAFE_ELT (dst->agg.items, j, item)
		item->offset -= dst->value.ancestor.offset;
	    }

	  if (src->type == IPA_JF_PASS_THROUGH
	      && src->value.pass_through.operation == NOP_EXPR)
	    {
	      dst->value.ancestor.formal_id = src->value.pass_through.formal_id;
	      dst->value.ancestor.agg_preserved &=
		src->value.pass_through.agg_preserved;
	    }
	  else if (src->type == IPA_JF_ANCESTOR)
	    {
	      dst->value.ancestor.formal_id = src->value.ancestor.formal_id;
	      dst->value.ancestor.offset += src->value.ancestor.offset;
	      dst->value.ancestor.agg_preserved &=
		src->value.ancestor.agg_preserved;
	      dst->value.ancestor.keep_null |= src->value.ancestor.keep_null;
	    }
	  else
	    ipa_set_jf_unknown (dst);
	}
      else if (dst->type == IPA_JF_PASS_THROUGH)
	{
	  struct ipa_jump_func *src;
	  /* We must check range due to calls with variable number of arguments
	     and we cannot combine jump functions with operations.  */
	  if (dst->value.pass_through.operation == NOP_EXPR
	      && (top && dst->value.pass_through.formal_id
		  < ipa_get_cs_argument_count (top)))
	    {
	      int dst_fid = dst->value.pass_through.formal_id;
	      src = ipa_get_ith_jump_func (top, dst_fid);
	      bool dst_agg_p = ipa_get_jf_pass_through_agg_preserved (dst);
	      class ipa_polymorphic_call_context *src_ctx
		= ipa_get_ith_polymorhic_call_context (top, dst_fid);

	      if (src_ctx && !src_ctx->useless_p ())
		{
		  class ipa_polymorphic_call_context ctx = *src_ctx;

		  /* TODO: Make type preserved safe WRT contexts.  */
		  if (!ipa_get_jf_pass_through_type_preserved (dst))
		    ctx.possible_dynamic_type_change (e->in_polymorphic_cdtor);
		  if (!ctx.useless_p ())
		    {
		      if (!dst_ctx)
			{
			  vec_safe_grow_cleared (args->polymorphic_call_contexts,
						 count, true);
			  dst_ctx = ipa_get_ith_polymorhic_call_context (args, i);
			}
		      dst_ctx->combine_with (ctx);
		    }
		}
	      switch (src->type)
		{
		case IPA_JF_UNKNOWN:
		  ipa_set_jf_unknown (dst);
		  break;
		case IPA_JF_CONST:
		  {
		    bool rd = ipa_get_jf_pass_through_refdesc_decremented (dst);
		    ipa_set_jf_cst_copy (dst, src);
		    if (rd)
		      ipa_zap_jf_refdesc (dst);
		  }

		  break;

		case IPA_JF_PASS_THROUGH:
		  {
		    int formal_id = ipa_get_jf_pass_through_formal_id (src);
		    enum tree_code operation;
		    operation = ipa_get_jf_pass_through_operation (src);

		    if (operation == NOP_EXPR)
		      {
			bool agg_p;
			agg_p = dst_agg_p
			  && ipa_get_jf_pass_through_agg_preserved (src);
			ipa_set_jf_simple_pass_through (dst, formal_id, agg_p);
		      }
		    else if (TREE_CODE_CLASS (operation) == tcc_unary)
		      {
			tree op_t = ipa_get_jf_pass_through_op_type (src);
			ipa_set_jf_unary_pass_through (dst, formal_id, operation,
						       op_t);
		      }
		    else
		      {
			tree operand = ipa_get_jf_pass_through_operand (src);
			tree op_t = ipa_get_jf_pass_through_op_type (src);
			ipa_set_jf_arith_pass_through (dst, formal_id, operand,
						       operation, op_t);
		      }
		    break;
		  }
		case IPA_JF_ANCESTOR:
		  {
		    bool agg_p;
		    agg_p = dst_agg_p
		      && ipa_get_jf_ancestor_agg_preserved (src);
		    ipa_set_ancestor_jf (dst,
					 ipa_get_jf_ancestor_offset (src),
					 ipa_get_jf_ancestor_formal_id (src),
					 agg_p,
					 ipa_get_jf_ancestor_keep_null (src));
		    break;
		  }
		default:
		  gcc_unreachable ();
		}

	      if (src->m_vr && src->m_vr->known_p ())
		{
		  value_range svr (src->m_vr->type ());
		  if (!dst->m_vr || !dst->m_vr->known_p ())
		    ipa_set_jfunc_vr (dst, *src->m_vr);
		  else if (ipa_vr_operation_and_type_effects (svr, *src->m_vr,
							   NOP_EXPR,
							   dst->m_vr->type (),
							   src->m_vr->type ()))
		    {
		      value_range dvr;
		      dst->m_vr->get_vrange (dvr);
		      dvr.intersect (svr);
		      if (!dvr.undefined_p ())
			ipa_set_jfunc_vr (dst, dvr);
		    }
		}

	      if (src->agg.items
		  && (dst_agg_p || !src->agg.by_ref))
		{
		  /* Currently we do not produce clobber aggregate jump
		     functions, replace with merging when we do.  */
		  gcc_assert (!dst->agg.items);

		  dst->agg.by_ref = src->agg.by_ref;
		  dst->agg.items = vec_safe_copy (src->agg.items);
		}
	    }
	  else
	    ipa_set_jf_unknown (dst);
	}
    }
}

/* If TARGET is an addr_expr of a function declaration, make it the
   (SPECULATIVE)destination of an indirect edge IE and return the edge.
   Otherwise, return NULL.  */

struct cgraph_edge *
ipa_make_edge_direct_to_target (struct cgraph_edge *ie, tree target,
				bool speculative)
{
  struct cgraph_node *callee;
  bool unreachable = false;

  if (TREE_CODE (target) == ADDR_EXPR)
    target = TREE_OPERAND (target, 0);
  if (TREE_CODE (target) != FUNCTION_DECL)
    {
      target = canonicalize_constructor_val (target, NULL);
      if (!target || TREE_CODE (target) != FUNCTION_DECL)
	{
	  /* Member pointer call that goes through a VMT lookup.  */
	  if (ie->indirect_info->member_ptr
	      /* Or if target is not an invariant expression and we do not
		 know if it will evaulate to function at runtime.
		 This can happen when folding through &VAR, where &VAR
		 is IP invariant, but VAR itself is not.

		 TODO: Revisit this when GCC 5 is branched.  It seems that
		 member_ptr check is not needed and that we may try to fold
		 the expression and see if VAR is readonly.  */
	      || !is_gimple_ip_invariant (target))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, ie->call_stmt,
				   "discovered direct call non-invariant %s\n",
				   ie->caller->dump_name ());
		}
	      return NULL;
	    }


          if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, ie->call_stmt,
			       "discovered direct call to non-function in %s, "
			       "making it __builtin_unreachable\n",
			       ie->caller->dump_name ());
	    }

	  target = builtin_decl_unreachable ();
	  callee = cgraph_node::get_create (target);
	  unreachable = true;
	}
      else
	callee = cgraph_node::get (target);
    }
  else
    callee = cgraph_node::get (target);

  /* Because may-edges are not explicitely represented and vtable may be external,
     we may create the first reference to the object in the unit.  */
  if (!callee || callee->inlined_to)
    {

      /* We are better to ensure we can refer to it.
	 In the case of static functions we are out of luck, since we already
	 removed its body.  In the case of public functions we may or may
	 not introduce the reference.  */
      if (!canonicalize_constructor_val (target, NULL)
	  || !TREE_PUBLIC (target))
	{
	  if (dump_file)
	    fprintf (dump_file, "ipa-prop: Discovered call to a known target "
		     "(%s -> %s) but cannot refer to it.  Giving up.\n",
		     ie->caller->dump_name (),
		     ie->callee->dump_name ());
	  return NULL;
	}
      callee = cgraph_node::get_create (target);
    }

  /* If the edge is already speculated.  */
  if (speculative && ie->speculative)
    {
      if (dump_file)
	{
	  cgraph_edge *e2 = ie->speculative_call_for_target (callee);
	  if (!e2)
	    {
	      if (dump_file)
		fprintf (dump_file, "ipa-prop: Discovered call to a "
			 "speculative target (%s -> %s) but the call is "
			 "already speculated to different target.  "
			 "Giving up.\n",
			 ie->caller->dump_name (), callee->dump_name ());
	    }
	  else
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "ipa-prop: Discovered call to a speculative target "
			 "(%s -> %s) this agree with previous speculation.\n",
			 ie->caller->dump_name (), callee->dump_name ());
	    }
	}
      return NULL;
    }

  if (!dbg_cnt (devirt))
    return NULL;

  ipa_check_create_node_params ();

  /* We cannot make edges to inline clones.  It is bug that someone removed
     the cgraph node too early.  */
  gcc_assert (!callee->inlined_to);

  if (dump_file && !unreachable)
    {
      fprintf (dump_file, "ipa-prop: Discovered %s call to a %s target "
	       "(%s -> %s), for stmt ",
	       ie->indirect_info->polymorphic ? "a virtual" : "an indirect",
	       speculative ? "speculative" : "known",
	       ie->caller->dump_name (),
	       callee->dump_name ());
      if (ie->call_stmt)
	print_gimple_stmt (dump_file, ie->call_stmt, 2, TDF_SLIM);
      else
	fprintf (dump_file, "with uid %i\n", ie->lto_stmt_uid);
     }
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, ie->call_stmt,
		       "converting indirect call in %s to direct call to %s\n",
		       ie->caller->dump_name (), callee->dump_name ());
    }
  if (!speculative)
    {
      struct cgraph_edge *orig = ie;
      ie = cgraph_edge::make_direct (ie, callee);
      /* If we resolved speculative edge the cost is already up to date
	 for direct call (adjusted by inline_edge_duplication_hook).  */
      if (ie == orig)
	{
	  ipa_call_summary *es = ipa_call_summaries->get (ie);
	  es->call_stmt_size -= (eni_size_weights.indirect_call_cost
				 - eni_size_weights.call_cost);
	  es->call_stmt_time -= (eni_time_weights.indirect_call_cost
				 - eni_time_weights.call_cost);
	}
    }
  else
    {
      if (!callee->can_be_discarded_p ())
	{
	  cgraph_node *alias;
	  alias = dyn_cast<cgraph_node *> (callee->noninterposable_alias ());
	  if (alias)
	    callee = alias;
	}
      /* make_speculative will update ie's cost to direct call cost. */
      ie = ie->make_speculative
	     (callee, ie->count.apply_scale (8, 10));
    }

  return ie;
}

/* Attempt to locate an interprocedural constant at a given REQ_OFFSET in
   CONSTRUCTOR and return it.  Return NULL if the search fails for some
   reason.  */

static tree
find_constructor_constant_at_offset (tree constructor, HOST_WIDE_INT req_offset)
{
  tree type = TREE_TYPE (constructor);
  if (TREE_CODE (type) != ARRAY_TYPE
      && TREE_CODE (type) != RECORD_TYPE)
    return NULL;

  unsigned ix;
  tree index, val;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (constructor), ix, index, val)
    {
      HOST_WIDE_INT elt_offset;
      if (TREE_CODE (type) == ARRAY_TYPE)
       {
         offset_int off;
         tree unit_size = TYPE_SIZE_UNIT (TREE_TYPE (type));
         gcc_assert (TREE_CODE (unit_size) == INTEGER_CST);

         if (index)
           {
	     if (TREE_CODE (index) == RANGE_EXPR)
	       off = wi::to_offset (TREE_OPERAND (index, 0));
	     else
	       off = wi::to_offset (index);
             if (TYPE_DOMAIN (type) && TYPE_MIN_VALUE (TYPE_DOMAIN (type)))
               {
                 tree low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
                 gcc_assert (TREE_CODE (unit_size) == INTEGER_CST);
                 off = wi::sext (off - wi::to_offset (low_bound),
                                 TYPE_PRECISION (TREE_TYPE (index)));
               }
             off *= wi::to_offset (unit_size);
	     /* ???  Handle more than just the first index of a
	        RANGE_EXPR.  */
           }
         else
           off = wi::to_offset (unit_size) * ix;

         off = wi::lshift (off, LOG2_BITS_PER_UNIT);
         if (!wi::fits_shwi_p (off) || wi::neg_p (off))
           continue;
         elt_offset = off.to_shwi ();
       }
      else if (TREE_CODE (type) == RECORD_TYPE)
       {
         gcc_checking_assert (index && TREE_CODE (index) == FIELD_DECL);
         if (DECL_BIT_FIELD (index))
           continue;
         elt_offset = int_bit_position (index);
       }
      else
       gcc_unreachable ();

      if (elt_offset > req_offset)
	return NULL;

      if (TREE_CODE (val) == CONSTRUCTOR)
	return find_constructor_constant_at_offset (val,
						    req_offset - elt_offset);

      if (elt_offset == req_offset
	  && is_gimple_reg_type (TREE_TYPE (val))
	  && is_gimple_ip_invariant (val))
	return val;
    }
  return NULL;
}

/* Check whether SCALAR could be used to look up an aggregate interprocedural
   invariant from a static constructor and if so, return it.  Otherwise return
   NULL. */

tree
ipa_find_agg_cst_from_init (tree scalar, HOST_WIDE_INT offset, bool by_ref)
{
  if (by_ref)
    {
      if (TREE_CODE (scalar) != ADDR_EXPR)
	return NULL;
      scalar = TREE_OPERAND (scalar, 0);
    }

  if (!VAR_P (scalar)
      || !is_global_var (scalar)
      || !TREE_READONLY (scalar)
      || !DECL_INITIAL (scalar)
      || TREE_CODE (DECL_INITIAL (scalar)) != CONSTRUCTOR)
    return NULL;

  return find_constructor_constant_at_offset (DECL_INITIAL (scalar), offset);
}

/* Retrieve value from AGG_JFUNC for the given OFFSET or return NULL if there
   is none.  BY_REF specifies whether the value has to be passed by reference
   or by value.  */

static tree
ipa_find_agg_cst_from_jfunc_items (struct ipa_agg_jump_function *agg_jfunc,
				   ipa_node_params *src_info,
				   cgraph_node *src_node,
				   HOST_WIDE_INT offset, bool by_ref)
{
  if (by_ref != agg_jfunc->by_ref)
    return NULL_TREE;

  for (const ipa_agg_jf_item &item : agg_jfunc->items)
    if (item.offset == offset)
      return ipa_agg_value_from_jfunc (src_info, src_node, &item);

  return NULL_TREE;
}

/* Remove a reference to SYMBOL from the list of references of a node given by
   reference description RDESC.  Return true if the reference has been
   successfully found and removed.  */

static bool
remove_described_reference (symtab_node *symbol, struct ipa_cst_ref_desc *rdesc)
{
  struct ipa_ref *to_del;
  struct cgraph_edge *origin;

  origin = rdesc->cs;
  if (!origin)
    return false;
  to_del = origin->caller->find_reference (symbol, origin->call_stmt,
					   origin->lto_stmt_uid, IPA_REF_ADDR);
  if (!to_del)
    return false;

  to_del->remove_reference ();
  if (dump_file)
    fprintf (dump_file, "ipa-prop: Removed a reference from %s to %s.\n",
	     origin->caller->dump_name (), symbol->dump_name ());
  return true;
}

/* If JFUNC has a reference description with refcount different from
   IPA_UNDESCRIBED_USE, return the reference description, otherwise return
   NULL.  JFUNC must be a constant jump function.  */

static struct ipa_cst_ref_desc *
jfunc_rdesc_usable (struct ipa_jump_func *jfunc)
{
  struct ipa_cst_ref_desc *rdesc = ipa_get_jf_constant_rdesc (jfunc);
  if (rdesc && rdesc->refcount != IPA_UNDESCRIBED_USE)
    return rdesc;
  else
    return NULL;
}

/* If the value of constant jump function JFUNC is an address of a function
   declaration, return the associated call graph node.  Otherwise return
   NULL.  */

static symtab_node *
symtab_node_for_jfunc (struct ipa_jump_func *jfunc)
{
  gcc_checking_assert (jfunc->type == IPA_JF_CONST);
  tree cst = ipa_get_jf_constant (jfunc);
  if (TREE_CODE (cst) != ADDR_EXPR
      || (TREE_CODE (TREE_OPERAND (cst, 0)) != FUNCTION_DECL
	  && TREE_CODE (TREE_OPERAND (cst, 0)) != VAR_DECL))
    return NULL;

  return symtab_node::get (TREE_OPERAND (cst, 0));
}


/* If JFUNC is a constant jump function with a usable rdesc, decrement its
   refcount and if it hits zero, remove reference to SYMBOL from the caller of
   the edge specified in the rdesc.  Return false if either the symbol or the
   reference could not be found, otherwise return true.  */

static bool
try_decrement_rdesc_refcount (struct ipa_jump_func *jfunc)
{
  struct ipa_cst_ref_desc *rdesc;
  if (jfunc->type == IPA_JF_CONST
      && (rdesc = jfunc_rdesc_usable (jfunc))
      && --rdesc->refcount == 0)
    {
      symtab_node *symbol = symtab_node_for_jfunc (jfunc);
      if (!symbol)
	return false;

      return remove_described_reference (symbol, rdesc);
    }
  return true;
}

/* Try to find a destination for indirect edge IE that corresponds to a simple
   call or a call of a member function pointer and where the destination is a
   pointer formal parameter described by jump function JFUNC.  TARGET_TYPE is
   the type of the parameter to which the result of JFUNC is passed.  If it can
   be determined, return the newly direct edge, otherwise return NULL.
   NEW_ROOT and NEW_ROOT_INFO is the node and its info that JFUNC lattices are
   relative to.  */

static struct cgraph_edge *
try_make_edge_direct_simple_call (struct cgraph_edge *ie,
				  struct ipa_jump_func *jfunc, tree target_type,
				  struct cgraph_node *new_root,
				  class ipa_node_params *new_root_info)
{
  struct cgraph_edge *cs;
  tree target = NULL_TREE;
  bool agg_contents = ie->indirect_info->agg_contents;
  tree scalar = ipa_value_from_jfunc (new_root_info, jfunc, target_type);
  if (agg_contents)
    {
      if (scalar)
	target = ipa_find_agg_cst_from_init (scalar, ie->indirect_info->offset,
					     ie->indirect_info->by_ref);
      if (!target && ie->indirect_info->guaranteed_unmodified)
	target = ipa_find_agg_cst_from_jfunc_items (&jfunc->agg, new_root_info,
						    new_root,
						    ie->indirect_info->offset,
						    ie->indirect_info->by_ref);
    }
  else
    target = scalar;
  if (!target)
    return NULL;
  cs = ipa_make_edge_direct_to_target (ie, target);

  if (cs && !agg_contents)
    {
      bool ok;
      gcc_checking_assert (cs->callee
			   && (cs != ie
			       || jfunc->type != IPA_JF_CONST
			       || !symtab_node_for_jfunc (jfunc)
			       || cs->callee == symtab_node_for_jfunc (jfunc)));
      ok = try_decrement_rdesc_refcount (jfunc);
      gcc_checking_assert (ok);
    }

  return cs;
}

/* Return the target to be used in cases of impossible devirtualization.  IE
   and target (the latter can be NULL) are dumped when dumping is enabled.  */

tree
ipa_impossible_devirt_target (struct cgraph_edge *ie, tree target)
{
  if (dump_file)
    {
      if (target)
	fprintf (dump_file,
		 "Type inconsistent devirtualization: %s->%s\n",
		 ie->caller->dump_name (),
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (target)));
      else
	fprintf (dump_file,
		 "No devirtualization target in %s\n",
		 ie->caller->dump_name ());
    }
  tree new_target = builtin_decl_unreachable ();
  cgraph_node::get_create (new_target);
  return new_target;
}

/* Try to find a destination for indirect edge IE that corresponds to a virtual
   call based on a formal parameter which is described by jump function JFUNC
   and if it can be determined, make it direct and return the direct edge.
   Otherwise, return NULL.  CTX describes the polymorphic context that the
   parameter the call is based on brings along with it.  NEW_ROOT and
   NEW_ROOT_INFO is the node and its info that JFUNC lattices are relative
   to.  */

static struct cgraph_edge *
try_make_edge_direct_virtual_call (struct cgraph_edge *ie,
				   struct ipa_jump_func *jfunc,
				   class ipa_polymorphic_call_context ctx,
				   struct cgraph_node *new_root,
				   class ipa_node_params *new_root_info)
{
  tree target = NULL;
  bool speculative = false;

  if (!opt_for_fn (ie->caller->decl, flag_devirtualize))
    return NULL;

  gcc_assert (!ie->indirect_info->by_ref);

  /* Try to do lookup via known virtual table pointer value.  */
  if (!ie->indirect_info->vptr_changed
      || opt_for_fn (ie->caller->decl, flag_devirtualize_speculatively))
    {
      tree vtable;
      unsigned HOST_WIDE_INT offset;
      tree t = NULL_TREE;
      if (jfunc->type == IPA_JF_CONST)
	t = ipa_find_agg_cst_from_init (ipa_get_jf_constant (jfunc),
					ie->indirect_info->offset, true);
      if (!t)
	t = ipa_find_agg_cst_from_jfunc_items (&jfunc->agg, new_root_info,
					       new_root,
					       ie->indirect_info->offset, true);
      if (t && vtable_pointer_value_to_vtable (t, &vtable, &offset))
	{
	  bool can_refer;
	  t = gimple_get_virt_method_for_vtable (ie->indirect_info->otr_token,
						 vtable, offset, &can_refer);
	  if (can_refer)
	    {
	      if (!t
		  || fndecl_built_in_p (t, BUILT_IN_UNREACHABLE,
					   BUILT_IN_UNREACHABLE_TRAP)
		  || !possible_polymorphic_call_target_p
		       (ie, cgraph_node::get (t)))
		{
		  /* Do not speculate builtin_unreachable, it is stupid!  */
		  if (!ie->indirect_info->vptr_changed)
		    target = ipa_impossible_devirt_target (ie, target);
		  else
		    target = NULL;
		}
	      else
		{
		  target = t;
		  speculative = ie->indirect_info->vptr_changed;
		}
	    }
	}
    }

  ipa_polymorphic_call_context ie_context (ie);
  vec <cgraph_node *>targets;
  bool final;

  ctx.offset_by (ie->indirect_info->offset);
  if (ie->indirect_info->vptr_changed)
    ctx.possible_dynamic_type_change (ie->in_polymorphic_cdtor,
				      ie->indirect_info->otr_type);
  ctx.combine_with (ie_context, ie->indirect_info->otr_type);
  targets = possible_polymorphic_call_targets
    (ie->indirect_info->otr_type,
     ie->indirect_info->otr_token,
     ctx, &final);
  if (final && targets.length () <= 1)
    {
      speculative = false;
      if (targets.length () == 1)
	target = targets[0]->decl;
      else
	target = ipa_impossible_devirt_target (ie, NULL_TREE);
    }
  else if (!target && opt_for_fn (ie->caller->decl, flag_devirtualize_speculatively)
	   && !ie->speculative && ie->maybe_hot_p ())
    {
      cgraph_node *n;
      n = try_speculative_devirtualization (ie->indirect_info->otr_type,
					    ie->indirect_info->otr_token,
					    ie->indirect_info->context);
      if (n)
	{
	  target = n->decl;
	  speculative = true;
	}
    }

  if (target)
    {
      if (!possible_polymorphic_call_target_p
	  (ie, cgraph_node::get_create (target)))
	{
	  if (speculative)
	    return NULL;
	  target = ipa_impossible_devirt_target (ie, target);
	}
      return ipa_make_edge_direct_to_target (ie, target, speculative);
    }
  else
    return NULL;
}

/* Update the param called notes associated with NODE when CS is being inlined,
   assuming NODE is (potentially indirectly) inlined into CS->callee.
   Moreover, if the callee is discovered to be constant, create a new cgraph
   edge for it.  Newly discovered indirect edges will be added to *NEW_EDGES,
   unless NEW_EDGES is NULL.  Return true iff a new edge(s) were created.  */

static bool
update_indirect_edges_after_inlining (struct cgraph_edge *cs,
				      struct cgraph_node *node,
				      vec<cgraph_edge *> *new_edges)
{
  class ipa_edge_args *top;
  struct cgraph_edge *ie, *next_ie, *new_direct_edge;
  struct cgraph_node *new_root;
  class ipa_node_params *new_root_info, *inlined_node_info;
  bool res = false;

  ipa_check_create_edge_args ();
  top = ipa_edge_args_sum->get (cs);
  new_root = cs->caller->inlined_to
		? cs->caller->inlined_to : cs->caller;
  new_root_info = ipa_node_params_sum->get (new_root);
  inlined_node_info = ipa_node_params_sum->get (cs->callee->function_symbol ());

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      class cgraph_indirect_call_info *ici = ie->indirect_info;
      struct ipa_jump_func *jfunc;
      int param_index;

      next_ie = ie->next_callee;

      if (ici->param_index == -1)
	continue;

      /* We must check range due to calls with variable number of arguments:  */
      if (!top || ici->param_index >= ipa_get_cs_argument_count (top))
	{
	  ici->param_index = -1;
	  continue;
	}

      param_index = ici->param_index;
      jfunc = ipa_get_ith_jump_func (top, param_index);

      auto_vec<cgraph_node *, 4> spec_targets;
      if (ie->speculative)
	for (cgraph_edge *direct = ie->first_speculative_call_target ();
	     direct;
	     direct = direct->next_speculative_call_target ())
	  spec_targets.safe_push (direct->callee);

      if (!opt_for_fn (node->decl, flag_indirect_inlining))
	new_direct_edge = NULL;
      else if (ici->polymorphic)
	{
          ipa_polymorphic_call_context ctx;
	  ctx = ipa_context_from_jfunc (new_root_info, cs, param_index, jfunc);
	  new_direct_edge = try_make_edge_direct_virtual_call (ie, jfunc, ctx,
							       new_root,
							       new_root_info);
	}
      else
	{
	  tree target_type =  ipa_get_type (inlined_node_info, param_index);
	  new_direct_edge = try_make_edge_direct_simple_call (ie, jfunc,
							      target_type,
							      new_root,
							      new_root_info);
	}

      /* If speculation was removed, then we need to do nothing.  */
      if (new_direct_edge && new_direct_edge != ie
	  && spec_targets.contains (new_direct_edge->callee))
	{
	  new_direct_edge->indirect_inlining_edge = 1;
	  res = true;
	  if (!new_direct_edge->speculative)
	    continue;
	}
      else if (new_direct_edge)
	{
	  new_direct_edge->indirect_inlining_edge = 1;
	  if (new_edges)
	    {
	      new_edges->safe_push (new_direct_edge);
	      res = true;
	    }
	  /* If speculative edge was introduced we still need to update
	     call info of the indirect edge.  */
	  if (!new_direct_edge->speculative)
	    continue;
	}
      if (jfunc->type == IPA_JF_PASS_THROUGH
          && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
	{
	  if (ici->agg_contents
	      && !ipa_get_jf_pass_through_agg_preserved (jfunc)
	      && !ici->polymorphic)
	    ici->param_index = -1;
	  else
	    {
	      ici->param_index = ipa_get_jf_pass_through_formal_id (jfunc);
	      if (ici->polymorphic
		  && !ipa_get_jf_pass_through_type_preserved (jfunc))
		ici->vptr_changed = true;
	      ipa_set_param_used_by_indirect_call (new_root_info,
			     			   ici->param_index, true);
	      if (ici->polymorphic)
		ipa_set_param_used_by_polymorphic_call (new_root_info,
						        ici->param_index, true);
	    }
	}
      else if (jfunc->type == IPA_JF_ANCESTOR)
	{
	  if (ici->agg_contents
	      && !ipa_get_jf_ancestor_agg_preserved (jfunc)
	      && !ici->polymorphic)
	    ici->param_index = -1;
	  else
	    {
	      ici->param_index = ipa_get_jf_ancestor_formal_id (jfunc);
	      ici->offset += ipa_get_jf_ancestor_offset (jfunc);
	      if (ici->polymorphic
		  && !ipa_get_jf_ancestor_type_preserved (jfunc))
		ici->vptr_changed = true;
	      ipa_set_param_used_by_indirect_call (new_root_info,
			     			   ici->param_index, true);
	      if (ici->polymorphic)
		ipa_set_param_used_by_polymorphic_call (new_root_info,
						        ici->param_index, true);
	    }
	}
      else
	/* Either we can find a destination for this edge now or never. */
	ici->param_index = -1;
    }

  return res;
}

/* Recursively traverse subtree of NODE (including node) made of inlined
   cgraph_edges when CS has been inlined and invoke
   update_indirect_edges_after_inlining on all nodes and
   update_jump_functions_after_inlining on all non-inlined edges that lead out
   of this subtree.  Newly discovered indirect edges will be added to
   *NEW_EDGES, unless NEW_EDGES is NULL.  Return true iff a new edge(s) were
   created.  */

static bool
propagate_info_to_inlined_callees (struct cgraph_edge *cs,
				   struct cgraph_node *node,
				   vec<cgraph_edge *> *new_edges)
{
  struct cgraph_edge *e;
  bool res;

  res = update_indirect_edges_after_inlining (cs, node, new_edges);

  for (e = node->callees; e; e = e->next_callee)
    if (!e->inline_failed)
      res |= propagate_info_to_inlined_callees (cs, e->callee, new_edges);
    else
      update_jump_functions_after_inlining (cs, e);
  for (e = node->indirect_calls; e; e = e->next_callee)
    update_jump_functions_after_inlining (cs, e);

  return res;
}

/* Combine two controlled uses counts as done during inlining.  */

static int
combine_controlled_uses_counters (int c, int d)
{
  if (c == IPA_UNDESCRIBED_USE || d == IPA_UNDESCRIBED_USE)
    return IPA_UNDESCRIBED_USE;
  else
    return c + d - 1;
}

/* Propagate number of controlled users from CS->caleee to the new root of the
   tree of inlined nodes.  */

static void
propagate_controlled_uses (struct cgraph_edge *cs)
{
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  if (!args)
    return;
  struct cgraph_node *new_root = cs->caller->inlined_to
    ? cs->caller->inlined_to : cs->caller;
  ipa_node_params *new_root_info = ipa_node_params_sum->get (new_root);
  ipa_node_params *old_root_info = ipa_node_params_sum->get (cs->callee);
  int count, i;

  if (!old_root_info)
    return;

  count = MIN (ipa_get_cs_argument_count (args),
	       ipa_get_param_count (old_root_info));
  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);
      struct ipa_cst_ref_desc *rdesc;

      if (jf->type == IPA_JF_PASS_THROUGH
	  && !ipa_get_jf_pass_through_refdesc_decremented (jf))
	{
	  int src_idx, c, d;
	  src_idx = ipa_get_jf_pass_through_formal_id (jf);
	  c = ipa_get_controlled_uses (new_root_info, src_idx);
	  d = ipa_get_controlled_uses (old_root_info, i);

	  gcc_checking_assert (ipa_get_jf_pass_through_operation (jf)
			       == NOP_EXPR || c == IPA_UNDESCRIBED_USE);
	  c = combine_controlled_uses_counters (c, d);
	  ipa_set_controlled_uses (new_root_info, src_idx, c);
	  bool lderef = true;
	  if (c != IPA_UNDESCRIBED_USE)
	    {
	      lderef = (ipa_get_param_load_dereferenced (new_root_info, src_idx)
			|| ipa_get_param_load_dereferenced (old_root_info, i));
	      ipa_set_param_load_dereferenced (new_root_info, src_idx, lderef);
	    }

	  if (c == 0 && !lderef && new_root_info->ipcp_orig_node)
	    {
	      struct cgraph_node *n;
	      struct ipa_ref *ref;
	      tree t = new_root_info->known_csts[src_idx];

	      if (t && TREE_CODE (t) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
		  && (n = cgraph_node::get (TREE_OPERAND (t, 0)))
		  && (ref = new_root->find_reference (n, NULL, 0,
						      IPA_REF_ADDR)))
		{
		  if (dump_file)
		    fprintf (dump_file, "ipa-prop: Removing cloning-created "
			     "reference from %s to %s.\n",
			     new_root->dump_name (),
			     n->dump_name ());
		  ref->remove_reference ();
		}
	    }
	}
      else if (jf->type == IPA_JF_CONST
	       && (rdesc = jfunc_rdesc_usable (jf)))
	{
	  int d = ipa_get_controlled_uses (old_root_info, i);
	  int c = rdesc->refcount;
	  tree cst = ipa_get_jf_constant (jf);
	  rdesc->refcount = combine_controlled_uses_counters (c, d);
	  if (rdesc->refcount != IPA_UNDESCRIBED_USE
	      && ipa_get_param_load_dereferenced (old_root_info, i)
	      && TREE_CODE (cst) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (cst, 0)))
	    {
	      symtab_node *n = symtab_node::get (TREE_OPERAND (cst, 0));
	      new_root->create_reference (n, IPA_REF_LOAD, NULL);
	      if (dump_file)
		fprintf (dump_file, "ipa-prop: Address IPA constant will reach "
			 "a load so adding LOAD reference from %s to %s.\n",
			 new_root->dump_name (), n->dump_name ());
	    }
	  if (rdesc->refcount == 0)
	    {
	      gcc_checking_assert (TREE_CODE (cst) == ADDR_EXPR
				   && ((TREE_CODE (TREE_OPERAND (cst, 0))
					== FUNCTION_DECL)
				       || VAR_P (TREE_OPERAND (cst, 0))));

	      symtab_node *n = symtab_node::get (TREE_OPERAND (cst, 0));
	      if (n)
		{
		  remove_described_reference (n, rdesc);
		  cgraph_node *clone = cs->caller;
		  while (clone->inlined_to
			 && clone->ipcp_clone
			 && clone != rdesc->cs->caller)
		    {
		      struct ipa_ref *ref;
		      ref = clone->find_reference (n, NULL, 0, IPA_REF_ADDR);
		      if (ref)
			{
			  if (dump_file)
			    fprintf (dump_file, "ipa-prop: Removing "
				     "cloning-created reference "
				     "from %s to %s.\n",
				     clone->dump_name (),
				     n->dump_name ());
			  ref->remove_reference ();
			}
		      clone = clone->callers->caller;
		    }
		}
	    }
	}
    }

  for (i = ipa_get_param_count (old_root_info);
       i < ipa_get_cs_argument_count (args);
       i++)
    {
      struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);

      if (jf->type == IPA_JF_CONST)
	{
	  struct ipa_cst_ref_desc *rdesc = jfunc_rdesc_usable (jf);
	  if (rdesc)
	    rdesc->refcount = IPA_UNDESCRIBED_USE;
	}
      else if (jf->type == IPA_JF_PASS_THROUGH)
	ipa_set_controlled_uses (new_root_info,
				 jf->value.pass_through.formal_id,
				 IPA_UNDESCRIBED_USE);
    }
}

/* Update jump functions and call note functions on inlining the call site CS.
   CS is expected to lead to a node already cloned by
   cgraph_clone_inline_nodes.  Newly discovered indirect edges will be added to
   *NEW_EDGES, unless NEW_EDGES is NULL.  Return true iff a new edge(s) were +
   created.  */

bool
ipa_propagate_indirect_call_infos (struct cgraph_edge *cs,
				   vec<cgraph_edge *> *new_edges)
{
  bool changed;
  /* Do nothing if the preparation phase has not been carried out yet
     (i.e. during early inlining).  */
  if (!ipa_node_params_sum)
    return false;
  gcc_assert (ipa_edge_args_sum);

  propagate_controlled_uses (cs);
  changed = propagate_info_to_inlined_callees (cs, cs->callee, new_edges);
  ipa_node_params_sum->remove (cs->callee);

  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  if (args)
    {
      bool ok = true;
      if (args->jump_functions)
	{
	  struct ipa_jump_func *jf;
	  int i;
	  FOR_EACH_VEC_ELT (*args->jump_functions, i, jf)
	    if (jf->type == IPA_JF_CONST
		&& ipa_get_jf_constant_rdesc (jf))
	      {
		ok = false;
		break;
	      }
	}
      if (ok)
        ipa_edge_args_sum->remove (cs);
    }
  if (ipcp_transformation_sum)
    ipcp_transformation_sum->remove (cs->callee);

  return changed;
}

/* Ensure that array of edge arguments infos is big enough to accommodate a
   structure for all edges and reallocates it if not.  Also, allocate
   associated hash tables is they do not already exist.  */

void
ipa_check_create_edge_args (void)
{
  if (!ipa_edge_args_sum)
    ipa_edge_args_sum
      = (new (ggc_alloc_no_dtor<ipa_edge_args_sum_t> ())
	 ipa_edge_args_sum_t (symtab, true));
  if (!ipa_vr_hash_table)
    ipa_vr_hash_table = hash_table<ipa_vr_ggc_hash_traits>::create_ggc (37);
}

/* Free all ipa_edge structures.  */

void
ipa_free_all_edge_args (void)
{
  if (!ipa_edge_args_sum)
    return;

  ggc_delete (ipa_edge_args_sum);
  ipa_edge_args_sum = NULL;
}

/* Free all ipa_node_params structures.  */

void
ipa_free_all_node_params (void)
{
  if (ipa_node_params_sum)
    ggc_delete (ipa_node_params_sum);
  ipa_node_params_sum = NULL;
}

/* Initialize IPA CP transformation summary and also allocate any necessary hash
   tables if they do not already exist.  */

void
ipcp_transformation_initialize (void)
{
  if (!ipa_vr_hash_table)
    ipa_vr_hash_table = hash_table<ipa_vr_ggc_hash_traits>::create_ggc (37);
  if (ipcp_transformation_sum == NULL)
    {
      ipcp_transformation_sum = ipcp_transformation_t::create_ggc (symtab);
      ipcp_transformation_sum->disable_insertion_hook ();
    }
}

/* Release the IPA CP transformation summary.  */

void
ipcp_free_transformation_sum (void)
{
  if (!ipcp_transformation_sum)
    return;

  ipcp_transformation_sum->~function_summary<ipcp_transformation *> ();
  ggc_free (ipcp_transformation_sum);
  ipcp_transformation_sum = NULL;
}

/* Set the aggregate replacements of NODE to be AGGVALS.  */

void
ipa_set_node_agg_value_chain (struct cgraph_node *node,
			      vec<ipa_argagg_value, va_gc> *aggs)
{
  ipcp_transformation_initialize ();
  ipcp_transformation *s = ipcp_transformation_sum->get_create (node);
  s->m_agg_values = aggs;
}

/* Hook that is called by cgraph.cc when an edge is removed.  Adjust reference
   count data structures accordingly.  */

void
ipa_edge_args_sum_t::remove (cgraph_edge *cs, ipa_edge_args *args)
{
  if (args->jump_functions)
    {
      struct ipa_jump_func *jf;
      int i;
      FOR_EACH_VEC_ELT (*args->jump_functions, i, jf)
	{
	  struct ipa_cst_ref_desc *rdesc;
	  try_decrement_rdesc_refcount (jf);
	  if (jf->type == IPA_JF_CONST
	      && (rdesc = ipa_get_jf_constant_rdesc (jf))
	      && rdesc->cs == cs)
	    rdesc->cs = NULL;
	}
    }
}

/* Copy information from SRC_JF to DST_JF which correstpond to call graph edges
   SRC and DST.  */

static void
ipa_duplicate_jump_function (cgraph_edge *src, cgraph_edge *dst,
			     ipa_jump_func *src_jf, ipa_jump_func *dst_jf)
{
  dst_jf->agg.items = vec_safe_copy (src_jf->agg.items);
  dst_jf->agg.by_ref = src_jf->agg.by_ref;

  /* We can avoid calling ipa_set_jfunc_vr since it would only look up the
     place in the hash_table where the source m_vr resides.  */
  dst_jf->m_vr = src_jf->m_vr;

  if (src_jf->type == IPA_JF_CONST)
    {
      ipa_set_jf_cst_copy (dst_jf, src_jf);
      struct ipa_cst_ref_desc *src_rdesc = jfunc_rdesc_usable (src_jf);

      if (!src_rdesc)
	dst_jf->value.constant.rdesc = NULL;
      else if (src->caller == dst->caller)
	{
	  /* Creation of a speculative edge.  If the source edge is the one
	     grabbing a reference, we must create a new (duplicate)
	     reference description.  Otherwise they refer to the same
	     description corresponding to a reference taken in a function
	     src->caller is inlined to.  In that case we just must
	     increment the refcount.  */
	  if (src_rdesc->cs == src)
	    {
	      symtab_node *n = symtab_node_for_jfunc (src_jf);
	      gcc_checking_assert (n);
	      ipa_ref *ref
		= src->caller->find_reference (n, src->call_stmt,
					       src->lto_stmt_uid,
					       IPA_REF_ADDR);
	      gcc_checking_assert (ref);
	      dst->caller->clone_reference (ref, ref->stmt);

	      ipa_cst_ref_desc *dst_rdesc = ipa_refdesc_pool.allocate ();
	      dst_rdesc->cs = dst;
	      dst_rdesc->refcount = src_rdesc->refcount;
	      dst_rdesc->next_duplicate = NULL;
	      dst_jf->value.constant.rdesc = dst_rdesc;
	    }
	  else
	    {
	      src_rdesc->refcount++;
	      dst_jf->value.constant.rdesc = src_rdesc;
	    }
	}
      else if (src_rdesc->cs == src)
	{
	  struct ipa_cst_ref_desc *dst_rdesc = ipa_refdesc_pool.allocate ();
	  dst_rdesc->cs = dst;
	  dst_rdesc->refcount = src_rdesc->refcount;
	  dst_rdesc->next_duplicate = src_rdesc->next_duplicate;
	  src_rdesc->next_duplicate = dst_rdesc;
	  dst_jf->value.constant.rdesc = dst_rdesc;
	}
      else
	{
	  struct ipa_cst_ref_desc *dst_rdesc;
	  /* This can happen during inlining, when a JFUNC can refer to a
	     reference taken in a function up in the tree of inline clones.
	     We need to find the duplicate that refers to our tree of
	     inline clones.  */

	  gcc_assert (dst->caller->inlined_to);
	  for (dst_rdesc = src_rdesc->next_duplicate;
	       dst_rdesc;
	       dst_rdesc = dst_rdesc->next_duplicate)
	    {
	      struct cgraph_node *top;
	      top = dst_rdesc->cs->caller->inlined_to
		? dst_rdesc->cs->caller->inlined_to
		: dst_rdesc->cs->caller;
	      if (dst->caller->inlined_to == top)
		break;
	    }
	  gcc_assert (dst_rdesc);
	  dst_jf->value.constant.rdesc = dst_rdesc;
	}
    }
  else if (src_jf->type == IPA_JF_PASS_THROUGH)
    {
      dst_jf->type = IPA_JF_PASS_THROUGH;
      dst_jf->value.pass_through = src_jf->value.pass_through;
      if (src->caller == dst->caller)
	{
	  struct cgraph_node *inline_root = dst->caller->inlined_to
	    ? dst->caller->inlined_to : dst->caller;
	  ipa_node_params *root_info = ipa_node_params_sum->get (inline_root);
	  int idx = ipa_get_jf_pass_through_formal_id (dst_jf);

	  int c = ipa_get_controlled_uses (root_info, idx);
	  if (c != IPA_UNDESCRIBED_USE)
	    {
	      c++;
	      ipa_set_controlled_uses (root_info, idx, c);
	    }
	}
    }
  else if (src_jf->type == IPA_JF_ANCESTOR)
    {
      dst_jf->type = IPA_JF_ANCESTOR;
      dst_jf->value.ancestor = src_jf->value.ancestor;
    }
  else
    gcc_assert (src_jf->type == IPA_JF_UNKNOWN);
}

/* Method invoked when an edge is duplicated.  Copy ipa_edge_args and adjust
   reference count data strucutres accordingly.  */

void
ipa_edge_args_sum_t::duplicate (cgraph_edge *src, cgraph_edge *dst,
				ipa_edge_args *old_args, ipa_edge_args *new_args)
{
  unsigned int i;

  if (old_args->polymorphic_call_contexts)
    new_args->polymorphic_call_contexts
      = vec_safe_copy (old_args->polymorphic_call_contexts);

  if (!vec_safe_length (old_args->jump_functions))
    {
      new_args->jump_functions = NULL;
      return;
    }
  vec_safe_grow_cleared (new_args->jump_functions,
			 old_args->jump_functions->length (), true);

  for (i = 0; i < vec_safe_length (old_args->jump_functions); i++)
    {
      struct ipa_jump_func *src_jf = ipa_get_ith_jump_func (old_args, i);
      struct ipa_jump_func *dst_jf = ipa_get_ith_jump_func (new_args, i);

      ipa_duplicate_jump_function (src, dst, src_jf, dst_jf);
    }
}

/* Analyze newly added function into callgraph.  */

static void
ipa_add_new_function (cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (node->has_gimple_body_p ())
    ipa_analyze_node (node);
}

/* Hook that is called by summary when a node is duplicated.  */

void
ipa_node_params_t::duplicate(cgraph_node *, cgraph_node *,
			     ipa_node_params *old_info,
			     ipa_node_params *new_info)
{
  new_info->descriptors = vec_safe_copy (old_info->descriptors);
  gcc_assert (new_info->lattices.is_empty ());
  new_info->ipcp_orig_node = old_info->ipcp_orig_node;
  new_info->known_csts = old_info->known_csts.copy ();
  new_info->known_contexts = old_info->known_contexts.copy ();

  new_info->analysis_done = old_info->analysis_done;
  new_info->node_enqueued = old_info->node_enqueued;
  new_info->versionable = old_info->versionable;
}

/* Duplication of ipcp transformation summaries.  */

void
ipcp_transformation_t::duplicate(cgraph_node *, cgraph_node *dst,
			         ipcp_transformation *src_trans,
			         ipcp_transformation *dst_trans)
{
  /* Avoid redundant work of duplicating vectors we will never use.  */
  if (dst->inlined_to)
    return;
  dst_trans->m_agg_values = vec_safe_copy (src_trans->m_agg_values);
  dst_trans->m_vr = vec_safe_copy (src_trans->m_vr);
}

/* Register our cgraph hooks if they are not already there.  */

void
ipa_register_cgraph_hooks (void)
{
  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();

  function_insertion_hook_holder =
      symtab->add_cgraph_insertion_hook (&ipa_add_new_function, NULL);
}

/* Unregister our cgraph hooks if they are not already there.  */

static void
ipa_unregister_cgraph_hooks (void)
{
  if (function_insertion_hook_holder)
    symtab->remove_cgraph_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after ipa-cp.  */

void
ipa_free_all_structures_after_ipa_cp (void)
{
  if (!optimize && !in_lto_p)
    {
      ipa_free_all_edge_args ();
      ipa_free_all_node_params ();
      ipcp_sources_pool.release ();
      ipcp_cst_values_pool.release ();
      ipcp_poly_ctx_values_pool.release ();
      ipcp_agg_lattice_pool.release ();
      ipa_unregister_cgraph_hooks ();
      ipa_refdesc_pool.release ();
    }
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after indirect inlining.  */

void
ipa_free_all_structures_after_iinln (void)
{
  ipa_free_all_edge_args ();
  ipa_free_all_node_params ();
  ipa_unregister_cgraph_hooks ();
  ipcp_sources_pool.release ();
  ipcp_cst_values_pool.release ();
  ipcp_poly_ctx_values_pool.release ();
  ipcp_agg_lattice_pool.release ();
  ipa_refdesc_pool.release ();
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */

void
ipa_print_node_params (FILE *f, struct cgraph_node *node)
{
  int i, count;
  class ipa_node_params *info;

  if (!node->definition)
    return;
  info = ipa_node_params_sum->get (node);
  fprintf (f, "  function  %s parameter descriptors:\n", node->dump_name ());
  if (!info)
    {
      fprintf (f, " no params return\n");
      return;
    }
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    {
      int c;

      fprintf (f, "    ");
      ipa_dump_param (f, info, i);
      if (ipa_is_param_used (info, i))
	fprintf (f, " used");
      if (ipa_is_param_used_by_ipa_predicates (info, i))
	fprintf (f, " used_by_ipa_predicates");
      if (ipa_is_param_used_by_indirect_call (info, i))
	fprintf (f, " used_by_indirect_call");
      if (ipa_is_param_used_by_polymorphic_call (info, i))
	fprintf (f, " used_by_polymorphic_call");
      c = ipa_get_controlled_uses (info, i);
      if (c == IPA_UNDESCRIBED_USE)
	fprintf (f, " undescribed_use");
      else
	fprintf (f, "  controlled_uses=%i %s", c,
		 ipa_get_param_load_dereferenced (info, i)
		 ? "(load_dereferenced)" : "");
      fprintf (f, "\n");
    }
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */

void
ipa_print_all_params (FILE * f)
{
  struct cgraph_node *node;

  fprintf (f, "\nFunction parameters:\n");
  FOR_EACH_FUNCTION (node)
    ipa_print_node_params (f, node);
}

/* Stream out jump function JUMP_FUNC to OB.  */

static void
ipa_write_jump_function (struct output_block *ob,
			 struct ipa_jump_func *jump_func)
{
  struct ipa_agg_jf_item *item;
  struct bitpack_d bp;
  int i, count;
  int flag = 0;

  /* ADDR_EXPRs are very comon IP invariants; save some streamer data
     as well as WPA memory by handling them specially.  */
  if (jump_func->type == IPA_JF_CONST
      && TREE_CODE (jump_func->value.constant.value) == ADDR_EXPR)
    flag = 1;

  streamer_write_uhwi (ob, jump_func->type * 2 + flag);
  switch (jump_func->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_CONST:
      gcc_assert (
	  EXPR_LOCATION (jump_func->value.constant.value) == UNKNOWN_LOCATION);
      stream_write_tree (ob,
			 flag
			 ? TREE_OPERAND (jump_func->value.constant.value, 0)
			 : jump_func->value.constant.value, true);
      break;
    case IPA_JF_PASS_THROUGH:
      streamer_write_uhwi (ob, jump_func->value.pass_through.operation);
      if (jump_func->value.pass_through.operation == NOP_EXPR)
	{
	  streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, jump_func->value.pass_through.agg_preserved, 1);
	  gcc_assert (!jump_func->value.pass_through.refdesc_decremented);
	  streamer_write_bitpack (&bp);
	}
      else if (TREE_CODE_CLASS (jump_func->value.pass_through.operation)
	       == tcc_unary)
	{
	  stream_write_tree (ob, jump_func->value.pass_through.op_type, true);
	  streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
	}
      else
	{
	  stream_write_tree (ob, jump_func->value.pass_through.op_type, true);
	  stream_write_tree (ob, jump_func->value.pass_through.operand, true);
	  streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
	}
      break;
    case IPA_JF_ANCESTOR:
      streamer_write_uhwi (ob, jump_func->value.ancestor.offset);
      streamer_write_uhwi (ob, jump_func->value.ancestor.formal_id);
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, jump_func->value.ancestor.agg_preserved, 1);
      bp_pack_value (&bp, jump_func->value.ancestor.keep_null, 1);
      streamer_write_bitpack (&bp);
      break;
    default:
      fatal_error (UNKNOWN_LOCATION, "invalid jump function in LTO stream");
    }

  count = vec_safe_length (jump_func->agg.items);
  streamer_write_uhwi (ob, count);
  if (count)
    {
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, jump_func->agg.by_ref, 1);
      streamer_write_bitpack (&bp);
    }

  FOR_EACH_VEC_SAFE_ELT (jump_func->agg.items, i, item)
    {
      stream_write_tree (ob, item->type, true);
      streamer_write_uhwi (ob, item->offset);
      streamer_write_uhwi (ob, item->jftype);
      switch (item->jftype)
	{
	case IPA_JF_UNKNOWN:
	  break;
	case IPA_JF_CONST:
	  stream_write_tree (ob, item->value.constant, true);
	  break;
	case IPA_JF_PASS_THROUGH:
	case IPA_JF_LOAD_AGG:
	  streamer_write_uhwi (ob, item->value.pass_through.operation);
	  streamer_write_uhwi (ob, item->value.pass_through.formal_id);
	  if (item->value.pass_through.operation != NOP_EXPR)
	    stream_write_tree (ob, item->value.pass_through.op_type, true);
	  if (TREE_CODE_CLASS (item->value.pass_through.operation)
							!= tcc_unary)
	    stream_write_tree (ob, item->value.pass_through.operand, true);
	  if (item->jftype == IPA_JF_LOAD_AGG)
	    {
	      stream_write_tree (ob, item->value.load_agg.type, true);
	      streamer_write_uhwi (ob, item->value.load_agg.offset);
	      bp = bitpack_create (ob->main_stream);
	      bp_pack_value (&bp, item->value.load_agg.by_ref, 1);
	      streamer_write_bitpack (&bp);
	    }
	  break;
	default:
	  fatal_error (UNKNOWN_LOCATION,
		       "invalid jump function in LTO stream");
	}
    }

  bp = bitpack_create (ob->main_stream);
  if (jump_func->m_vr)
    jump_func->m_vr->streamer_write (ob);
  else
    {
      bp_pack_value (&bp, false, 1);
      streamer_write_bitpack (&bp);
    }
}

/* Read in jump function JUMP_FUNC from IB.  */

static void
ipa_read_jump_function (class lto_input_block *ib,
			struct ipa_jump_func *jump_func,
			struct cgraph_edge *cs,
			class data_in *data_in,
			bool prevails)
{
  enum jump_func_type jftype;
  enum tree_code operation;
  int i, count;
  int val = streamer_read_uhwi (ib);
  bool flag = val & 1;

  jftype = (enum jump_func_type) (val / 2);
  switch (jftype)
    {
    case IPA_JF_UNKNOWN:
      ipa_set_jf_unknown (jump_func);
      break;
    case IPA_JF_CONST:
      {
	tree t = stream_read_tree (ib, data_in);
	if (flag && prevails)
	  t = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (t)), t);
	ipa_set_jf_constant (jump_func, t, cs);
      }
      break;
    case IPA_JF_PASS_THROUGH:
      operation = (enum tree_code) streamer_read_uhwi (ib);
      if (operation == NOP_EXPR)
	{
	  int formal_id =  streamer_read_uhwi (ib);
	  struct bitpack_d bp = streamer_read_bitpack (ib);
	  bool agg_preserved = bp_unpack_value (&bp, 1);
	  ipa_set_jf_simple_pass_through (jump_func, formal_id, agg_preserved);
	}
      else if (TREE_CODE_CLASS (operation) == tcc_unary)
	{
	  tree op_type = stream_read_tree (ib, data_in);
	  int formal_id =  streamer_read_uhwi (ib);
	  ipa_set_jf_unary_pass_through (jump_func, formal_id, operation,
					 op_type);
	}
      else
	{
	  tree op_type = stream_read_tree (ib, data_in);
	  tree operand = stream_read_tree (ib, data_in);
	  int formal_id =  streamer_read_uhwi (ib);
	  ipa_set_jf_arith_pass_through (jump_func, formal_id, operand,
					 operation, op_type);
	}
      break;
    case IPA_JF_ANCESTOR:
      {
	HOST_WIDE_INT offset = streamer_read_uhwi (ib);
	int formal_id = streamer_read_uhwi (ib);
	struct bitpack_d bp = streamer_read_bitpack (ib);
	bool agg_preserved = bp_unpack_value (&bp, 1);
	bool keep_null = bp_unpack_value (&bp, 1);
	ipa_set_ancestor_jf (jump_func, offset, formal_id, agg_preserved,
			     keep_null);
	break;
      }
    default:
      fatal_error (UNKNOWN_LOCATION, "invalid jump function in LTO stream");
    }

  count = streamer_read_uhwi (ib);
  if (prevails)
    {
      jump_func->agg.items = NULL;
      vec_safe_reserve (jump_func->agg.items, count, true);
    }
  if (count)
    {
      struct bitpack_d bp = streamer_read_bitpack (ib);
      jump_func->agg.by_ref = bp_unpack_value (&bp, 1);
    }
  for (i = 0; i < count; i++)
    {
      struct ipa_agg_jf_item item;
      item.type = stream_read_tree (ib, data_in);
      item.offset = streamer_read_uhwi (ib);
      item.jftype = (enum jump_func_type) streamer_read_uhwi (ib);

      switch (item.jftype)
	{
	case IPA_JF_UNKNOWN:
	  break;
	case IPA_JF_CONST:
	  item.value.constant = stream_read_tree (ib, data_in);
	  break;
	case IPA_JF_PASS_THROUGH:
	case IPA_JF_LOAD_AGG:
	  operation = (enum tree_code) streamer_read_uhwi (ib);
	  item.value.pass_through.operation = operation;
	  item.value.pass_through.formal_id = streamer_read_uhwi (ib);
	  if (operation != NOP_EXPR)
	    item.value.pass_through.op_type = stream_read_tree (ib, data_in);
	  else
	    item.value.pass_through.op_type = NULL_TREE;
	  if (TREE_CODE_CLASS (operation) == tcc_unary)
	    item.value.pass_through.operand = NULL_TREE;
	  else
	    item.value.pass_through.operand = stream_read_tree (ib, data_in);
	  if (item.jftype == IPA_JF_LOAD_AGG)
	    {
	      struct bitpack_d bp;
	      item.value.load_agg.type = stream_read_tree (ib, data_in);
	      item.value.load_agg.offset = streamer_read_uhwi (ib);
	      bp = streamer_read_bitpack (ib);
	      item.value.load_agg.by_ref = bp_unpack_value (&bp, 1);
	    }
	  break;
	default:
	  fatal_error (UNKNOWN_LOCATION,
		       "invalid jump function in LTO stream");
	}
      if (prevails)
        jump_func->agg.items->quick_push (item);
    }

  ipa_vr vr;
  vr.streamer_read (ib, data_in);
  if (vr.known_p ())
    {
      if (prevails)
	ipa_set_jfunc_vr (jump_func, vr);
    }
  else
    jump_func->m_vr = NULL;
}

/* Stream out parts of cgraph_indirect_call_info corresponding to CS that are
   relevant to indirect inlining to OB.  */

static void
ipa_write_indirect_edge_info (struct output_block *ob,
			      struct cgraph_edge *cs)
{
  class cgraph_indirect_call_info *ii = cs->indirect_info;
  struct bitpack_d bp;

  streamer_write_hwi (ob, ii->param_index);
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, ii->polymorphic, 1);
  bp_pack_value (&bp, ii->agg_contents, 1);
  bp_pack_value (&bp, ii->member_ptr, 1);
  bp_pack_value (&bp, ii->by_ref, 1);
  bp_pack_value (&bp, ii->guaranteed_unmodified, 1);
  bp_pack_value (&bp, ii->vptr_changed, 1);
  streamer_write_bitpack (&bp);
  if (ii->agg_contents || ii->polymorphic)
    streamer_write_hwi (ob, ii->offset);
  else
    gcc_assert (ii->offset == 0);

  if (ii->polymorphic)
    {
      streamer_write_hwi (ob, ii->otr_token);
      stream_write_tree (ob, ii->otr_type, true);
      ii->context.stream_out (ob);
    }
}

/* Read in parts of cgraph_indirect_call_info corresponding to CS that are
   relevant to indirect inlining from IB.  */

static void
ipa_read_indirect_edge_info (class lto_input_block *ib,
			     class data_in *data_in,
			     struct cgraph_edge *cs,
			     class ipa_node_params *info)
{
  class cgraph_indirect_call_info *ii = cs->indirect_info;
  struct bitpack_d bp;

  ii->param_index = (int) streamer_read_hwi (ib);
  bp = streamer_read_bitpack (ib);
  ii->polymorphic = bp_unpack_value (&bp, 1);
  ii->agg_contents = bp_unpack_value (&bp, 1);
  ii->member_ptr = bp_unpack_value (&bp, 1);
  ii->by_ref = bp_unpack_value (&bp, 1);
  ii->guaranteed_unmodified = bp_unpack_value (&bp, 1);
  ii->vptr_changed = bp_unpack_value (&bp, 1);
  if (ii->agg_contents || ii->polymorphic)
    ii->offset = (HOST_WIDE_INT) streamer_read_hwi (ib);
  else
    ii->offset = 0;
  if (ii->polymorphic)
    {
      ii->otr_token = (HOST_WIDE_INT) streamer_read_hwi (ib);
      ii->otr_type = stream_read_tree (ib, data_in);
      ii->context.stream_in (ib, data_in);
    }
  if (info && ii->param_index >= 0)
    {
      if (ii->polymorphic)
	ipa_set_param_used_by_polymorphic_call (info,
						ii->param_index , true);
      ipa_set_param_used_by_indirect_call (info,
					   ii->param_index, true);
    }
}

/* Stream out NODE info to OB.  */

static void
ipa_write_node_info (struct output_block *ob, struct cgraph_node *node)
{
  int node_ref;
  lto_symtab_encoder_t encoder;
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int j;
  struct cgraph_edge *e;
  struct bitpack_d bp;

  encoder = ob->decl_state->symtab_node_encoder;
  node_ref = lto_symtab_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  streamer_write_uhwi (ob, ipa_get_param_count (info));
  for (j = 0; j < ipa_get_param_count (info); j++)
    streamer_write_uhwi (ob, ipa_get_param_move_cost (info, j));
  bp = bitpack_create (ob->main_stream);
  gcc_assert (info->analysis_done
	      || ipa_get_param_count (info) == 0);
  gcc_assert (!info->node_enqueued);
  gcc_assert (!info->ipcp_orig_node);
  for (j = 0; j < ipa_get_param_count (info); j++)
    {
      /* TODO: We could just not stream the bit in the undescribed case. */
      bool d = (ipa_get_controlled_uses (info, j) != IPA_UNDESCRIBED_USE)
	? ipa_get_param_load_dereferenced (info, j) : true;
      bp_pack_value (&bp, d, 1);
      bp_pack_value (&bp, ipa_is_param_used (info, j), 1);
    }
  streamer_write_bitpack (&bp);
  for (j = 0; j < ipa_get_param_count (info); j++)
    {
      streamer_write_hwi (ob, ipa_get_controlled_uses (info, j));
      stream_write_tree (ob, ipa_get_type (info, j), true);
    }
  for (e = node->callees; e; e = e->next_callee)
    {
      ipa_edge_args *args = ipa_edge_args_sum->get (e);

      if (!args)
	{
	  streamer_write_uhwi (ob, 0);
	  continue;
	}

      streamer_write_uhwi (ob,
			   ipa_get_cs_argument_count (args) * 2
			   + (args->polymorphic_call_contexts != NULL));
      for (j = 0; j < ipa_get_cs_argument_count (args); j++)
	{
	  ipa_write_jump_function (ob, ipa_get_ith_jump_func (args, j));
	  if (args->polymorphic_call_contexts != NULL)
	    ipa_get_ith_polymorhic_call_context (args, j)->stream_out (ob);
	}
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      ipa_edge_args *args = ipa_edge_args_sum->get (e);
      if (!args)
	streamer_write_uhwi (ob, 0);
      else
	{
	  streamer_write_uhwi (ob,
			       ipa_get_cs_argument_count (args) * 2
			       + (args->polymorphic_call_contexts != NULL));
	  for (j = 0; j < ipa_get_cs_argument_count (args); j++)
	    {
	      ipa_write_jump_function (ob, ipa_get_ith_jump_func (args, j));
	      if (args->polymorphic_call_contexts != NULL)
		ipa_get_ith_polymorhic_call_context (args, j)->stream_out (ob);
	    }
	}
      ipa_write_indirect_edge_info (ob, e);
    }
}

/* Stream in edge E from IB.  */

static void
ipa_read_edge_info (class lto_input_block *ib,
		    class data_in *data_in,
		    struct cgraph_edge *e, bool prevails)
{
  int count = streamer_read_uhwi (ib);
  bool contexts_computed = count & 1;

  count /= 2;
  if (!count)
    return;
  if (prevails
      && (e->possibly_call_in_translation_unit_p ()
	  /* Also stream in jump functions to builtins in hope that they
	     will get fnspecs.  */
	  || fndecl_built_in_p (e->callee->decl, BUILT_IN_NORMAL)))
    {
      ipa_edge_args *args = ipa_edge_args_sum->get_create (e);
      vec_safe_grow_cleared (args->jump_functions, count, true);
      if (contexts_computed)
	vec_safe_grow_cleared (args->polymorphic_call_contexts, count, true);
      for (int k = 0; k < count; k++)
	{
	  ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k), e,
				  data_in, prevails);
	  if (contexts_computed)
	    ipa_get_ith_polymorhic_call_context (args, k)->stream_in
							     (ib, data_in);
	}
    }
  else
    {
      for (int k = 0; k < count; k++)
	{
	  struct ipa_jump_func dummy;
	  ipa_read_jump_function (ib, &dummy, e,
				  data_in, prevails);
	  if (contexts_computed)
	    {
	      class ipa_polymorphic_call_context ctx;
	      ctx.stream_in (ib, data_in);
	    }
	}
    }
}

/* Stream in NODE info from IB.  */

static void
ipa_read_node_info (class lto_input_block *ib, struct cgraph_node *node,
		    class data_in *data_in)
{
  int k;
  struct cgraph_edge *e;
  struct bitpack_d bp;
  bool prevails = node->prevailing_p ();
  ipa_node_params *info
    = prevails ? ipa_node_params_sum->get_create (node) : NULL;

  int param_count = streamer_read_uhwi (ib);
  if (prevails)
    {
      ipa_alloc_node_params (node, param_count);
      for (k = 0; k < param_count; k++)
        (*info->descriptors)[k].move_cost = streamer_read_uhwi (ib);
      if (ipa_get_param_count (info) != 0)
	info->analysis_done = true;
      info->node_enqueued = false;
    }
  else
    for (k = 0; k < param_count; k++)
      streamer_read_uhwi (ib);

  bp = streamer_read_bitpack (ib);
  for (k = 0; k < param_count; k++)
    {
      bool load_dereferenced = bp_unpack_value (&bp, 1);
      bool used = bp_unpack_value (&bp, 1);

      if (prevails)
	{
	  ipa_set_param_load_dereferenced (info, k, load_dereferenced);
	  ipa_set_param_used (info, k, used);
	}
    }
  for (k = 0; k < param_count; k++)
    {
      int nuses = streamer_read_hwi (ib);
      tree type = stream_read_tree (ib, data_in);

      if (prevails)
	{
	  ipa_set_controlled_uses (info, k, nuses);
	  (*info->descriptors)[k].decl_or_type = type;
	}
    }
  for (e = node->callees; e; e = e->next_callee)
    ipa_read_edge_info (ib, data_in, e, prevails);
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      ipa_read_edge_info (ib, data_in, e, prevails);
      ipa_read_indirect_edge_info (ib, data_in, e, info);
    }
}

/* Stream out ipa_return_summary.  */
static void
ipa_write_return_summaries (output_block *ob)
{
  if (!ipa_return_value_sum)
    {
      streamer_write_uhwi (ob, 0);
      return;
    }

  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;
  for (int i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      ipa_return_value_summary *v;

      if (cnode && cnode->definition && !cnode->alias
	  && (v = ipa_return_value_sum->get (cnode))
	  && v->vr)
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (int i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      ipa_return_value_summary *v;

      if (cnode && cnode->definition && !cnode->alias
	  && (v = ipa_return_value_sum->get (cnode))
	  && v->vr)
	{
	  streamer_write_uhwi
	    (ob,
	     lto_symtab_encoder_encode (encoder, cnode));
	  v->vr->streamer_write (ob);
	}
    }
}

/* Write jump functions for nodes in SET.  */

void
ipa_prop_write_jump_functions (void)
{
  struct output_block *ob;
  unsigned int count = 0;
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder;

  if (!ipa_node_params_sum || !ipa_edge_args_sum)
    return;

  ob = create_output_block (LTO_section_jump_functions);
  encoder = ob->decl_state->symtab_node_encoder;
  ob->symbol = NULL;
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *node = lsei_cgraph_node (lsei);
      if (node->has_gimple_body_p ()
	  && ipa_node_params_sum->get (node) != NULL)
	count++;
    }

  streamer_write_uhwi (ob, count);

  /* Process all of the functions.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *node = lsei_cgraph_node (lsei);
      if (node->has_gimple_body_p ()
	  && ipa_node_params_sum->get (node) != NULL)
        ipa_write_node_info (ob, node);
    }
  ipa_write_return_summaries (ob);
  produce_asm (ob);
  destroy_output_block (ob);
}

/* Record that return value range of N is VAL.  */

static void
ipa_record_return_value_range_1 (cgraph_node *n, value_range val)
{
  if (!ipa_return_value_sum)
    {
      if (!ipa_vr_hash_table)
	ipa_vr_hash_table = hash_table<ipa_vr_ggc_hash_traits>::create_ggc (37);
      ipa_return_value_sum = new (ggc_alloc_no_dtor <ipa_return_value_sum_t> ())
	      ipa_return_value_sum_t (symtab, true);
      ipa_return_value_sum->disable_insertion_hook ();
    }
  ipa_return_value_sum->get_create (n)->vr = ipa_get_value_range (val);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Recording return range of %s:", n->dump_name ());
      val.dump (dump_file);
      fprintf (dump_file, "\n");
    }
}

/* Stream out ipa_return_summary.  */
static void
ipa_read_return_summaries (lto_input_block *ib,
			   struct lto_file_decl_data *file_data,
			   class data_in *data_in)
{
  unsigned int f_count = streamer_read_uhwi (ib);
  for (unsigned int i = 0; i < f_count; i++)
    {
      unsigned int index = streamer_read_uhwi (ib);
      lto_symtab_encoder_t encoder = file_data->symtab_node_encoder;
      struct cgraph_node *node
	      = dyn_cast <cgraph_node *>
		  (lto_symtab_encoder_deref (encoder, index));
      ipa_vr rvr;
      rvr.streamer_read (ib, data_in);
      if (node->prevailing_p ())
	{
	  value_range tmp;
	  rvr.get_vrange (tmp);
	  ipa_record_return_value_range_1 (node, tmp);
	}
    }
}

/* Read section in file FILE_DATA of length LEN with data DATA.  */

static void
ipa_prop_read_section (struct lto_file_decl_data *file_data, const char *data,
		       size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  class data_in *data_in;
  unsigned int i;
  unsigned int count;

  lto_input_block ib_main ((const char *) data + main_offset,
			   header->main_size, file_data);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));
      gcc_assert (node->definition);
      ipa_read_node_info (&ib_main, node, data_in);
    }
  ipa_read_return_summaries (&ib_main, file_data, data_in);
  lto_free_section_data (file_data, LTO_section_jump_functions, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read ipcp jump functions.  */

void
ipa_prop_read_jump_functions (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  ipa_register_cgraph_hooks ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data
	= lto_get_summary_section_data (file_data, LTO_section_jump_functions,
					&len);
      if (data)
        ipa_prop_read_section (file_data, data, len);
    }
}

/* Return true if the IPA-CP transformation summary TS is non-NULL and contains
   useful info.  */
static bool
useful_ipcp_transformation_info_p (ipcp_transformation *ts)
{
  if (!ts)
    return false;
  if (!vec_safe_is_empty (ts->m_agg_values)
      || !vec_safe_is_empty (ts->m_vr))
    return true;
  return false;
}

/* Write into OB IPA-CP transfromation summary TS describing NODE.  */

void
write_ipcp_transformation_info (output_block *ob, cgraph_node *node,
				ipcp_transformation *ts)
{
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  int node_ref = lto_symtab_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  streamer_write_uhwi (ob, vec_safe_length (ts->m_agg_values));
  for (const ipa_argagg_value &av : ts->m_agg_values)
    {
      struct bitpack_d bp;

      stream_write_tree (ob, av.value, true);
      streamer_write_uhwi (ob, av.unit_offset);
      streamer_write_uhwi (ob, av.index);

      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, av.by_ref, 1);
      bp_pack_value (&bp, av.killed, 1);
      streamer_write_bitpack (&bp);
    }

  /* If all instances of this node are inlined, ipcp info is not useful.  */
  if (!lto_symtab_encoder_only_for_inlining_p (encoder, node))
    {
      streamer_write_uhwi (ob, vec_safe_length (ts->m_vr));
      for (const ipa_vr &parm_vr : ts->m_vr)
	parm_vr.streamer_write (ob);
    }
  else
    streamer_write_uhwi (ob, 0);
}

/* Stream in the aggregate value replacement chain for NODE from IB.  */

static void
read_ipcp_transformation_info (lto_input_block *ib, cgraph_node *node,
			       data_in *data_in)
{
  unsigned int count, i;
  ipcp_transformation_initialize ();
  ipcp_transformation *ts = ipcp_transformation_sum->get_create (node);

  count = streamer_read_uhwi (ib);
  if (count > 0)
    {
      vec_safe_grow_cleared (ts->m_agg_values, count, true);
      for (i = 0; i <count; i++)
	{
	  ipa_argagg_value *av = &(*ts->m_agg_values)[i];;

	  av->value = stream_read_tree (ib, data_in);
	  av->unit_offset = streamer_read_uhwi (ib);
	  av->index = streamer_read_uhwi (ib);

	  bitpack_d bp = streamer_read_bitpack (ib);
	  av->by_ref = bp_unpack_value (&bp, 1);
	  av->killed = bp_unpack_value (&bp, 1);
	}
    }

  count = streamer_read_uhwi (ib);
  if (count > 0)
    {
      vec_safe_grow_cleared (ts->m_vr, count, true);
      for (i = 0; i < count; i++)
	{
	  ipa_vr *parm_vr;
	  parm_vr = &(*ts->m_vr)[i];
	  parm_vr->streamer_read (ib, data_in);
	}
    }
}


/* Write all aggregate replacement for nodes in set.  */

void
ipcp_write_transformation_summaries (void)
{
  struct output_block *ob;
  unsigned int count = 0;
  lto_symtab_encoder_t encoder;

  ob = create_output_block (LTO_section_ipcp_transform);
  encoder = ob->decl_state->symtab_node_encoder;
  ob->symbol = NULL;

  for (int i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      if (!cnode)
	continue;
      ipcp_transformation *ts = ipcp_get_transformation_summary (cnode);
      if (useful_ipcp_transformation_info_p (ts)
	  && lto_symtab_encoder_encode_body_p (encoder, cnode))
	count++;
    }

  streamer_write_uhwi (ob, count);

  for (int i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      if (!cnode)
	continue;
      ipcp_transformation *ts = ipcp_get_transformation_summary (cnode);
      if (useful_ipcp_transformation_info_p (ts)
	  && lto_symtab_encoder_encode_body_p (encoder, cnode))
	write_ipcp_transformation_info (ob, cnode, ts);
    }
  ipa_write_return_summaries (ob);
  produce_asm (ob);
  destroy_output_block (ob);
}

/* Read replacements section in file FILE_DATA of length LEN with data
   DATA.  */

static void
read_replacements_section (struct lto_file_decl_data *file_data,
			   const char *data,
			   size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  class data_in *data_in;
  unsigned int i;
  unsigned int count;

  lto_input_block ib_main ((const char *) data + main_offset,
			   header->main_size, file_data);

  data_in = lto_data_in_create (file_data, (const char *) data + string_offset,
				header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));
      read_ipcp_transformation_info (&ib_main, node, data_in);
    }
  ipa_read_return_summaries (&ib_main, file_data, data_in);
  lto_free_section_data (file_data, LTO_section_jump_functions, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read IPA-CP aggregate replacements.  */

void
ipcp_read_transformation_summaries (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data
	= lto_get_summary_section_data (file_data, LTO_section_ipcp_transform,
					&len);
      if (data)
        read_replacements_section (file_data, data, len);
    }
}

/* Adjust the aggregate replacements in TS to reflect any parameter removals
   which might have already taken place.  If after adjustments there are no
   aggregate replacements left, the m_agg_values will be set to NULL.  In other
   cases, it may be shrunk.  */

static void
adjust_agg_replacement_values (cgraph_node *node, ipcp_transformation *ts)
{
  clone_info *cinfo = clone_info::get (node);
  if (!cinfo || !cinfo->param_adjustments)
    return;

  auto_vec<int, 16> new_indices;
  cinfo->param_adjustments->get_updated_indices (&new_indices);
  bool removed_item = false;
  unsigned dst_index = 0;
  unsigned count = ts->m_agg_values->length ();
  for (unsigned i = 0; i < count; i++)
    {
      ipa_argagg_value *v = &(*ts->m_agg_values)[i];
      gcc_checking_assert (v->index >= 0);

      int new_idx = -1;
      if ((unsigned) v->index < new_indices.length ())
	new_idx = new_indices[v->index];

      if (new_idx >= 0)
	{
	  v->index = new_idx;
	  if (removed_item)
	    (*ts->m_agg_values)[dst_index] = *v;
	  dst_index++;
	}
      else
	removed_item = true;
    }

  if (dst_index == 0)
    {
      ggc_free (ts->m_agg_values);
      ts->m_agg_values = NULL;
    }
  else if (removed_item)
    ts->m_agg_values->truncate (dst_index);

  return;
}

/* Dominator walker driving the ipcp modification phase.  */

class ipcp_modif_dom_walker : public dom_walker
{
public:
  ipcp_modif_dom_walker (struct ipa_func_body_info *fbi,
			 vec<ipa_param_descriptor, va_gc> *descs,
			 ipcp_transformation *ts, bool *sc)
    : dom_walker (CDI_DOMINATORS), m_fbi (fbi), m_descriptors (descs),
      m_ts (ts), m_something_changed (sc) {}

  edge before_dom_children (basic_block) final override;
  bool cleanup_eh ()
    { return gimple_purge_all_dead_eh_edges (m_need_eh_cleanup); }

private:
  struct ipa_func_body_info *m_fbi;
  vec<ipa_param_descriptor, va_gc> *m_descriptors;
  ipcp_transformation *m_ts;
  bool *m_something_changed;
  auto_bitmap m_need_eh_cleanup;
};

edge
ipcp_modif_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree rhs, val, t;
      HOST_WIDE_INT bit_offset;
      poly_int64 size;
      int index;
      bool by_ref, vce;

      if (!gimple_assign_load_p (stmt))
	continue;
      rhs = gimple_assign_rhs1 (stmt);
      if (!is_gimple_reg_type (TREE_TYPE (rhs)))
	continue;

      vce = false;
      t = rhs;
      while (handled_component_p (t))
	{
	  /* V_C_E can do things like convert an array of integers to one
	     bigger integer and similar things we do not handle below.  */
	  if (TREE_CODE (t) == VIEW_CONVERT_EXPR)
	    {
	      vce = true;
	      break;
	    }
	  t = TREE_OPERAND (t, 0);
	}
      if (vce)
	continue;

      if (!ipa_load_from_parm_agg (m_fbi, m_descriptors, stmt, rhs, &index,
				   &bit_offset, &size, &by_ref))
	continue;
      unsigned unit_offset = bit_offset / BITS_PER_UNIT;
      ipa_argagg_value_list avl (m_ts);
      tree v = avl.get_value (index, unit_offset, by_ref);

      if (!v
	  || maybe_ne (tree_to_poly_int64 (TYPE_SIZE (TREE_TYPE (v))), size))
	continue;

      gcc_checking_assert (is_gimple_ip_invariant (v));
      if (!useless_type_conversion_p (TREE_TYPE (rhs), TREE_TYPE (v)))
	{
	  if (fold_convertible_p (TREE_TYPE (rhs), v))
	    val = fold_build1 (NOP_EXPR, TREE_TYPE (rhs), v);
	  else if (TYPE_SIZE (TREE_TYPE (rhs))
		   == TYPE_SIZE (TREE_TYPE (v)))
	    val = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (rhs), v);
	  else
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "    const ");
		  print_generic_expr (dump_file, v);
		  fprintf (dump_file, "  can't be converted to type of ");
		  print_generic_expr (dump_file, rhs);
		  fprintf (dump_file, "\n");
		}
	      continue;
	    }
	}
      else
	val = v;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Modifying stmt:\n  ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      gimple_assign_set_rhs_from_tree (&gsi, val);
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "into:\n  ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}

      *m_something_changed = true;
      if (maybe_clean_eh_stmt (stmt))
	bitmap_set_bit (m_need_eh_cleanup, bb->index);
    }
  return NULL;
}

/* If IPA-CP discovered a constant in parameter PARM at OFFSET of a given SIZE
   - whether passed by reference or not is given by BY_REF - return that
   constant.  Otherwise return NULL_TREE.  The is supposed to be used only
   after clone materialization and transformation is done (because it asserts
   that killed constants have been pruned). */

tree
ipcp_get_aggregate_const (struct function *func, tree parm, bool by_ref,
			  HOST_WIDE_INT bit_offset, HOST_WIDE_INT bit_size)
{
  cgraph_node *node = cgraph_node::get (func->decl);
  ipcp_transformation *ts = ipcp_get_transformation_summary (node);

  if (!ts || !ts->m_agg_values)
    return NULL_TREE;

  int index = ts->get_param_index (func->decl, parm);
  if (index < 0)
    return NULL_TREE;

  ipa_argagg_value_list avl (ts);
  unsigned unit_offset = bit_offset / BITS_PER_UNIT;
  const ipa_argagg_value *av = avl.get_elt (index, unit_offset);
  if (!av || av->by_ref != by_ref)
    return NULL_TREE;
  gcc_assert (!av->killed);
  tree v = av->value;
  if (!v
      || maybe_ne (tree_to_poly_int64 (TYPE_SIZE (TREE_TYPE (v))), bit_size))
    return NULL_TREE;

  return v;
}

/* Return true if we have recorded VALUE and MASK about PARM.
   Set VALUE and MASk accordingly.  */

bool
ipcp_get_parm_bits (tree parm, tree *value, widest_int *mask)
{
  cgraph_node *cnode = cgraph_node::get (current_function_decl);
  ipcp_transformation *ts = ipcp_get_transformation_summary (cnode);
  if (!ts
      || vec_safe_length (ts->m_vr) == 0
      || !ipa_vr_supported_type_p (TREE_TYPE (parm)))
    return false;

  int i = ts->get_param_index (current_function_decl, parm);
  if (i < 0)
    return false;
  clone_info *cinfo = clone_info::get (cnode);
  if (cinfo && cinfo->param_adjustments)
    {
      i = cinfo->param_adjustments->get_original_index (i);
      if (i < 0)
	return false;
    }

  vec<ipa_vr, va_gc> &vr = *ts->m_vr;
  if (!vr[i].known_p ())
    return false;
  value_range tmp;
  vr[i].get_vrange (tmp);
  if (tmp.undefined_p () || tmp.varying_p ())
    return false;
  irange_bitmask bm;
  bm = tmp.get_bitmask ();
  *mask = widest_int::from (bm.mask (), TYPE_SIGN (TREE_TYPE (parm)));
  *value = wide_int_to_tree (TREE_TYPE (parm), bm.value ());
  return true;
}

/* Update value range of formal parameters of NODE as described in TS.  */

static void
ipcp_update_vr (struct cgraph_node *node, ipcp_transformation *ts)
{
  if (vec_safe_is_empty (ts->m_vr))
    return;
  const vec<ipa_vr, va_gc> &vr = *ts->m_vr;
  unsigned count = vr.length ();
  if (!count)
    return;

  auto_vec<int, 16> new_indices;
  bool need_remapping = false;
  clone_info *cinfo = clone_info::get (node);
  if (cinfo && cinfo->param_adjustments)
    {
      cinfo->param_adjustments->get_updated_indices (&new_indices);
      need_remapping = true;
    }
  auto_vec <tree, 16> parm_decls;
  push_function_arg_decls (&parm_decls, node->decl);

  for (unsigned i = 0; i < count; ++i)
    {
      tree parm;
      int remapped_idx;
      if (need_remapping)
	{
	  if (i >= new_indices.length ())
	    continue;
	  remapped_idx = new_indices[i];
	  if (remapped_idx < 0)
	    continue;
	}
      else
	remapped_idx = i;

      parm = parm_decls[remapped_idx];

      gcc_checking_assert (parm);
      tree ddef = ssa_default_def (DECL_STRUCT_FUNCTION (node->decl), parm);

      if (!ddef || !is_gimple_reg (parm))
	continue;

      if (vr[i].known_p ())
	{
	  value_range tmp;
	  vr[i].get_vrange (tmp);

	  if (!tmp.undefined_p () && !tmp.varying_p ())
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Setting value range of param %u "
			   "(now %i) ", i, remapped_idx);
		  tmp.dump (dump_file);
		  fprintf (dump_file, "]\n");
		}
	      set_range_info (ddef, tmp);

	      if (POINTER_TYPE_P (TREE_TYPE (parm))
		  && opt_for_fn (node->decl, flag_ipa_bit_cp))
		{
		  irange_bitmask bm = tmp.get_bitmask ();
		  unsigned tem = bm.mask ().to_uhwi ();
		  unsigned HOST_WIDE_INT bitpos = bm.value ().to_uhwi ();
		  unsigned align = tem & -tem;
		  unsigned misalign = bitpos & (align - 1);

		  if (align > 1)
		    {
		      if (dump_file)
			{
			  fprintf (dump_file,
				   "Adjusting mask for param %u to ", i);
			  print_hex (bm.mask (), dump_file);
			  fprintf (dump_file, "\n");
			}

		      if (dump_file)
			fprintf (dump_file,
				 "Adjusting align: %u, misalign: %u\n",
				 align, misalign);

		      unsigned old_align, old_misalign;
		      struct ptr_info_def *pi = get_ptr_info (ddef);
		      bool old_known = get_ptr_info_alignment (pi, &old_align,
							       &old_misalign);

		      if (old_known && old_align > align)
			{
			  if (dump_file)
			    {
			      fprintf (dump_file,
				       "But alignment was already %u.\n",
				       old_align);
			      if ((old_misalign & (align - 1)) != misalign)
				fprintf (dump_file,
					 "old_misalign (%u) and misalign "
					 "(%u) mismatch\n",
					 old_misalign, misalign);
			    }
			  continue;
			}

		      if (dump_file
			  && old_known
			  && ((misalign & (old_align - 1)) != old_misalign))
			fprintf (dump_file,
				 "old_misalign (%u) and misalign (%u) "
				 "mismatch\n",
				 old_misalign, misalign);

		      set_ptr_info_alignment (pi, align, misalign);
		    }
		}
	      else if (dump_file && INTEGRAL_TYPE_P (TREE_TYPE (parm)))
		{
		  irange &r = as_a<irange> (tmp);
		  irange_bitmask bm = r.get_bitmask ();
		  unsigned prec = TYPE_PRECISION (TREE_TYPE (parm));
		  if (wi::ne_p (bm.mask (), wi::shwi (-1, prec)))
		    {
		      fprintf (dump_file,
			       "Adjusting mask for param %u to ", i);
		      print_hex (bm.mask (), dump_file);
		      fprintf (dump_file, "\n");
		    }
		}
	    }
	}
    }
}

/* IPCP transformation phase doing propagation of aggregate values.  */

unsigned int
ipcp_transform_function (struct cgraph_node *node)
{
  struct ipa_func_body_info fbi;
  int param_count;

  gcc_checking_assert (cfun);
  gcc_checking_assert (current_function_decl);

  if (dump_file)
    fprintf (dump_file, "Modification phase of node %s\n",
	     node->dump_name ());

  ipcp_transformation *ts = ipcp_get_transformation_summary (node);
  if (!ts
      || (vec_safe_is_empty (ts->m_agg_values)
	  && vec_safe_is_empty (ts->m_vr)))
    return 0;

  ts->maybe_create_parm_idx_map (cfun->decl);
  ipcp_update_vr (node, ts);
  if (vec_safe_is_empty (ts->m_agg_values))
      return 0;
  param_count = count_formal_params (node->decl);
  if (param_count == 0)
    return 0;

  adjust_agg_replacement_values (node, ts);
  if (vec_safe_is_empty (ts->m_agg_values))
    {
      if (dump_file)
	fprintf (dump_file, "  All affected aggregate parameters were either "
		 "removed or converted into scalars, phase done.\n");
      return 0;
    }
  if (dump_file)
    {
      fprintf (dump_file, "     Aggregate replacements:");
      ipa_argagg_value_list avs (ts);
      avs.dump (dump_file);
    }

  fbi.node = node;
  fbi.info = NULL;
  fbi.bb_infos = vNULL;
  fbi.bb_infos.safe_grow_cleared (last_basic_block_for_fn (cfun), true);
  fbi.param_count = param_count;
  fbi.aa_walk_budget = opt_for_fn (node->decl, param_ipa_max_aa_steps);

  vec<ipa_param_descriptor, va_gc> *descriptors = NULL;
  vec_safe_grow_cleared (descriptors, param_count, true);
  ipa_populate_param_decls (node, *descriptors);
  bool modified_mem_access = false;
  calculate_dominance_info (CDI_DOMINATORS);
  ipcp_modif_dom_walker walker (&fbi, descriptors, ts, &modified_mem_access);
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  free_dominance_info (CDI_DOMINATORS);
  bool cfg_changed = walker.cleanup_eh ();

  int i;
  struct ipa_bb_info *bi;
  FOR_EACH_VEC_ELT (fbi.bb_infos, i, bi)
    free_ipa_bb_info (bi);
  fbi.bb_infos.release ();

  ts->remove_argaggs_if ([](const ipa_argagg_value &v)
    {
      return v.killed;
    });

  vec_free (descriptors);
  if (cfg_changed)
    delete_unreachable_blocks_update_callgraph (node, false);

  return modified_mem_access ? TODO_update_ssa_only_virtuals : 0;
}

/* Record that current function return value range is VAL.  */

void
ipa_record_return_value_range (value_range val)
{
  ipa_record_return_value_range_1
	  (cgraph_node::get (current_function_decl), val);
}

/* Return true if value range of DECL is known and if so initialize RANGE.  */

bool
ipa_return_value_range (value_range &range, tree decl)
{
  cgraph_node *n = cgraph_node::get (decl);
  if (!n || !ipa_return_value_sum)
    return false;
  enum availability avail;
  n = n->ultimate_alias_target (&avail);
  if (avail < AVAIL_AVAILABLE)
    return false;
  if (n->decl != decl && !useless_type_conversion_p (TREE_TYPE (decl), TREE_TYPE (n->decl)))
    return false;
  ipa_return_value_summary *v = ipa_return_value_sum->get (n);
  if (!v)
    return false;
  v->vr->get_vrange (range);
  return true;
}

/* Reset all state within ipa-prop.cc so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_prop_cc_finalize (void)
{
  if (function_insertion_hook_holder)
    symtab->remove_cgraph_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;

  if (ipa_edge_args_sum)
    ggc_delete (ipa_edge_args_sum);
  ipa_edge_args_sum = NULL;

  if (ipa_node_params_sum)
    ggc_delete (ipa_node_params_sum);
  ipa_node_params_sum = NULL;
}

/* Return true if the two pass_through components of two jump functions are
   known to be equivalent.  AGG_JF denotes whether they are part of aggregate
   functions or not.  The function can be used before the IPA phase of IPA-CP
   or inlining because it cannot cope with refdesc changes these passes can
   carry out.  */

static bool
ipa_agg_pass_through_jf_equivalent_p (ipa_pass_through_data *ipt1,
				      ipa_pass_through_data *ipt2,
				      bool agg_jf)

{
  gcc_assert (agg_jf ||
	      (!ipt1->refdesc_decremented && !ipt2->refdesc_decremented));
  if (ipt1->operation != ipt2->operation
      || ipt1->formal_id != ipt2->formal_id
      || (!agg_jf && (ipt1->agg_preserved != ipt2->agg_preserved)))
    return false;
  if (ipt1->operation != NOP_EXPR
      && (TYPE_MAIN_VARIANT (ipt1->op_type)
	  != TYPE_MAIN_VARIANT (ipt2->op_type)))
    return false;
  if (((ipt1->operand != NULL_TREE) != (ipt2->operand != NULL_TREE))
      || (ipt1->operand
	  && !values_equal_for_ipcp_p (ipt1->operand, ipt2->operand)))
    return false;
  return true;
}

/* Return true if the two aggregate jump functions are known to be equivalent.
   The function can be used before the IPA phase of IPA-CP or inlining because
   it cannot cope with refdesc changes these passes can carry out.  */

static bool
ipa_agg_jump_functions_equivalent_p (ipa_agg_jf_item *ajf1,
				     ipa_agg_jf_item *ajf2)
{
  if (ajf1->offset != ajf2->offset
      || ajf1->jftype != ajf2->jftype
      || !types_compatible_p (ajf1->type, ajf2->type))
    return false;

  switch (ajf1->jftype)
    {
    case IPA_JF_CONST:
      if (!values_equal_for_ipcp_p (ajf1->value.constant,
				    ajf2->value.constant))
	return false;
      break;
    case IPA_JF_PASS_THROUGH:
      {
	ipa_pass_through_data *ipt1 = &ajf1->value.pass_through;
	ipa_pass_through_data *ipt2 = &ajf2->value.pass_through;
	if (!ipa_agg_pass_through_jf_equivalent_p (ipt1, ipt2, true))
	  return false;
      }
      break;
    case IPA_JF_LOAD_AGG:
      {
	ipa_load_agg_data *ila1 = &ajf1->value.load_agg;
	ipa_load_agg_data *ila2 = &ajf2->value.load_agg;
	if (!ipa_agg_pass_through_jf_equivalent_p (&ila1->pass_through,
						   &ila2->pass_through, true))
	  return false;
	if (ila1->offset != ila2->offset
	    || ila1->by_ref != ila2->by_ref
	    || !types_compatible_p (ila1->type, ila2->type))
	  return false;
      }
      break;
    default:
	gcc_unreachable ();
    }
  return true;
}

/* Return true if the two jump functions are known to be equivalent.  The
   function can be used before the IPA phase of IPA-CP or inlining because it
   cannot cope with refdesc changes these passes can carry out.  */

bool
ipa_jump_functions_equivalent_p (ipa_jump_func *jf1, ipa_jump_func *jf2)
{
  if (jf1->type != jf2->type)
    return false;

  switch (jf1->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_CONST:
      {
	tree cst1 = ipa_get_jf_constant (jf1);
	tree cst2 = ipa_get_jf_constant (jf2);
	if (!values_equal_for_ipcp_p (cst1, cst2))
	  return false;

	ipa_cst_ref_desc *rd1 = jfunc_rdesc_usable (jf1);
	ipa_cst_ref_desc *rd2 = jfunc_rdesc_usable (jf2);
	if (rd1 && rd2)
	  {
	    gcc_assert (rd1->refcount == 1
			&& rd2->refcount == 1);
	    gcc_assert (!rd1->next_duplicate && !rd2->next_duplicate);
	  }
	else if (rd1)
	  return false;
	else if (rd2)
	  return false;
      }
      break;
    case IPA_JF_PASS_THROUGH:
      {
	ipa_pass_through_data *ipt1 = &jf1->value.pass_through;
	ipa_pass_through_data *ipt2 = &jf2->value.pass_through;
	if (!ipa_agg_pass_through_jf_equivalent_p (ipt1, ipt2, false))
	  return false;
      }
      break;
    case IPA_JF_ANCESTOR:
      {
	ipa_ancestor_jf_data *ia1 = &jf1->value.ancestor;
	ipa_ancestor_jf_data *ia2 = &jf2->value.ancestor;

	if (ia1->formal_id != ia2->formal_id
	    || ia1->agg_preserved != ia2->agg_preserved
	    || ia1->keep_null != ia2->keep_null
	    || ia1->offset != ia2->offset)
	  return false;
      }
      break;
    default:
      gcc_unreachable ();
    }

  if (((jf1->m_vr != nullptr) != (jf2->m_vr != nullptr))
      || (jf1->m_vr && !jf1->m_vr->equal_p (*jf2->m_vr)))
    return false;

  unsigned alen = vec_safe_length (jf1->agg.items);
  if (vec_safe_length (jf2->agg.items) != alen)
    return false;

  if (!alen)
    return true;

  if (jf1->agg.by_ref != jf2->agg.by_ref)
    return false;

  for (unsigned i = 0 ; i < alen; i++)
    if (!ipa_agg_jump_functions_equivalent_p (&(*jf1->agg.items)[i],
					      &(*jf2->agg.items)[i]))
      return false;

  return true;
}

#include "gt-ipa-prop.h"
