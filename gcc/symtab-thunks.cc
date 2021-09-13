/* Support for thunks in symbol table.
   Copyright (C) 2003-2021 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "target.h"
#include "rtl.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "symbol-summary.h"
#include "symtab-thunks.h"
#include "lto-streamer.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "stor-layout.h"
#include "gimplify-me.h"
#include "varasm.h"
#include "output.h"
#include "cfg.h"
#include "cfghooks.h"
#include "gimple-ssa.h"
#include "gimple-fold.h"
#include "cfgloop.h"
#include "tree-into-ssa.h"
#include "tree-cfg.h"
#include "cfgcleanup.h"
#include "tree-pass.h"
#include "data-streamer.h"
#include "langhooks.h"

/* Used for vtable lookup in thunk adjusting.  */
static GTY (()) tree vtable_entry_type;
struct GTY (()) unprocessed_thunk
{
  cgraph_node *node;
  thunk_info *info;
};
/* To be PCH safe we store thunks into a vector before end of compilation
   unit.  */
static GTY (()) vec<unprocessed_thunk, va_gc> *thunks;

namespace {

/* Function summary for thunk_infos.  */
class GTY((user)) thunk_infos_t: public function_summary <thunk_info *>
{
public:
  thunk_infos_t (symbol_table *table, bool ggc):
    function_summary<thunk_info *> (table, ggc) { }

  /* Hook that is called by summary when a node is duplicated.  */
  virtual void duplicate (cgraph_node *node,
			  cgraph_node *node2,
			  thunk_info *data,
			  thunk_info *data2);
};

/* Duplication hook.  */
void
thunk_infos_t::duplicate (cgraph_node *, cgraph_node *,
			  thunk_info *src, thunk_info *dst)
{
  *dst = *src;
}

}  /* anon namespace  */

/* Return thunk_info possibly creating new one.  */
thunk_info *
thunk_info::get_create (cgraph_node *node)
{
  if (!symtab->m_thunks)
    {
      symtab->m_thunks
	 = new (ggc_alloc_no_dtor <thunk_infos_t> ())
	     thunk_infos_t (symtab, true);
      symtab->m_thunks->disable_insertion_hook ();
    }
  return symtab->m_thunks->get_create (node);
}

/* Stream out THIS to OB.  */
void
thunk_info::stream_out (lto_simple_output_block *ob)
{
  streamer_write_uhwi_stream
     (ob->main_stream,
      1 + (this_adjusting != 0) * 2
      + (virtual_offset_p != 0) * 4);
  streamer_write_uhwi_stream (ob->main_stream, fixed_offset);
  streamer_write_uhwi_stream (ob->main_stream, virtual_value);
  streamer_write_uhwi_stream (ob->main_stream, indirect_offset);
}

/* Stream in THIS from IB.  */
void
thunk_info::stream_in (class lto_input_block *ib)
{
  int type = streamer_read_uhwi (ib);
  fixed_offset = streamer_read_uhwi (ib);
  virtual_value = streamer_read_uhwi (ib);
  indirect_offset = streamer_read_uhwi (ib);

  this_adjusting = (type & 2);
  virtual_offset_p = (type & 4);
}

/* Dump THIS to F.  */
void
thunk_info::dump (FILE *f)
{
  if (alias)
    fprintf (f, "  of %s (asm:%s)",
	     lang_hooks.decl_printable_name (alias, 2),
	     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (alias)));
  fprintf (f, " fixed offset %i virtual value %i indirect_offset %i "
	      "has virtual offset %i\n",
	   (int)fixed_offset,
	   (int)virtual_value,
	   (int)indirect_offset,
	   (int)virtual_offset_p);
}

/* Hash THIS.  */
hashval_t
thunk_info::hash ()
{
  inchash::hash hstate;
  hstate.add_hwi (fixed_offset);
  hstate.add_hwi (virtual_value);
  hstate.add_flag (this_adjusting);
  hstate.add_flag (virtual_offset_p);
  return hstate.end ();
}

/* Add unprocessed thunk.  */
void
thunk_info::register_early (cgraph_node *node)
{
  unprocessed_thunk entry = {node, new (ggc_alloc <thunk_info> ()) thunk_info};
  *entry.info = *this;
  vec_safe_push (thunks, entry);
}

/* Attach recorded thunks to cgraph_nodes.
   All this is done only to avoid need to stream summaries to PCH.  */
void
thunk_info::process_early_thunks ()
{
  unprocessed_thunk *e;
  unsigned int i;
  if (!thunks)
    return;

  FOR_EACH_VEC_ELT (*thunks, i, e)
    {
      *thunk_info::get_create (e->node) = *e->info;
    }
  vec_free (thunks);
  thunks = NULL;
}

/* Adjust PTR by the constant FIXED_OFFSET, by the vtable offset indicated by
   VIRTUAL_OFFSET, and by the indirect offset indicated by INDIRECT_OFFSET, if
   it is non-null. THIS_ADJUSTING is nonzero for a this adjusting thunk and zero
   for a result adjusting thunk.  */
tree
thunk_adjust (gimple_stmt_iterator * bsi,
	      tree ptr, bool this_adjusting,
	      HOST_WIDE_INT fixed_offset, tree virtual_offset,
	      HOST_WIDE_INT indirect_offset)
{
  gassign *stmt;
  tree ret;

  if (this_adjusting
      && fixed_offset != 0)
    {
      stmt = gimple_build_assign
		(ptr, fold_build_pointer_plus_hwi_loc (input_location,
						       ptr,
						       fixed_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
    }

  if (!vtable_entry_type && (virtual_offset || indirect_offset != 0))
    {
      tree vfunc_type = make_node (FUNCTION_TYPE);
      TREE_TYPE (vfunc_type) = integer_type_node;
      TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
      layout_type (vfunc_type);

      vtable_entry_type = build_pointer_type (vfunc_type);
    }

  /* If there's a virtual offset, look up that value in the vtable and
     adjust the pointer again.  */
  if (virtual_offset)
    {
      tree vtabletmp;
      tree vtabletmp2;
      tree vtabletmp3;

      vtabletmp = create_tmp_reg
		    (build_pointer_type
			  (build_pointer_type (vtable_entry_type)), "vptr");

      /* The vptr is always at offset zero in the object.  */
      stmt = gimple_build_assign (vtabletmp,
				  build1 (NOP_EXPR, TREE_TYPE (vtabletmp),
					  ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Form the vtable address.  */
      vtabletmp2 = create_tmp_reg (TREE_TYPE (TREE_TYPE (vtabletmp)),
				     "vtableaddr");
      stmt = gimple_build_assign (vtabletmp2,
				  build_simple_mem_ref (vtabletmp));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Find the entry with the vcall offset.  */
      stmt = gimple_build_assign (vtabletmp2,
				  fold_build_pointer_plus_loc (input_location,
							       vtabletmp2,
							       virtual_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Get the offset itself.  */
      vtabletmp3 = create_tmp_reg (TREE_TYPE (TREE_TYPE (vtabletmp2)),
				     "vcalloffset");
      stmt = gimple_build_assign (vtabletmp3,
				  build_simple_mem_ref (vtabletmp2));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Adjust the `this' pointer.  */
      ptr = fold_build_pointer_plus_loc (input_location, ptr, vtabletmp3);
      ptr = force_gimple_operand_gsi (bsi, ptr, true, NULL_TREE, false,
				      GSI_CONTINUE_LINKING);
    }

  /* Likewise for an offset that is stored in the object that contains the
     vtable.  */
  if (indirect_offset != 0)
    {
      tree offset_ptr, offset_tree;

      /* Get the address of the offset.  */
      offset_ptr
	= create_tmp_reg (build_pointer_type
			  (build_pointer_type (vtable_entry_type)),
			  "offset_ptr");
      stmt = gimple_build_assign (offset_ptr,
				  build1 (NOP_EXPR, TREE_TYPE (offset_ptr),
					  ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      stmt = gimple_build_assign
	     (offset_ptr,
	      fold_build_pointer_plus_hwi_loc (input_location, offset_ptr,
					       indirect_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Get the offset itself.  */
      offset_tree = create_tmp_reg (TREE_TYPE (TREE_TYPE (offset_ptr)),
				    "offset");
      stmt = gimple_build_assign (offset_tree,
				  build_simple_mem_ref (offset_ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Adjust the `this' pointer.  */
      ptr = fold_build_pointer_plus_loc (input_location, ptr, offset_tree);
      ptr = force_gimple_operand_gsi (bsi, ptr, true, NULL_TREE, false,
				      GSI_CONTINUE_LINKING);
    }

  if (!this_adjusting
      && fixed_offset != 0)
    /* Adjust the pointer by the constant.  */
    {
      tree ptrtmp;

      if (VAR_P (ptr))
	ptrtmp = ptr;
      else
	{
	  ptrtmp = create_tmp_reg (TREE_TYPE (ptr), "ptr");
	  stmt = gimple_build_assign (ptrtmp, ptr);
	  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
	}
      ptr = fold_build_pointer_plus_hwi_loc (input_location,
					     ptrtmp, fixed_offset);
    }

  /* Emit the statement and gimplify the adjustment expression.  */
  ret = create_tmp_reg (TREE_TYPE (ptr), "adjusted_this");
  stmt = gimple_build_assign (ret, ptr);
  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

  return ret;
}

/* Expand thunk NODE to gimple if possible.
   When FORCE_GIMPLE_THUNK is true, gimple thunk is created and
   no assembler is produced.
   When OUTPUT_ASM_THUNK is true, also produce assembler for
   thunks that are not lowered.  */
bool
expand_thunk (cgraph_node *node, bool output_asm_thunks,
	      bool force_gimple_thunk)
{
  thunk_info *info = thunk_info::get (node);
  bool this_adjusting = info->this_adjusting;
  HOST_WIDE_INT fixed_offset = info->fixed_offset;
  HOST_WIDE_INT virtual_value = info->virtual_value;
  HOST_WIDE_INT indirect_offset = info->indirect_offset;
  tree virtual_offset = NULL;
  tree alias = node->callees->callee->decl;
  tree thunk_fndecl = node->decl;
  tree a;

  if (!force_gimple_thunk
      && this_adjusting
      && indirect_offset == 0
      && !DECL_EXTERNAL (alias)
      && !DECL_STATIC_CHAIN (alias)
      && targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					      virtual_value, alias))
    {
      tree fn_block;
      tree restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));

      if (!output_asm_thunks)
	{
	  node->analyzed = true;
	  return false;
	}

      if (in_lto_p)
	node->get_untransformed_body ();
      a = DECL_ARGUMENTS (thunk_fndecl);

      current_function_decl = thunk_fndecl;

      /* Ensure thunks are emitted in their correct sections.  */
      resolve_unique_section (thunk_fndecl, 0,
			      flag_function_sections);

      DECL_RESULT (thunk_fndecl)
	= build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
		      RESULT_DECL, 0, restype);
      DECL_CONTEXT (DECL_RESULT (thunk_fndecl)) = thunk_fndecl;

      /* The back end expects DECL_INITIAL to contain a BLOCK, so we
	 create one.  */
      fn_block = make_node (BLOCK);
      BLOCK_VARS (fn_block) = a;
      DECL_INITIAL (thunk_fndecl) = fn_block;
      BLOCK_SUPERCONTEXT (fn_block) = thunk_fndecl;
      allocate_struct_function (thunk_fndecl, false);
      init_function_start (thunk_fndecl);
      cfun->is_thunk = 1;
      insn_locations_init ();
      set_curr_insn_location (DECL_SOURCE_LOCATION (thunk_fndecl));
      prologue_location = curr_insn_location ();

      targetm.asm_out.output_mi_thunk (asm_out_file, thunk_fndecl,
				       fixed_offset, virtual_value, alias);

      insn_locations_finalize ();
      init_insn_lengths ();
      free_after_compilation (cfun);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      node->thunk = false;
      node->analyzed = false;
    }
  else if (stdarg_p (TREE_TYPE (thunk_fndecl)))
    {
      error ("generic thunk code fails for method %qD which uses %<...%>",
	     thunk_fndecl);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      node->analyzed = true;
      return false;
    }
  else
    {
      tree restype;
      basic_block bb, then_bb, else_bb, return_bb;
      gimple_stmt_iterator bsi;
      int nargs = 0;
      tree arg;
      int i;
      tree resdecl;
      tree restmp = NULL;

      gcall *call;
      greturn *ret;
      bool alias_is_noreturn = TREE_THIS_VOLATILE (alias);

      /* We may be called from expand_thunk that releases body except for
	 DECL_ARGUMENTS.  In this case force_gimple_thunk is true.  */
      if (in_lto_p && !force_gimple_thunk)
	node->get_untransformed_body ();

      /* We need to force DECL_IGNORED_P when the thunk is created
	 after early debug was run.  */
      if (force_gimple_thunk)
	DECL_IGNORED_P (thunk_fndecl) = 1;

      a = DECL_ARGUMENTS (thunk_fndecl);

      current_function_decl = thunk_fndecl;

      /* Ensure thunks are emitted in their correct sections.  */
      resolve_unique_section (thunk_fndecl, 0,
			      flag_function_sections);

      bitmap_obstack_initialize (NULL);

      if (info->virtual_offset_p)
	virtual_offset = size_int (virtual_value);

      /* Build the return declaration for the function.  */
      restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
      if (DECL_RESULT (thunk_fndecl) == NULL_TREE)
	{
	  resdecl = build_decl (input_location, RESULT_DECL, 0, restype);
	  DECL_ARTIFICIAL (resdecl) = 1;
	  DECL_IGNORED_P (resdecl) = 1;
	  DECL_CONTEXT (resdecl) = thunk_fndecl;
	  DECL_RESULT (thunk_fndecl) = resdecl;
	}
      else
	resdecl = DECL_RESULT (thunk_fndecl);

      profile_count cfg_count = node->count;
      if (!cfg_count.initialized_p ())
	cfg_count = profile_count::from_gcov_type
			 (BB_FREQ_MAX).guessed_local ();

      bb = then_bb = else_bb = return_bb
	= init_lowered_empty_function (thunk_fndecl, true, cfg_count);

      bsi = gsi_start_bb (bb);

      /* Build call to the function being thunked.  */
      if (!VOID_TYPE_P (restype)
	  && (!alias_is_noreturn
	      || TREE_ADDRESSABLE (restype)
	      || TREE_CODE (TYPE_SIZE_UNIT (restype)) != INTEGER_CST))
	{
	  if (DECL_BY_REFERENCE (resdecl))
	    {
	      restmp = gimple_fold_indirect_ref (resdecl);
	      if (!restmp)
		restmp = build2 (MEM_REF,
				 TREE_TYPE (TREE_TYPE (resdecl)),
				 resdecl,
				 build_int_cst (TREE_TYPE (resdecl), 0));
	    }
	  else if (!is_gimple_reg_type (restype))
	    {
	      if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl)))
		{
		  restmp = resdecl;

		  if (VAR_P (restmp))
		    {
		      add_local_decl (cfun, restmp);
		      BLOCK_VARS (DECL_INITIAL (current_function_decl))
			= restmp;
		    }
		}
	      else
		restmp = create_tmp_var (restype, "retval");
	    }
	  else
	    restmp = create_tmp_reg (restype, "retval");
	}

      for (arg = a; arg; arg = DECL_CHAIN (arg))
	nargs++;
      auto_vec<tree> vargs (nargs);
      i = 0;
      arg = a;
      if (this_adjusting)
	{
	  vargs.quick_push (thunk_adjust (&bsi, a, 1, fixed_offset,
					  virtual_offset, indirect_offset));
	  arg = DECL_CHAIN (a);
	  i = 1;
	}

      if (nargs)
	for (; i < nargs; i++, arg = DECL_CHAIN (arg))
	  {
	    tree tmp = arg;
	    DECL_NOT_GIMPLE_REG_P (arg) = 0;
	    if (!is_gimple_val (arg))
	      {
		tmp = create_tmp_reg (TYPE_MAIN_VARIANT
				      (TREE_TYPE (arg)), "arg");
		gimple *stmt = gimple_build_assign (tmp, arg);
		gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      }
	    vargs.quick_push (tmp);
	  }
      call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias), vargs);
      node->callees->call_stmt = call;
      gimple_call_set_from_thunk (call, true);
      if (DECL_STATIC_CHAIN (alias))
	{
	  tree p = DECL_STRUCT_FUNCTION (alias)->static_chain_decl;
	  tree type = TREE_TYPE (p);
	  tree decl = build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
				  PARM_DECL, create_tmp_var_name ("CHAIN"),
				  type);
	  DECL_ARTIFICIAL (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;
	  TREE_USED (decl) = 1;
	  DECL_CONTEXT (decl) = thunk_fndecl;
	  DECL_ARG_TYPE (decl) = type;
	  TREE_READONLY (decl) = 1;

	  struct function *sf = DECL_STRUCT_FUNCTION (thunk_fndecl);
	  sf->static_chain_decl = decl;

	  gimple_call_set_chain (call, decl);
	}

      /* Return slot optimization is always possible and in fact required to
	 return values with DECL_BY_REFERENCE.  */
      if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl))
	  && (!is_gimple_reg_type (TREE_TYPE (resdecl))
	      || DECL_BY_REFERENCE (resdecl)))
	gimple_call_set_return_slot_opt (call, true);

      if (restmp)
	{
	  gimple_call_set_lhs (call, restmp);
	  gcc_assert (useless_type_conversion_p (TREE_TYPE (restmp),
						 TREE_TYPE (TREE_TYPE (alias))));
	}
      gsi_insert_after (&bsi, call, GSI_NEW_STMT);
      if (!alias_is_noreturn)
	{
	  if (restmp && !this_adjusting
	      && (fixed_offset || virtual_offset))
	    {
	      tree true_label = NULL_TREE;

	      if (TREE_CODE (TREE_TYPE (restmp)) == POINTER_TYPE)
		{
		  gimple *stmt;
		  edge e;
		  /* If the return type is a pointer, we need to
		     protect against NULL.  We know there will be an
		     adjustment, because that's why we're emitting a
		     thunk.  */
		  then_bb = create_basic_block (NULL, bb);
		  then_bb->count = cfg_count - cfg_count.apply_scale (1, 16);
		  return_bb = create_basic_block (NULL, then_bb);
		  return_bb->count = cfg_count;
		  else_bb = create_basic_block (NULL, else_bb);
		  else_bb->count = cfg_count.apply_scale (1, 16);
		  add_bb_to_loop (then_bb, bb->loop_father);
		  add_bb_to_loop (return_bb, bb->loop_father);
		  add_bb_to_loop (else_bb, bb->loop_father);
		  remove_edge (single_succ_edge (bb));
		  true_label = gimple_block_label (then_bb);
		  stmt = gimple_build_cond (NE_EXPR, restmp,
					    build_zero_cst (TREE_TYPE (restmp)),
					    NULL_TREE, NULL_TREE);
		  gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
		  e = make_edge (bb, then_bb, EDGE_TRUE_VALUE);
		  e->probability = profile_probability::guessed_always ()
					.apply_scale (1, 16);
		  e = make_edge (bb, else_bb, EDGE_FALSE_VALUE);
		  e->probability = profile_probability::guessed_always ()
					.apply_scale (1, 16);
		  make_single_succ_edge (return_bb,
					 EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
		  make_single_succ_edge (then_bb, return_bb, EDGE_FALLTHRU);
		  e = make_edge (else_bb, return_bb, EDGE_FALLTHRU);
		  e->probability = profile_probability::always ();
		  bsi = gsi_last_bb (then_bb);
		}

	      restmp = thunk_adjust (&bsi, restmp, /*this_adjusting=*/0,
				     fixed_offset, virtual_offset,
				     indirect_offset);
	      if (true_label)
		{
		  gimple *stmt;
		  bsi = gsi_last_bb (else_bb);
		  stmt = gimple_build_assign (restmp,
					      build_zero_cst
						 (TREE_TYPE (restmp)));
		  gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
		  bsi = gsi_last_bb (return_bb);
		}
	    }
	  else
	    {
	      gimple_call_set_tail (call, true);
	      cfun->tail_call_marked = true;
	    }

	  /* Build return value.  */
	  if (!DECL_BY_REFERENCE (resdecl))
	    ret = gimple_build_return (restmp);
	  else
	    ret = gimple_build_return (resdecl);

	  gsi_insert_after (&bsi, ret, GSI_NEW_STMT);
	}
      else
	{
	  gimple_call_set_tail (call, true);
	  cfun->tail_call_marked = true;
	  remove_edge (single_succ_edge (bb));
	}

      cfun->gimple_df->in_ssa_p = true;
      update_max_bb_count ();
      profile_status_for_fn (cfun)
	= cfg_count.initialized_p () && cfg_count.ipa_p ()
	  ? PROFILE_READ : PROFILE_GUESSED;
      /* FIXME: C++ FE should stop setting TREE_ASM_WRITTEN on thunks.  */
      TREE_ASM_WRITTEN (thunk_fndecl) = false;
      delete_unreachable_blocks ();
      update_ssa (TODO_update_ssa);
      checking_verify_flow_info ();
      free_dominance_info (CDI_DOMINATORS);

      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      node->thunk = false;
      node->lowered = true;
      bitmap_obstack_release (NULL);
    }
  current_function_decl = NULL;
  set_cfun (NULL);
  return true;
}

void
symtab_thunks_cc_finalize (void)
{
  vtable_entry_type = NULL;
}

#include "gt-symtab-thunks.h"
