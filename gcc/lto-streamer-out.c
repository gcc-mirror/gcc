/* Write the GIMPLE representation to a file stream.

   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>
   Re-implemented by Diego Novillo <dnovillo@google.com>

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
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic-core.h"
#include "except.h"
#include "vec.h"
#include "lto-symtab.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "gimple-streamer.h"
#include "tree-streamer.h"
#include "streamer-hooks.h"


/* Clear the line info stored in DATA_IN.  */

static void
clear_line_info (struct output_block *ob)
{
  ob->current_file = NULL;
  ob->current_line = 0;
  ob->current_col = 0;
}


/* Create the output block and return it.  SECTION_TYPE is
   LTO_section_function_body or LTO_static_initializer.  */

struct output_block *
create_output_block (enum lto_section_type section_type)
{
  struct output_block *ob = XCNEW (struct output_block);

  ob->section_type = section_type;
  ob->decl_state = lto_get_out_decl_state ();
  ob->main_stream = XCNEW (struct lto_output_stream);
  ob->string_stream = XCNEW (struct lto_output_stream);
  ob->writer_cache = streamer_tree_cache_create ();

  if (section_type == LTO_section_function_body)
    ob->cfg_stream = XCNEW (struct lto_output_stream);

  clear_line_info (ob);

  ob->string_hash_table = htab_create (37, hash_string_slot_node,
				       eq_string_slot_node, NULL);
  gcc_obstack_init (&ob->obstack);

  return ob;
}


/* Destroy the output block OB.  */

void
destroy_output_block (struct output_block *ob)
{
  enum lto_section_type section_type = ob->section_type;

  htab_delete (ob->string_hash_table);

  free (ob->main_stream);
  free (ob->string_stream);
  if (section_type == LTO_section_function_body)
    free (ob->cfg_stream);

  streamer_tree_cache_delete (ob->writer_cache);
  obstack_free (&ob->obstack, NULL);

  free (ob);
}


/* Look up NODE in the type table and write the index for it to OB.  */

static void
output_type_ref (struct output_block *ob, tree node)
{
  streamer_write_record_start (ob, LTO_type_ref);
  lto_output_type_ref_index (ob->decl_state, ob->main_stream, node);
}


/* Return true if tree node T is written to various tables.  For these
   nodes, we sometimes want to write their phyiscal representation
   (via lto_output_tree), and sometimes we need to emit an index
   reference into a table (via lto_output_tree_ref).  */

static bool
tree_is_indexable (tree t)
{
  if (TREE_CODE (t) == PARM_DECL)
    return false;
  else if (TREE_CODE (t) == VAR_DECL && decl_function_context (t)
	   && !TREE_STATIC (t))
    return false;
  /* If this is a decl generated for block local externs for
     debug info generation, stream it unshared alongside BLOCK_VARS.  */
  else if (VAR_OR_FUNCTION_DECL_P (t)
	   /* ???  The following tests are a literal match on what
	      c-decl.c:pop_scope does.  */
	   && TREE_PUBLIC (t)
	   && DECL_EXTERNAL (t)
	   && DECL_CONTEXT (t)
	   && TREE_CODE (DECL_CONTEXT (t)) == FUNCTION_DECL)
    return false;
  /* Variably modified types need to be streamed alongside function
     bodies because they can refer to local entities.  Together with
     them we have to localize their members as well.
     ???  In theory that includes non-FIELD_DECLs as well.  */
  else if (TYPE_P (t)
	   && variably_modified_type_p (t, NULL_TREE))
    return false;
  else if (TREE_CODE (t) == FIELD_DECL
	   && variably_modified_type_p (DECL_CONTEXT (t), NULL_TREE))
    return false;
  else
    return (TYPE_P (t) || DECL_P (t) || TREE_CODE (t) == SSA_NAME);
}


/* Output info about new location into bitpack BP.
   After outputting bitpack, lto_output_location_data has
   to be done to output actual data.  */

static inline void
lto_output_location_bitpack (struct bitpack_d *bp,
			     struct output_block *ob,
			     location_t loc)
{
  expanded_location xloc;

  bp_pack_value (bp, loc == UNKNOWN_LOCATION, 1);
  if (loc == UNKNOWN_LOCATION)
    return;

  xloc = expand_location (loc);

  bp_pack_value (bp, ob->current_file != xloc.file, 1);
  if (ob->current_file != xloc.file)
    bp_pack_var_len_unsigned (bp,
	                      streamer_string_index (ob, xloc.file,
						     strlen (xloc.file) + 1,
						     true));
  ob->current_file = xloc.file;

  bp_pack_value (bp, ob->current_line != xloc.line, 1);
  if (ob->current_line != xloc.line)
    bp_pack_var_len_unsigned (bp, xloc.line);
  ob->current_line = xloc.line;

  bp_pack_value (bp, ob->current_col != xloc.column, 1);
  if (ob->current_col != xloc.column)
    bp_pack_var_len_unsigned (bp, xloc.column);
  ob->current_col = xloc.column;
}


/* Emit location LOC to output block OB.
   If the output_location streamer hook exists, call it.
   Otherwise, when bitpack is handy, it is more space efficient to call
   lto_output_location_bitpack with existing bitpack.  */

void
lto_output_location (struct output_block *ob, location_t loc)
{
  if (streamer_hooks.output_location)
    streamer_hooks.output_location (ob, loc);
  else
    {
      struct bitpack_d bp = bitpack_create (ob->main_stream);
      lto_output_location_bitpack (&bp, ob, loc);
      streamer_write_bitpack (&bp);
    }
}


/* If EXPR is an indexable tree node, output a reference to it to
   output block OB.  Otherwise, output the physical representation of
   EXPR to OB.  */

static void
lto_output_tree_ref (struct output_block *ob, tree expr)
{
  enum tree_code code;

  if (TYPE_P (expr))
    {
      output_type_ref (ob, expr);
      return;
    }

  code = TREE_CODE (expr);
  switch (code)
    {
    case SSA_NAME:
      streamer_write_record_start (ob, LTO_ssa_name_ref);
      streamer_write_uhwi (ob, SSA_NAME_VERSION (expr));
      break;

    case FIELD_DECL:
      streamer_write_record_start (ob, LTO_field_decl_ref);
      lto_output_field_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case FUNCTION_DECL:
      streamer_write_record_start (ob, LTO_function_decl_ref);
      lto_output_fn_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case VAR_DECL:
    case DEBUG_EXPR_DECL:
      gcc_assert (decl_function_context (expr) == NULL || TREE_STATIC (expr));
      streamer_write_record_start (ob, LTO_global_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case CONST_DECL:
      streamer_write_record_start (ob, LTO_const_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case IMPORTED_DECL:
      gcc_assert (decl_function_context (expr) == NULL);
      streamer_write_record_start (ob, LTO_imported_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case TYPE_DECL:
      streamer_write_record_start (ob, LTO_type_decl_ref);
      lto_output_type_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case NAMESPACE_DECL:
      streamer_write_record_start (ob, LTO_namespace_decl_ref);
      lto_output_namespace_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case LABEL_DECL:
      streamer_write_record_start (ob, LTO_label_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case RESULT_DECL:
      streamer_write_record_start (ob, LTO_result_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case TRANSLATION_UNIT_DECL:
      streamer_write_record_start (ob, LTO_translation_unit_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    default:
      /* No other node is indexable, so it should have been handled by
	 lto_output_tree.  */
      gcc_unreachable ();
    }
}


/* Return true if EXPR is a tree node that can be written to disk.  */

static inline bool
lto_is_streamable (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  /* Notice that we reject SSA_NAMEs as well.  We only emit the SSA
     name version in lto_output_tree_ref (see output_ssa_names).  */
  return !is_lang_specific (expr)
	 && code != SSA_NAME
	 && code != CALL_EXPR
	 && code != LANG_TYPE
	 && code != MODIFY_EXPR
	 && code != INIT_EXPR
	 && code != TARGET_EXPR
	 && code != BIND_EXPR
	 && code != WITH_CLEANUP_EXPR
	 && code != STATEMENT_LIST
	 && code != OMP_CLAUSE
	 && code != OPTIMIZATION_NODE
	 && (code == CASE_LABEL_EXPR
	     || code == DECL_EXPR
	     || TREE_CODE_CLASS (code) != tcc_statement);
}


/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree (struct output_block *ob, tree expr, bool ref_p)
{
  struct bitpack_d bp;

  if (!lto_is_streamable (expr))
    internal_error ("tree code %qs is not supported in LTO streams",
	            tree_code_name[TREE_CODE (expr)]);

  /* Write the header, containing everything needed to materialize
     EXPR on the reading side.  */
  streamer_write_tree_header (ob, expr);

  /* Pack all the non-pointer fields in EXPR into a bitpack and write
     the resulting bitpack.  */
  bp = bitpack_create (ob->main_stream);
  streamer_pack_tree_bitfields (&bp, expr);
  streamer_write_bitpack (&bp);

  /* Write all the pointer fields in EXPR.  */
  streamer_write_tree_body (ob, expr, ref_p);

  /* Write any LTO-specific data to OB.  */
  if (DECL_P (expr)
      && TREE_CODE (expr) != FUNCTION_DECL
      && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
    {
      /* Handle DECL_INITIAL for symbols.  */
      tree initial = DECL_INITIAL (expr);
      if (TREE_CODE (expr) == VAR_DECL
	  && (TREE_STATIC (expr) || DECL_EXTERNAL (expr))
	  && initial)
	{
	  lto_varpool_encoder_t varpool_encoder;
	  struct varpool_node *vnode;

	  varpool_encoder = ob->decl_state->varpool_node_encoder;
	  vnode = varpool_get_node (expr);
	  if (!vnode)
	    initial = error_mark_node;
	  else if (!lto_varpool_encoder_encode_initializer_p (varpool_encoder,
							      vnode))
	    initial = NULL;
	}

      stream_write_tree (ob, initial, ref_p);
    }

  /* Mark the end of EXPR.  */
  streamer_write_zero (ob);
}


/* Emit the physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  */

void
lto_output_tree (struct output_block *ob, tree expr, bool ref_p)
{
  unsigned ix;
  bool existed_p;

  if (expr == NULL_TREE)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  if (ref_p && tree_is_indexable (expr))
    {
      lto_output_tree_ref (ob, expr);
      return;
    }

  /* INTEGER_CST nodes are special because they need their original type
     to be materialized by the reader (to implement TYPE_CACHED_VALUES).  */
  if (TREE_CODE (expr) == INTEGER_CST)
    {
      streamer_write_integer_cst (ob, expr, ref_p);
      return;
    }

  existed_p = streamer_tree_cache_insert (ob->writer_cache, expr, &ix);
  if (existed_p)
    {
      /* If a node has already been streamed out, make sure that
	 we don't write it more than once.  Otherwise, the reader
	 will instantiate two different nodes for the same object.  */
      streamer_write_record_start (ob, LTO_tree_pickle_reference);
      streamer_write_uhwi (ob, ix);
      streamer_write_enum (ob->main_stream, LTO_tags, LTO_NUM_TAGS,
			   lto_tree_code_to_tag (TREE_CODE (expr)));
    }
  else if (streamer_handle_as_builtin_p (expr))
    {
      /* MD and NORMAL builtins do not need to be written out
	 completely as they are always instantiated by the
	 compiler on startup.  The only builtins that need to
	 be written out are BUILT_IN_FRONTEND.  For all other
	 builtins, we simply write the class and code.  */
      streamer_write_builtin (ob, expr);
    }
  else
    {
      /* This is the first time we see EXPR, write its fields
	 to OB.  */
      lto_write_tree (ob, expr, ref_p);
    }
}


/* Output to OB a list of try/catch handlers starting with FIRST.  */

static void
output_eh_try_list (struct output_block *ob, eh_catch first)
{
  eh_catch n;

  for (n = first; n; n = n->next_catch)
    {
      streamer_write_record_start (ob, LTO_eh_catch);
      stream_write_tree (ob, n->type_list, true);
      stream_write_tree (ob, n->filter_list, true);
      stream_write_tree (ob, n->label, true);
    }

  streamer_write_record_start (ob, LTO_null);
}


/* Output EH region R in function FN to OB.  CURR_RN is the slot index
   that is being emitted in FN->EH->REGION_ARRAY.  This is used to
   detect EH region sharing.  */

static void
output_eh_region (struct output_block *ob, eh_region r)
{
  enum LTO_tags tag;

  if (r == NULL)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  if (r->type == ERT_CLEANUP)
    tag = LTO_ert_cleanup;
  else if (r->type == ERT_TRY)
    tag = LTO_ert_try;
  else if (r->type == ERT_ALLOWED_EXCEPTIONS)
    tag = LTO_ert_allowed_exceptions;
  else if (r->type == ERT_MUST_NOT_THROW)
    tag = LTO_ert_must_not_throw;
  else
    gcc_unreachable ();

  streamer_write_record_start (ob, tag);
  streamer_write_hwi (ob, r->index);

  if (r->outer)
    streamer_write_hwi (ob, r->outer->index);
  else
    streamer_write_zero (ob);

  if (r->inner)
    streamer_write_hwi (ob, r->inner->index);
  else
    streamer_write_zero (ob);

  if (r->next_peer)
    streamer_write_hwi (ob, r->next_peer->index);
  else
    streamer_write_zero (ob);

  if (r->type == ERT_TRY)
    {
      output_eh_try_list (ob, r->u.eh_try.first_catch);
    }
  else if (r->type == ERT_ALLOWED_EXCEPTIONS)
    {
      stream_write_tree (ob, r->u.allowed.type_list, true);
      stream_write_tree (ob, r->u.allowed.label, true);
      streamer_write_uhwi (ob, r->u.allowed.filter);
    }
  else if (r->type == ERT_MUST_NOT_THROW)
    {
      stream_write_tree (ob, r->u.must_not_throw.failure_decl, true);
      lto_output_location (ob, r->u.must_not_throw.failure_loc);
    }

  if (r->landing_pads)
    streamer_write_hwi (ob, r->landing_pads->index);
  else
    streamer_write_zero (ob);
}


/* Output landing pad LP to OB.  */

static void
output_eh_lp (struct output_block *ob, eh_landing_pad lp)
{
  if (lp == NULL)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  streamer_write_record_start (ob, LTO_eh_landing_pad);
  streamer_write_hwi (ob, lp->index);
  if (lp->next_lp)
    streamer_write_hwi (ob, lp->next_lp->index);
  else
    streamer_write_zero (ob);

  if (lp->region)
    streamer_write_hwi (ob, lp->region->index);
  else
    streamer_write_zero (ob);

  stream_write_tree (ob, lp->post_landing_pad, true);
}


/* Output the existing eh_table to OB.  */

static void
output_eh_regions (struct output_block *ob, struct function *fn)
{
  if (fn->eh && fn->eh->region_tree)
    {
      unsigned i;
      eh_region eh;
      eh_landing_pad lp;
      tree ttype;

      streamer_write_record_start (ob, LTO_eh_table);

      /* Emit the index of the root of the EH region tree.  */
      streamer_write_hwi (ob, fn->eh->region_tree->index);

      /* Emit all the EH regions in the region array.  */
      streamer_write_hwi (ob, VEC_length (eh_region, fn->eh->region_array));
      FOR_EACH_VEC_ELT (eh_region, fn->eh->region_array, i, eh)
	output_eh_region (ob, eh);

      /* Emit all landing pads.  */
      streamer_write_hwi (ob, VEC_length (eh_landing_pad, fn->eh->lp_array));
      FOR_EACH_VEC_ELT (eh_landing_pad, fn->eh->lp_array, i, lp)
	output_eh_lp (ob, lp);

      /* Emit all the runtime type data.  */
      streamer_write_hwi (ob, VEC_length (tree, fn->eh->ttype_data));
      FOR_EACH_VEC_ELT (tree, fn->eh->ttype_data, i, ttype)
	stream_write_tree (ob, ttype, true);

      /* Emit the table of action chains.  */
      if (targetm.arm_eabi_unwinder)
	{
	  tree t;
	  streamer_write_hwi (ob, VEC_length (tree,
				              fn->eh->ehspec_data.arm_eabi));
	  FOR_EACH_VEC_ELT (tree, fn->eh->ehspec_data.arm_eabi, i, t)
	    stream_write_tree (ob, t, true);
	}
      else
	{
	  uchar c;
	  streamer_write_hwi (ob, VEC_length (uchar,
				              fn->eh->ehspec_data.other));
	  FOR_EACH_VEC_ELT (uchar, fn->eh->ehspec_data.other, i, c)
	    streamer_write_char_stream (ob->main_stream, c);
	}
    }

  /* The LTO_null either terminates the record or indicates that there
     are no eh_records at all.  */
  streamer_write_record_start (ob, LTO_null);
}


/* Output all of the active ssa names to the ssa_names stream.  */

static void
output_ssa_names (struct output_block *ob, struct function *fn)
{
  unsigned int i, len;

  len = VEC_length (tree, SSANAMES (fn));
  streamer_write_uhwi (ob, len);

  for (i = 1; i < len; i++)
    {
      tree ptr = VEC_index (tree, SSANAMES (fn), i);

      if (ptr == NULL_TREE
	  || SSA_NAME_IN_FREE_LIST (ptr)
	  || !is_gimple_reg (ptr))
	continue;

      streamer_write_uhwi (ob, i);
      streamer_write_char_stream (ob->main_stream,
				  SSA_NAME_IS_DEFAULT_DEF (ptr));
      stream_write_tree (ob, SSA_NAME_VAR (ptr), true);
    }

  streamer_write_zero (ob);
}


/* Output the cfg.  */

static void
output_cfg (struct output_block *ob, struct function *fn)
{
  struct lto_output_stream *tmp_stream = ob->main_stream;
  basic_block bb;

  ob->main_stream = ob->cfg_stream;

  streamer_write_enum (ob->main_stream, profile_status_d, PROFILE_LAST,
		       profile_status_for_function (fn));

  /* Output the number of the highest basic block.  */
  streamer_write_uhwi (ob, last_basic_block_for_function (fn));

  FOR_ALL_BB_FN (bb, fn)
    {
      edge_iterator ei;
      edge e;

      streamer_write_hwi (ob, bb->index);

      /* Output the successors and the edge flags.  */
      streamer_write_uhwi (ob, EDGE_COUNT (bb->succs));
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  streamer_write_uhwi (ob, e->dest->index);
	  streamer_write_hwi (ob, e->probability);
	  streamer_write_hwi (ob, e->count);
	  streamer_write_uhwi (ob, e->flags);
	}
    }

  streamer_write_hwi (ob, -1);

  bb = ENTRY_BLOCK_PTR;
  while (bb->next_bb)
    {
      streamer_write_hwi (ob, bb->next_bb->index);
      bb = bb->next_bb;
    }

  streamer_write_hwi (ob, -1);

  ob->main_stream = tmp_stream;
}


/* Create the header in the file using OB.  If the section type is for
   a function, set FN to the decl for that function.  */

void
produce_asm (struct output_block *ob, tree fn)
{
  enum lto_section_type section_type = ob->section_type;
  struct lto_function_header header;
  char *section_name;
  struct lto_output_stream *header_stream;

  if (section_type == LTO_section_function_body)
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn));
      section_name = lto_get_section_name (section_type, name, NULL);
    }
  else
    section_name = lto_get_section_name (section_type, NULL, NULL);

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header is stream computed here.  */
  memset (&header, 0, sizeof (struct lto_function_header));

  /* Write the header.  */
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = section_type;

  header.compressed_size = 0;

  if (section_type == LTO_section_function_body)
    header.cfg_size = ob->cfg_stream->total_size;
  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof header);
  lto_write_stream (header_stream);
  free (header_stream);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  if (section_type == LTO_section_function_body)
    lto_write_stream (ob->cfg_stream);
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();
}


/* Output the base body of struct function FN using output block OB.  */

static void
output_struct_function_base (struct output_block *ob, struct function *fn)
{
  struct bitpack_d bp;
  unsigned i;
  tree t;

  /* Output the static chain and non-local goto save area.  */
  stream_write_tree (ob, fn->static_chain_decl, true);
  stream_write_tree (ob, fn->nonlocal_goto_save_area, true);

  /* Output all the local variables in the function.  */
  streamer_write_hwi (ob, VEC_length (tree, fn->local_decls));
  FOR_EACH_VEC_ELT (tree, fn->local_decls, i, t)
    stream_write_tree (ob, t, true);

  /* Output the function start and end loci.  */
  lto_output_location (ob, fn->function_start_locus);
  lto_output_location (ob, fn->function_end_locus);

  /* Output current IL state of the function.  */
  streamer_write_uhwi (ob, fn->curr_properties);

  /* Write all the attributes for FN.  */
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, fn->is_thunk, 1);
  bp_pack_value (&bp, fn->has_local_explicit_reg_vars, 1);
  bp_pack_value (&bp, fn->after_tree_profile, 1);
  bp_pack_value (&bp, fn->returns_pcc_struct, 1);
  bp_pack_value (&bp, fn->returns_struct, 1);
  bp_pack_value (&bp, fn->can_throw_non_call_exceptions, 1);
  bp_pack_value (&bp, fn->always_inline_functions_inlined, 1);
  bp_pack_value (&bp, fn->after_inlining, 1);
  bp_pack_value (&bp, fn->stdarg, 1);
  bp_pack_value (&bp, fn->has_nonlocal_label, 1);
  bp_pack_value (&bp, fn->calls_alloca, 1);
  bp_pack_value (&bp, fn->calls_setjmp, 1);
  bp_pack_value (&bp, fn->va_list_fpr_size, 8);
  bp_pack_value (&bp, fn->va_list_gpr_size, 8);
  streamer_write_bitpack (&bp);
}


/* Output the body of function NODE->DECL.  */

static void
output_function (struct cgraph_node *node)
{
  tree function;
  struct function *fn;
  basic_block bb;
  struct output_block *ob;

  function = node->decl;
  fn = DECL_STRUCT_FUNCTION (function);
  ob = create_output_block (LTO_section_function_body);

  clear_line_info (ob);
  ob->cgraph_node = node;

  gcc_assert (current_function_decl == NULL_TREE && cfun == NULL);

  /* Set current_function_decl and cfun.  */
  current_function_decl = function;
  push_cfun (fn);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  streamer_write_record_start (ob, LTO_function);

  output_struct_function_base (ob, fn);

  /* Output the head of the arguments list.  */
  stream_write_tree (ob, DECL_ARGUMENTS (function), true);

  /* Output all the SSA names used in the function.  */
  output_ssa_names (ob, fn);

  /* Output any exception handling regions.  */
  output_eh_regions (ob, fn);

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  stream_write_tree (ob, DECL_INITIAL (function), true);

  /* We will renumber the statements.  The code that does this uses
     the same ordering that we use for serializing them so we can use
     the same code on the other end and not have to write out the
     statement numbers.  We do not assign UIDs to PHIs here because
     virtual PHIs get re-computed on-the-fly which would make numbers
     inconsistent.  */
  set_gimple_stmt_max_uid (cfun, 0);
  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
    }

  /* Output the code for the function.  */
  FOR_ALL_BB_FN (bb, fn)
    output_bb (ob, bb, fn);

  /* The terminator for this function.  */
  streamer_write_record_start (ob, LTO_null);

  output_cfg (ob, fn);

  /* Create a section to hold the pickled output of this function.   */
  produce_asm (ob, function);

  destroy_output_block (ob);

  current_function_decl = NULL;
  pop_cfun ();
}


/* Used to pass data to trivally_defined_alias callback.  */
struct sets {
  cgraph_node_set set;
  varpool_node_set vset;
};


/* Return true if alias pair P belongs to the set of cgraph nodes in
   SET.  If P is a an alias for a VAR_DECL, it can always be emitted.
   However, for FUNCTION_DECL aliases, we should only output the pair
   if it belongs to a function whose cgraph node is in SET.
   Otherwise, the LTRANS phase will get into trouble when finalizing
   aliases because the alias will refer to a function not defined in
   the file processed by LTRANS.  */

static bool
trivally_defined_alias (tree decl ATTRIBUTE_UNUSED,
			tree target, void *data)
{
  struct sets *set = (struct sets *) data;
  struct cgraph_node *fnode = NULL;
  struct varpool_node *vnode = NULL;

  fnode = cgraph_node_for_asm (target);
  if (fnode)
    return cgraph_node_in_set_p (fnode, set->set);
  vnode = varpool_node_for_asm (target);
  return vnode && varpool_node_in_set_p (vnode, set->vset);
}

/* Return true if alias pair P should be output in the current
   partition contains cgrpah nodes SET and varpool nodes VSET.
   DEFINED is set of all aliases whose targets are defined in
   the partition.

   Normal aliases are output when they are defined, while WEAKREF
   aliases are output when they are used.  */

static bool
output_alias_pair_p (alias_pair *p, symbol_alias_set_t *defined,
		     cgraph_node_set set, varpool_node_set vset)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;

  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)))
    {
      if (TREE_CODE (p->decl) == VAR_DECL)
	{
	  vnode = varpool_get_node (p->decl);
	  return (vnode
		  && referenced_from_this_partition_p (&vnode->ref_list, set, vset));
	}
      node = cgraph_get_node (p->decl);
      return (node
	      && (referenced_from_this_partition_p (&node->ref_list, set, vset)
		  || reachable_from_this_partition_p (node, set)));
    }
  else
    return symbol_alias_set_contains (defined, p->decl);
}

/* Output any unreferenced global symbol defined in SET, alias pairs
   and labels.  */

static void
output_unreferenced_globals (cgraph_node_set set, varpool_node_set vset)
{
  struct output_block *ob;
  alias_pair *p;
  unsigned i;
  symbol_alias_set_t *defined;
  struct sets setdata;

  setdata.set = set;
  setdata.vset = vset;

  ob = create_output_block (LTO_section_static_initializer);
  ob->cgraph_node = NULL;

  clear_line_info (ob);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  /* We really need to propagate in both directoins:
     for normal aliases we propagate from first defined alias to
     all aliases defined based on it.  For weakrefs we propagate in
     the oposite direction.  */
  defined = propagate_aliases_backward (trivally_defined_alias, &setdata);

  /* Emit the alias pairs for the nodes in SET.  */
  FOR_EACH_VEC_ELT (alias_pair, alias_pairs, i, p)
    if (output_alias_pair_p (p, defined, set, vset))
      {
	stream_write_tree (ob, p->decl, true);
	stream_write_tree (ob, p->target, true);
      }
  symbol_alias_set_destroy (defined);

  streamer_write_record_start (ob, LTO_null);

  produce_asm (ob, NULL);
  destroy_output_block (ob);
}


/* Emit toplevel asms.  */

void
lto_output_toplevel_asms (void)
{
  struct output_block *ob;
  struct cgraph_asm_node *can;
  char *section_name;
  struct lto_output_stream *header_stream;
  struct lto_asm_header header;

  if (! cgraph_asm_nodes)
    return;

  ob = create_output_block (LTO_section_asm);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  for (can = cgraph_asm_nodes; can; can = can->next)
    {
      streamer_write_string_cst (ob, ob->main_stream, can->asm_str);
      streamer_write_hwi (ob, can->order);
    }

  streamer_write_string_cst (ob, ob->main_stream, NULL_TREE);

  section_name = lto_get_section_name (LTO_section_asm, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header stream is computed here.  */
  memset (&header, 0, sizeof (header));

  /* Write the header.  */
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = LTO_section_asm;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof (header));
  lto_write_stream (header_stream);
  free (header_stream);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  destroy_output_block (ob);
}


/* Copy the function body of NODE without deserializing. */

static void
copy_function (struct cgraph_node *node)
{
  tree function = node->decl;
  struct lto_file_decl_data *file_data = node->local.lto_file_data;
  struct lto_output_stream *output_stream = XCNEW (struct lto_output_stream);
  const char *data;
  size_t len;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function));
  char *section_name =
    lto_get_section_name (LTO_section_function_body, name, NULL);
  size_t i, j;
  struct lto_in_decl_state *in_state;
  struct lto_out_decl_state *out_state = lto_get_out_decl_state ();

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_section_data (file_data, LTO_section_function_body,
                               name, &len);
  gcc_assert (data);

  /* Do a bit copy of the function body.  */
  lto_output_data_stream (output_stream, data, len);
  lto_write_stream (output_stream);

  /* Copy decls. */
  in_state =
    lto_get_function_in_decl_state (node->local.lto_file_data, function);
  gcc_assert (in_state);

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      size_t n = in_state->streams[i].size;
      tree *trees = in_state->streams[i].trees;
      struct lto_tree_ref_encoder *encoder = &(out_state->streams[i]);

      /* The out state must have the same indices and the in state.
	 So just copy the vector.  All the encoders in the in state
	 must be empty where we reach here. */
      gcc_assert (lto_tree_ref_encoder_size (encoder) == 0);
      for (j = 0; j < n; j++)
	VEC_safe_push (tree, heap, encoder->trees, trees[j]);
      encoder->next_index = n;
    }

  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len);
  free (output_stream);
  lto_end_section ();
}


/* Main entry point from the pass manager.  */

static void
lto_output (cgraph_node_set set, varpool_node_set vset)
{
  struct cgraph_node *node;
  struct lto_out_decl_state *decl_state;
#ifdef ENABLE_CHECKING
  bitmap output = lto_bitmap_alloc ();
#endif
  int i, n_nodes;
  lto_cgraph_encoder_t encoder = lto_get_out_decl_state ()->cgraph_node_encoder;

  /* Initialize the streamer.  */
  lto_streamer_init ();

  n_nodes = lto_cgraph_encoder_size (encoder);
  /* Process only the functions with bodies.  */
  for (i = 0; i < n_nodes; i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      if (lto_cgraph_encoder_encode_body_p (encoder, node)
	  && !node->alias
	  && !node->thunk.thunk_p)
	{
#ifdef ENABLE_CHECKING
	  gcc_assert (!bitmap_bit_p (output, DECL_UID (node->decl)));
	  bitmap_set_bit (output, DECL_UID (node->decl));
#endif
	  decl_state = lto_new_out_decl_state ();
	  lto_push_out_decl_state (decl_state);
	  if (gimple_has_body_p (node->decl))
	    output_function (node);
	  else
	    copy_function (node);
	  gcc_assert (lto_get_out_decl_state () == decl_state);
	  lto_pop_out_decl_state ();
	  lto_record_function_out_decl_state (node->decl, decl_state);
	}
    }

  /* Emit the callgraph after emitting function bodies.  This needs to
     be done now to make sure that all the statements in every function
     have been renumbered so that edges can be associated with call
     statements using the statement UIDs.  */
  output_cgraph (set, vset);

#ifdef ENABLE_CHECKING
  lto_bitmap_free (output);
#endif
}

struct ipa_opt_pass_d pass_ipa_lto_gimple_out =
{
 {
  IPA_PASS,
  "lto_gimple_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,		                	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_GIMPLE_OUT,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 lto_output,           			/* write_summary */
 NULL,		         		/* read_summary */
 lto_output,           			/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};


/* Write each node in encoded by ENCODER to OB, as well as those reachable
   from it and required for correct representation of its semantics.
   Each node in ENCODER must be a global declaration or a type.  A node
   is written only once, even if it appears multiple times in the
   vector.  Certain transitively-reachable nodes, such as those
   representing expressions, may be duplicated, but such nodes
   must not appear in ENCODER itself.  */

static void
write_global_stream (struct output_block *ob,
		     struct lto_tree_ref_encoder *encoder)
{
  tree t;
  size_t index;
  const size_t size = lto_tree_ref_encoder_size (encoder);

  for (index = 0; index < size; index++)
    {
      t = lto_tree_ref_encoder_get_tree (encoder, index);
      if (!streamer_tree_cache_lookup (ob->writer_cache, t, NULL))
	stream_write_tree (ob, t, false);
    }
}


/* Write a sequence of indices into the globals vector corresponding
   to the trees in ENCODER.  These are used by the reader to map the
   indices used to refer to global entities within function bodies to
   their referents.  */

static void
write_global_references (struct output_block *ob,
			 struct lto_output_stream *ref_stream,
 			 struct lto_tree_ref_encoder *encoder)
{
  tree t;
  uint32_t index;
  const uint32_t size = lto_tree_ref_encoder_size (encoder);

  /* Write size as 32-bit unsigned. */
  lto_output_data_stream (ref_stream, &size, sizeof (int32_t));

  for (index = 0; index < size; index++)
    {
      uint32_t slot_num;

      t = lto_tree_ref_encoder_get_tree (encoder, index);
      streamer_tree_cache_lookup (ob->writer_cache, t, &slot_num);
      gcc_assert (slot_num != (unsigned)-1);
      lto_output_data_stream (ref_stream, &slot_num, sizeof slot_num);
    }
}


/* Write all the streams in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

void
lto_output_decl_state_streams (struct output_block *ob,
			       struct lto_out_decl_state *state)
{
  int i;

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_stream (ob, &state->streams[i]);
}


/* Write all the references in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

void
lto_output_decl_state_refs (struct output_block *ob,
			    struct lto_output_stream *out_stream,
			    struct lto_out_decl_state *state)
{
  unsigned i;
  uint32_t ref;
  tree decl;

  /* Write reference to FUNCTION_DECL.  If there is not function,
     write reference to void_type_node. */
  decl = (state->fn_decl) ? state->fn_decl : void_type_node;
  streamer_tree_cache_lookup (ob->writer_cache, decl, &ref);
  gcc_assert (ref != (unsigned)-1);
  lto_output_data_stream (out_stream, &ref, sizeof (uint32_t));

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_references (ob, out_stream, &state->streams[i]);
}


/* Return the written size of STATE. */

static size_t
lto_out_decl_state_written_size (struct lto_out_decl_state *state)
{
  int i;
  size_t size;

  size = sizeof (int32_t);	/* fn_ref. */
  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      size += sizeof (int32_t); /* vector size. */
      size += (lto_tree_ref_encoder_size (&state->streams[i])
	       * sizeof (int32_t));
    }
  return size;
}


/* Write symbol T into STREAM in CACHE. SEEN specifies symbols we wrote
   so far.  */

static void
write_symbol (struct streamer_tree_cache_d *cache,
	      struct lto_output_stream *stream,
	      tree t, struct pointer_set_t *seen, bool alias)
{
  const char *name;
  enum gcc_plugin_symbol_kind kind;
  enum gcc_plugin_symbol_visibility visibility;
  unsigned slot_num;
  uint64_t size;
  const char *comdat;
  unsigned char c;

  /* None of the following kinds of symbols are needed in the
     symbol table.  */
  if (!TREE_PUBLIC (t)
      || is_builtin_fn (t)
      || DECL_ABSTRACT (t)
      || TREE_CODE (t) == RESULT_DECL)
    return;

  gcc_assert (TREE_CODE (t) == VAR_DECL
	      || TREE_CODE (t) == FUNCTION_DECL);

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));

  /* This behaves like assemble_name_raw in varasm.c, performing the
     same name manipulations that ASM_OUTPUT_LABELREF does. */
  name = IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name) (name));

  if (pointer_set_contains (seen, name))
    return;
  pointer_set_insert (seen, name);

  streamer_tree_cache_lookup (cache, t, &slot_num);
  gcc_assert (slot_num != (unsigned)-1);

  if (DECL_EXTERNAL (t))
    {
      if (DECL_WEAK (t))
	kind = GCCPK_WEAKUNDEF;
      else
	kind = GCCPK_UNDEF;
    }
  else
    {
      if (DECL_WEAK (t))
	kind = GCCPK_WEAKDEF;
      else if (DECL_COMMON (t))
	kind = GCCPK_COMMON;
      else
	kind = GCCPK_DEF;

      /* When something is defined, it should have node attached.  */
      gcc_assert (alias || TREE_CODE (t) != VAR_DECL
		  || varpool_get_node (t)->finalized);
      gcc_assert (alias || TREE_CODE (t) != FUNCTION_DECL
		  || (cgraph_get_node (t)
		      && cgraph_get_node (t)->analyzed));
    }

  /* Imitate what default_elf_asm_output_external do.
     When symbol is external, we need to output it with DEFAULT visibility
     when compiling with -fvisibility=default, while with HIDDEN visibility
     when symbol has attribute (visibility("hidden")) specified.
     targetm.binds_local_p check DECL_VISIBILITY_SPECIFIED and gets this
     right. */
     
  if (DECL_EXTERNAL (t)
      && !targetm.binds_local_p (t))
    visibility = GCCPV_DEFAULT;
  else
    switch (DECL_VISIBILITY(t))
      {
      case VISIBILITY_DEFAULT:
	visibility = GCCPV_DEFAULT;
	break;
      case VISIBILITY_PROTECTED:
	visibility = GCCPV_PROTECTED;
	break;
      case VISIBILITY_HIDDEN:
	visibility = GCCPV_HIDDEN;
	break;
      case VISIBILITY_INTERNAL:
	visibility = GCCPV_INTERNAL;
	break;
      }

  if (kind == GCCPK_COMMON
      && DECL_SIZE (t)
      && TREE_CODE (DECL_SIZE (t)) == INTEGER_CST)
    {
      size = (HOST_BITS_PER_WIDE_INT >= 64)
	? (uint64_t) int_size_in_bytes (TREE_TYPE (t))
	: (((uint64_t) TREE_INT_CST_HIGH (DECL_SIZE_UNIT (t))) << 32)
		| TREE_INT_CST_LOW (DECL_SIZE_UNIT (t));
    }
  else
    size = 0;

  if (DECL_ONE_ONLY (t))
    comdat = IDENTIFIER_POINTER (DECL_COMDAT_GROUP (t));
  else
    comdat = "";

  lto_output_data_stream (stream, name, strlen (name) + 1);
  lto_output_data_stream (stream, comdat, strlen (comdat) + 1);
  c = (unsigned char) kind;
  lto_output_data_stream (stream, &c, 1);
  c = (unsigned char) visibility;
  lto_output_data_stream (stream, &c, 1);
  lto_output_data_stream (stream, &size, 8);
  lto_output_data_stream (stream, &slot_num, 4);
}


/* Write an IL symbol table to OB.
   SET and VSET are cgraph/varpool node sets we are outputting.  */

static void
produce_symtab (struct output_block *ob,
	        cgraph_node_set set, varpool_node_set vset)
{
  struct streamer_tree_cache_d *cache = ob->writer_cache;
  char *section_name = lto_get_section_name (LTO_section_symtab, NULL, NULL);
  struct pointer_set_t *seen;
  struct cgraph_node *node;
  struct varpool_node *vnode;
  struct lto_output_stream stream;
  lto_varpool_encoder_t varpool_encoder = ob->decl_state->varpool_node_encoder;
  lto_cgraph_encoder_t encoder = ob->decl_state->cgraph_node_encoder;
  int i;
  alias_pair *p;
  struct sets setdata;
  symbol_alias_set_t *defined;

  setdata.set = set;
  setdata.vset = vset;

  lto_begin_section (section_name, false);
  free (section_name);

  seen = pointer_set_create ();
  memset (&stream, 0, sizeof (stream));

  /* Write all functions. 
     First write all defined functions and then write all used functions.
     This is done so only to handle duplicated symbols in cgraph.  */
  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      if (DECL_EXTERNAL (node->decl))
	continue;
      if (DECL_COMDAT (node->decl)
	  && cgraph_comdat_can_be_unshared_p (node))
	continue;
      if ((node->alias && !node->thunk.alias) || node->global.inlined_to)
	continue;
      write_symbol (cache, &stream, node->decl, seen, false);
    }
  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      if (!DECL_EXTERNAL (node->decl))
	continue;
      /* We keep around unused extern inlines in order to be able to inline
	 them indirectly or via vtables.  Do not output them to symbol
	 table: they end up being undefined and just consume space.  */
      if (!node->address_taken && !node->callers)
	{
	  gcc_assert (node->analyzed);
	  gcc_assert (DECL_DECLARED_INLINE_P (node->decl));
	  continue;
	}
      if (DECL_COMDAT (node->decl)
	  && cgraph_comdat_can_be_unshared_p (node))
	continue;
      if ((node->alias && !node->thunk.alias) || node->global.inlined_to)
	continue;
      write_symbol (cache, &stream, node->decl, seen, false);
    }

  /* Write all variables.  */
  for (i = 0; i < lto_varpool_encoder_size (varpool_encoder); i++)
    {
      vnode = lto_varpool_encoder_deref (varpool_encoder, i);
      if (DECL_EXTERNAL (vnode->decl))
	continue;
      /* COMDAT virtual tables can be unshared.  Do not declare them
	 in the LTO symbol table to prevent linker from forcing them
	 into the output. */
      if (DECL_COMDAT (vnode->decl)
	  && !vnode->force_output
	  && vnode->finalized 
	  && DECL_VIRTUAL_P (vnode->decl))
	continue;
      if (vnode->alias && !vnode->alias_of)
	continue;
      write_symbol (cache, &stream, vnode->decl, seen, false);
    }
  for (i = 0; i < lto_varpool_encoder_size (varpool_encoder); i++)
    {
      vnode = lto_varpool_encoder_deref (varpool_encoder, i);
      if (!DECL_EXTERNAL (vnode->decl))
	continue;
      if (DECL_COMDAT (vnode->decl)
	  && !vnode->force_output
	  && vnode->finalized 
	  && DECL_VIRTUAL_P (vnode->decl))
	continue;
      if (vnode->alias && !vnode->alias_of)
	continue;
      write_symbol (cache, &stream, vnode->decl, seen, false);
    }

  /* Write all aliases.  */
  defined = propagate_aliases_backward (trivally_defined_alias, &setdata);
  FOR_EACH_VEC_ELT (alias_pair, alias_pairs, i, p)
    if (output_alias_pair_p (p, defined, set, vset))
      write_symbol (cache, &stream, p->decl, seen, true);
  symbol_alias_set_destroy (defined);

  lto_write_stream (&stream);
  pointer_set_destroy (seen);

  lto_end_section ();
}


/* This pass is run after all of the functions are serialized and all
   of the IPA passes have written their serialized forms.  This pass
   causes the vector of all of the global decls and types used from
   this file to be written in to a section that can then be read in to
   recover these on other side.  */

static void
produce_asm_for_decls (cgraph_node_set set, varpool_node_set vset)
{
  struct lto_out_decl_state *out_state;
  struct lto_out_decl_state *fn_out_state;
  struct lto_decl_header header;
  char *section_name;
  struct output_block *ob;
  struct lto_output_stream *header_stream, *decl_state_stream;
  unsigned idx, num_fns;
  size_t decl_state_size;
  int32_t num_decl_states;

  ob = create_output_block (LTO_section_decls);
  ob->global = true;

  /* Write out unreferenced globals, alias pairs and labels.  We defer
     doing this until now so that we can write out only what is
     needed.  */
  output_unreferenced_globals (set, vset);

  memset (&header, 0, sizeof (struct lto_decl_header));

  section_name = lto_get_section_name (LTO_section_decls, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  /* Write the global symbols.  */
  out_state = lto_get_out_decl_state ();
  num_fns = VEC_length (lto_out_decl_state_ptr, lto_function_decl_states);
  lto_output_decl_state_streams (ob, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      lto_output_decl_state_streams (ob, fn_out_state);
    }

  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = LTO_section_decls;

  /* Currently not used.  This field would allow us to preallocate
     the globals vector, so that it need not be resized as it is extended.  */
  header.num_nodes = -1;

  /* Compute the total size of all decl out states. */
  decl_state_size = sizeof (int32_t);
  decl_state_size += lto_out_decl_state_written_size (out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      decl_state_size += lto_out_decl_state_written_size (fn_out_state);
    }
  header.decl_state_size = decl_state_size;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof header);
  lto_write_stream (header_stream);
  free (header_stream);

  /* Write the main out-decl state, followed by out-decl states of
     functions. */
  decl_state_stream = ((struct lto_output_stream *)
		       xcalloc (1, sizeof (struct lto_output_stream)));
  num_decl_states = num_fns + 1;
  lto_output_data_stream (decl_state_stream, &num_decl_states,
			  sizeof (num_decl_states));
  lto_output_decl_state_refs (ob, decl_state_stream, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      lto_output_decl_state_refs (ob, decl_state_stream, fn_out_state);
    }
  lto_write_stream (decl_state_stream);
  free(decl_state_stream);

  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  /* Write the symbol table.  It is used by linker to determine dependencies
     and thus we can skip it for WPA.  */
  if (!flag_wpa)
    produce_symtab (ob, set, vset);

  /* Write command line opts.  */
  lto_write_options ();

  /* Deallocate memory and clean up.  */
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      lto_delete_out_decl_state (fn_out_state);
    }
  lto_cgraph_encoder_delete (ob->decl_state->cgraph_node_encoder);
  lto_varpool_encoder_delete (ob->decl_state->varpool_node_encoder);
  VEC_free (lto_out_decl_state_ptr, heap, lto_function_decl_states);
  lto_function_decl_states = NULL;
  destroy_output_block (ob);
}


struct ipa_opt_pass_d pass_ipa_lto_finish_out =
{
 {
  IPA_PASS,
  "lto_decls_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,        	                        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_DECL_OUT,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 produce_asm_for_decls,			/* write_summary */
 NULL,		         		/* read_summary */
 produce_asm_for_decls,			/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};
