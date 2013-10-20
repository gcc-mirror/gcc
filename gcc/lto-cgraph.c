/* Write and read the cgraph to the memory mapped representation of a
   .o file.

   Copyright (C) 2009-2013 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "langhooks.h"
#include "basic-block.h"
#include "tree-ssa.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic-core.h"
#include "except.h"
#include "vec.h"
#include "timevar.h"
#include "pointer-set.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "tree-streamer.h"
#include "gcov-io.h"
#include "tree-pass.h"
#include "profile.h"
#include "context.h"
#include "pass_manager.h"
#include "ipa-utils.h"

static void output_cgraph_opt_summary (void);
static void input_cgraph_opt_summary (vec<symtab_node>  nodes);

/* Number of LDPR values known to GCC.  */
#define LDPR_NUM_KNOWN (LDPR_PREVAILING_DEF_IRONLY_EXP + 1)

/* All node orders are ofsetted by ORDER_BASE.  */
static int order_base;

/* Cgraph streaming is organized as set of record whose type
   is indicated by a tag.  */
enum LTO_symtab_tags
{
  /* Must leave 0 for the stopper.  */

  /* Cgraph node without body available.  */
  LTO_symtab_unavail_node = 1,
  /* Cgraph node with function body.  */
  LTO_symtab_analyzed_node,
  /* Cgraph edges.  */
  LTO_symtab_edge,
  LTO_symtab_indirect_edge,
  LTO_symtab_variable,
  LTO_symtab_last_tag
};

/* Create a new symtab encoder.
   if FOR_INPUT, the encoder allocate only datastructures needed
   to read the symtab.  */

lto_symtab_encoder_t
lto_symtab_encoder_new (bool for_input)
{
  lto_symtab_encoder_t encoder = XCNEW (struct lto_symtab_encoder_d);

  if (!for_input)
    encoder->map = pointer_map_create ();
  encoder->nodes.create (0);
  return encoder;
}


/* Delete ENCODER and its components.  */

void
lto_symtab_encoder_delete (lto_symtab_encoder_t encoder)
{
   encoder->nodes.release ();
   if (encoder->map)
     pointer_map_destroy (encoder->map);
   free (encoder);
}


/* Return the existing reference number of NODE in the symtab encoder in
   output block OB.  Assign a new reference if this is the first time
   NODE is encoded.  */

int
lto_symtab_encoder_encode (lto_symtab_encoder_t encoder,
			   symtab_node node)
{
  int ref;
  void **slot;

  if (!encoder->map)
    {
      lto_encoder_entry entry = {node, false, false, false};

      ref = encoder->nodes.length ();
      encoder->nodes.safe_push (entry);
      return ref;
    }

  slot = pointer_map_contains (encoder->map, node);
  if (!slot || !*slot)
    {
      lto_encoder_entry entry = {node, false, false, false};
      ref = encoder->nodes.length ();
      if (!slot)
        slot = pointer_map_insert (encoder->map, node);
      *slot = (void *) (intptr_t) (ref + 1);
      encoder->nodes.safe_push (entry);
    }
  else
    ref = (size_t) *slot - 1;

  return ref;
}

/* Remove NODE from encoder.  */

bool
lto_symtab_encoder_delete_node (lto_symtab_encoder_t encoder,
			        symtab_node node)
{
  void **slot, **last_slot;
  int index;
  lto_encoder_entry last_node;

  slot = pointer_map_contains (encoder->map, node);
  if (slot == NULL || !*slot)
    return false;

  index = (size_t) *slot - 1;
  gcc_checking_assert (encoder->nodes[index].node == node);

  /* Remove from vector. We do this by swapping node with the last element
     of the vector.  */
  last_node = encoder->nodes.pop ();
  if (last_node.node != node)
    {
      last_slot = pointer_map_contains (encoder->map, last_node.node);
      gcc_checking_assert (last_slot && *last_slot);
      *last_slot = (void *)(size_t) (index + 1);

      /* Move the last element to the original spot of NODE.  */
      encoder->nodes[index] = last_node;
    }

  /* Remove element from hash table.  */
  *slot = NULL;
  return true;
}


/* Return TRUE if we should encode initializer of NODE (if any).  */

bool
lto_symtab_encoder_encode_body_p (lto_symtab_encoder_t encoder,
				  struct cgraph_node *node)
{
  int index = lto_symtab_encoder_lookup (encoder, (symtab_node)node);
  return encoder->nodes[index].body;
}

/* Return TRUE if we should encode body of NODE (if any).  */

static void
lto_set_symtab_encoder_encode_body (lto_symtab_encoder_t encoder,
				    struct cgraph_node *node)
{
  int index = lto_symtab_encoder_encode (encoder, (symtab_node)node);
  gcc_checking_assert (encoder->nodes[index].node == (symtab_node)node);
  encoder->nodes[index].body = true;
}

/* Return TRUE if we should encode initializer of NODE (if any).  */

bool
lto_symtab_encoder_encode_initializer_p (lto_symtab_encoder_t encoder,
					 struct varpool_node *node)
{
  int index = lto_symtab_encoder_lookup (encoder, (symtab_node)node);
  if (index == LCC_NOT_FOUND)
    return false;
  return encoder->nodes[index].initializer;
}

/* Return TRUE if we should encode initializer of NODE (if any).  */

static void
lto_set_symtab_encoder_encode_initializer (lto_symtab_encoder_t encoder,
					   struct varpool_node *node)
{
  int index = lto_symtab_encoder_lookup (encoder, (symtab_node)node);
  encoder->nodes[index].initializer = true;
}

/* Return TRUE if we should encode initializer of NODE (if any).  */

bool
lto_symtab_encoder_in_partition_p (lto_symtab_encoder_t encoder,
				   symtab_node node)
{
  int index = lto_symtab_encoder_lookup (encoder, (symtab_node)node);
  if (index == LCC_NOT_FOUND)
    return false;
  return encoder->nodes[index].in_partition;
}

/* Return TRUE if we should encode body of NODE (if any).  */

void
lto_set_symtab_encoder_in_partition (lto_symtab_encoder_t encoder,
				     symtab_node node)
{
  int index = lto_symtab_encoder_encode (encoder, (symtab_node)node);
  encoder->nodes[index].in_partition = true;
}

/* Output the cgraph EDGE to OB using ENCODER.  */

static void
lto_output_edge (struct lto_simple_output_block *ob, struct cgraph_edge *edge,
		 lto_symtab_encoder_t encoder)
{
  unsigned int uid;
  intptr_t ref;
  struct bitpack_d bp;

  if (edge->indirect_unknown_callee)
    streamer_write_enum (ob->main_stream, LTO_symtab_tags, LTO_symtab_last_tag,
			 LTO_symtab_indirect_edge);
  else
    streamer_write_enum (ob->main_stream, LTO_symtab_tags, LTO_symtab_last_tag,
			 LTO_symtab_edge);

  ref = lto_symtab_encoder_lookup (encoder, (symtab_node)edge->caller);
  gcc_assert (ref != LCC_NOT_FOUND);
  streamer_write_hwi_stream (ob->main_stream, ref);

  if (!edge->indirect_unknown_callee)
    {
      ref = lto_symtab_encoder_lookup (encoder, (symtab_node)edge->callee);
      gcc_assert (ref != LCC_NOT_FOUND);
      streamer_write_hwi_stream (ob->main_stream, ref);
    }

  streamer_write_gcov_count_stream (ob->main_stream, edge->count);

  bp = bitpack_create (ob->main_stream);
  uid = (!gimple_has_body_p (edge->caller->symbol.decl)
	 ? edge->lto_stmt_uid : gimple_uid (edge->call_stmt) + 1);
  bp_pack_enum (&bp, cgraph_inline_failed_enum,
	        CIF_N_REASONS, edge->inline_failed);
  bp_pack_var_len_unsigned (&bp, uid);
  bp_pack_var_len_unsigned (&bp, edge->frequency);
  bp_pack_value (&bp, edge->indirect_inlining_edge, 1);
  bp_pack_value (&bp, edge->speculative, 1);
  bp_pack_value (&bp, edge->call_stmt_cannot_inline_p, 1);
  bp_pack_value (&bp, edge->can_throw_external, 1);
  if (edge->indirect_unknown_callee)
    {
      int flags = edge->indirect_info->ecf_flags;
      bp_pack_value (&bp, (flags & ECF_CONST) != 0, 1);
      bp_pack_value (&bp, (flags & ECF_PURE) != 0, 1);
      bp_pack_value (&bp, (flags & ECF_NORETURN) != 0, 1);
      bp_pack_value (&bp, (flags & ECF_MALLOC) != 0, 1);
      bp_pack_value (&bp, (flags & ECF_NOTHROW) != 0, 1);
      bp_pack_value (&bp, (flags & ECF_RETURNS_TWICE) != 0, 1);
      /* Flags that should not appear on indirect calls.  */
      gcc_assert (!(flags & (ECF_LOOPING_CONST_OR_PURE
			     | ECF_MAY_BE_ALLOCA
			     | ECF_SIBCALL
			     | ECF_LEAF
			     | ECF_NOVOPS)));
    }
  streamer_write_bitpack (&bp);
  if (edge->indirect_unknown_callee)
    {
      streamer_write_hwi_stream (ob->main_stream,
			         edge->indirect_info->common_target_id);
      if (edge->indirect_info->common_target_id)
	streamer_write_hwi_stream
	   (ob->main_stream, edge->indirect_info->common_target_probability);
    }
}

/* Return if LIST contain references from other partitions.  */

bool
referenced_from_other_partition_p (struct ipa_ref_list *list, lto_symtab_encoder_t encoder)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (list, i, ref); i++)
    {
      if (ref->referring->symbol.in_other_partition
          || !lto_symtab_encoder_in_partition_p (encoder, ref->referring))
	return true;
    }
  return false;
}

/* Return true when node is reachable from other partition.  */

bool
reachable_from_other_partition_p (struct cgraph_node *node, lto_symtab_encoder_t encoder)
{
  struct cgraph_edge *e;
  if (!node->symbol.definition)
    return false;
  if (node->global.inlined_to)
    return false;
  for (e = node->callers; e; e = e->next_caller)
    if (e->caller->symbol.in_other_partition
	|| !lto_symtab_encoder_in_partition_p (encoder, (symtab_node)e->caller))
      return true;
  return false;
}

/* Return if LIST contain references from other partitions.  */

bool
referenced_from_this_partition_p (struct ipa_ref_list *list,
				  lto_symtab_encoder_t encoder)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (list, i, ref); i++)
    if (lto_symtab_encoder_in_partition_p (encoder, ref->referring))
      return true;
  return false;
}

/* Return true when node is reachable from other partition.  */

bool
reachable_from_this_partition_p (struct cgraph_node *node, lto_symtab_encoder_t encoder)
{
  struct cgraph_edge *e;
  for (e = node->callers; e; e = e->next_caller)
    if (lto_symtab_encoder_in_partition_p (encoder, (symtab_node)e->caller))
      return true;
  return false;
}

/* Output the cgraph NODE to OB.  ENCODER is used to find the
   reference number of NODE->inlined_to.  SET is the set of nodes we
   are writing to the current file.  If NODE is not in SET, then NODE
   is a boundary of a cgraph_node_set and we pretend NODE just has a
   decl and no callees.  WRITTEN_DECLS is the set of FUNCTION_DECLs
   that have had their callgraph node written so far.  This is used to
   determine if NODE is a clone of a previously written node.  */

static void
lto_output_node (struct lto_simple_output_block *ob, struct cgraph_node *node,
		 lto_symtab_encoder_t encoder)
{
  unsigned int tag;
  struct bitpack_d bp;
  bool boundary_p;
  intptr_t ref;
  bool in_other_partition = false;
  struct cgraph_node *clone_of, *ultimate_clone_of;
  struct ipa_opt_pass_d *pass;
  int i;
  bool alias_p;

  boundary_p = !lto_symtab_encoder_in_partition_p (encoder, (symtab_node)node);

  if (node->symbol.analyzed && !boundary_p)
    tag = LTO_symtab_analyzed_node;
  else
    tag = LTO_symtab_unavail_node;

  streamer_write_enum (ob->main_stream, LTO_symtab_tags, LTO_symtab_last_tag,
		       tag);
  streamer_write_hwi_stream (ob->main_stream, node->symbol.order);

  /* In WPA mode, we only output part of the call-graph.  Also, we
     fake cgraph node attributes.  There are two cases that we care.

     Boundary nodes: There are nodes that are not part of SET but are
     called from within SET.  We artificially make them look like
     externally visible nodes with no function body.

     Cherry-picked nodes:  These are nodes we pulled from other
     translation units into SET during IPA-inlining.  We make them as
     local static nodes to prevent clashes with other local statics.  */
  if (boundary_p && node->symbol.analyzed && !DECL_EXTERNAL (node->symbol.decl))
    {
      /* Inline clones can not be part of boundary.  
         gcc_assert (!node->global.inlined_to);  

	 FIXME: At the moment they can be, when partition contains an inline
	 clone that is clone of inline clone from outside partition.  We can
	 reshape the clone tree and make other tree to be the root, but it
	 needs a bit extra work and will be promplty done by cgraph_remove_node
	 after reading back.  */
      in_other_partition = 1;
    }

  clone_of = node->clone_of;
  while (clone_of
	 && (ref = lto_symtab_encoder_lookup (encoder, (symtab_node)clone_of)) == LCC_NOT_FOUND)
    if (clone_of->prev_sibling_clone)
      clone_of = clone_of->prev_sibling_clone;
    else
      clone_of = clone_of->clone_of;

  /* See if body of the master function is output.  If not, we are seeing only
     an declaration and we do not need to pass down clone tree. */
  ultimate_clone_of = clone_of;
  while (ultimate_clone_of && ultimate_clone_of->clone_of)
    ultimate_clone_of = ultimate_clone_of->clone_of;

  if (clone_of && !lto_symtab_encoder_encode_body_p (encoder, ultimate_clone_of))
    clone_of = NULL;

  if (tag == LTO_symtab_analyzed_node)
    gcc_assert (clone_of || !node->clone_of);
  if (!clone_of)
    streamer_write_hwi_stream (ob->main_stream, LCC_NOT_FOUND);
  else
    streamer_write_hwi_stream (ob->main_stream, ref);


  lto_output_fn_decl_index (ob->decl_state, ob->main_stream, node->symbol.decl);
  streamer_write_gcov_count_stream (ob->main_stream, node->count);
  streamer_write_hwi_stream (ob->main_stream, node->count_materialization_scale);

  streamer_write_hwi_stream (ob->main_stream,
			     node->ipa_transforms_to_apply.length ());
  FOR_EACH_VEC_ELT (node->ipa_transforms_to_apply, i, pass)
    streamer_write_hwi_stream (ob->main_stream, pass->static_pass_number);

  if (tag == LTO_symtab_analyzed_node)
    {
      if (node->global.inlined_to)
	{
	  ref = lto_symtab_encoder_lookup (encoder, (symtab_node)node->global.inlined_to);
	  gcc_assert (ref != LCC_NOT_FOUND);
	}
      else
	ref = LCC_NOT_FOUND;

      streamer_write_hwi_stream (ob->main_stream, ref);
    }

  if (node->symbol.same_comdat_group && !boundary_p)
    {
      ref = lto_symtab_encoder_lookup (encoder,
				       node->symbol.same_comdat_group);
      gcc_assert (ref != LCC_NOT_FOUND);
    }
  else
    ref = LCC_NOT_FOUND;
  streamer_write_hwi_stream (ob->main_stream, ref);

  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, node->local.local, 1);
  bp_pack_value (&bp, node->symbol.externally_visible, 1);
  bp_pack_value (&bp, node->symbol.definition, 1);
  bp_pack_value (&bp, node->local.versionable, 1);
  bp_pack_value (&bp, node->local.can_change_signature, 1);
  bp_pack_value (&bp, node->local.redefined_extern_inline, 1);
  bp_pack_value (&bp, node->symbol.force_output, 1);
  bp_pack_value (&bp, node->symbol.forced_by_abi, 1);
  bp_pack_value (&bp, node->symbol.unique_name, 1);
  bp_pack_value (&bp, node->symbol.address_taken, 1);
  bp_pack_value (&bp, tag == LTO_symtab_analyzed_node
		 && !DECL_EXTERNAL (node->symbol.decl)
		 && !DECL_COMDAT (node->symbol.decl)
		 && (reachable_from_other_partition_p (node, encoder)
		     || referenced_from_other_partition_p (&node->symbol.ref_list,
							   encoder)), 1);
  bp_pack_value (&bp, node->lowered, 1);
  bp_pack_value (&bp, in_other_partition, 1);
  /* Real aliases in a boundary become non-aliases. However we still stream
     alias info on weakrefs. 
     TODO: We lose a bit of information here - when we know that variable is
     defined in other unit, we may use the info on aliases to resolve 
     symbol1 != symbol2 type tests that we can do only for locally defined objects
     otherwise.  */
  alias_p = node->symbol.alias && (!boundary_p || node->symbol.weakref);
  bp_pack_value (&bp, alias_p, 1);
  bp_pack_value (&bp, node->symbol.weakref, 1);
  bp_pack_value (&bp, node->frequency, 2);
  bp_pack_value (&bp, node->only_called_at_startup, 1);
  bp_pack_value (&bp, node->only_called_at_exit, 1);
  bp_pack_value (&bp, node->tm_clone, 1);
  bp_pack_value (&bp, node->thunk.thunk_p && !boundary_p, 1);
  bp_pack_enum (&bp, ld_plugin_symbol_resolution,
	        LDPR_NUM_KNOWN, node->symbol.resolution);
  streamer_write_bitpack (&bp);

  if (node->thunk.thunk_p && !boundary_p)
    {
      streamer_write_uhwi_stream
	 (ob->main_stream,
	  1 + (node->thunk.this_adjusting != 0) * 2
	  + (node->thunk.virtual_offset_p != 0) * 4);
      streamer_write_uhwi_stream (ob->main_stream, node->thunk.fixed_offset);
      streamer_write_uhwi_stream (ob->main_stream, node->thunk.virtual_value);
    }
  streamer_write_hwi_stream (ob->main_stream, node->profile_id);
}

/* Output the varpool NODE to OB. 
   If NODE is not in SET, then NODE is a boundary.  */

static void
lto_output_varpool_node (struct lto_simple_output_block *ob, struct varpool_node *node,
			 lto_symtab_encoder_t encoder)
{
  bool boundary_p = !lto_symtab_encoder_in_partition_p (encoder, (symtab_node)node);
  struct bitpack_d bp;
  int ref;
  bool alias_p;

  streamer_write_enum (ob->main_stream, LTO_symtab_tags, LTO_symtab_last_tag,
		       LTO_symtab_variable);
  streamer_write_hwi_stream (ob->main_stream, node->symbol.order);
  lto_output_var_decl_index (ob->decl_state, ob->main_stream, node->symbol.decl);
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, node->symbol.externally_visible, 1);
  bp_pack_value (&bp, node->symbol.force_output, 1);
  bp_pack_value (&bp, node->symbol.forced_by_abi, 1);
  bp_pack_value (&bp, node->symbol.unique_name, 1);
  bp_pack_value (&bp, node->symbol.definition, 1);
  alias_p = node->symbol.alias && (!boundary_p || node->symbol.weakref);
  bp_pack_value (&bp, alias_p, 1);
  bp_pack_value (&bp, node->symbol.weakref, 1);
  bp_pack_value (&bp, node->symbol.analyzed && !boundary_p, 1);
  gcc_assert (node->symbol.definition || !node->symbol.analyzed);
  /* Constant pool initializers can be de-unified into individual ltrans units.
     FIXME: Alternatively at -Os we may want to avoid generating for them the local
     labels and share them across LTRANS partitions.  */
  if (DECL_IN_CONSTANT_POOL (node->symbol.decl)
      && !DECL_EXTERNAL (node->symbol.decl)
      && !DECL_COMDAT (node->symbol.decl))
    {
      bp_pack_value (&bp, 0, 1);  /* used_from_other_parition.  */
      bp_pack_value (&bp, 0, 1);  /* in_other_partition.  */
    }
  else
    {
      bp_pack_value (&bp, node->symbol.definition
		     && referenced_from_other_partition_p (&node->symbol.ref_list,
							   encoder), 1);
      bp_pack_value (&bp, node->symbol.analyzed
		     && boundary_p && !DECL_EXTERNAL (node->symbol.decl), 1);
	  /* in_other_partition.  */
    }
  streamer_write_bitpack (&bp);
  if (node->symbol.same_comdat_group && !boundary_p)
    {
      ref = lto_symtab_encoder_lookup (encoder,
				       node->symbol.same_comdat_group);
      gcc_assert (ref != LCC_NOT_FOUND);
    }
  else
    ref = LCC_NOT_FOUND;
  streamer_write_hwi_stream (ob->main_stream, ref);
  streamer_write_enum (ob->main_stream, ld_plugin_symbol_resolution,
		       LDPR_NUM_KNOWN, node->symbol.resolution);
}

/* Output the varpool NODE to OB. 
   If NODE is not in SET, then NODE is a boundary.  */

static void
lto_output_ref (struct lto_simple_output_block *ob, struct ipa_ref *ref,
		lto_symtab_encoder_t encoder)
{
  struct bitpack_d bp;
  int nref;
  int uid = ref->lto_stmt_uid;
  struct cgraph_node *node;

  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, ref->use, 2);
  bp_pack_value (&bp, ref->speculative, 1);
  streamer_write_bitpack (&bp);
  nref = lto_symtab_encoder_lookup (encoder, ref->referred);
  gcc_assert (nref != LCC_NOT_FOUND);
  streamer_write_hwi_stream (ob->main_stream, nref);
  
  node = dyn_cast <cgraph_node> (ref->referring);
  if (node)
    {
      if (ref->stmt)
	uid = gimple_uid (ref->stmt) + 1;
      streamer_write_hwi_stream (ob->main_stream, uid);
    }
}

/* Stream out profile_summary to OB.  */

static void
output_profile_summary (struct lto_simple_output_block *ob)
{
  unsigned h_ix;
  struct bitpack_d bp;

  if (profile_info)
    {
      /* We do not output num and run_max, they are not used by
         GCC profile feedback and they are difficult to merge from multiple
         units.  */
      gcc_assert (profile_info->runs);
      streamer_write_uhwi_stream (ob->main_stream, profile_info->runs);
      streamer_write_gcov_count_stream (ob->main_stream, profile_info->sum_max);

      /* sum_all is needed for computing the working set with the
         histogram.  */
      streamer_write_gcov_count_stream (ob->main_stream, profile_info->sum_all);

      /* Create and output a bitpack of non-zero histogram entries indices.  */
      bp = bitpack_create (ob->main_stream);
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        bp_pack_value (&bp, profile_info->histogram[h_ix].num_counters > 0, 1);
      streamer_write_bitpack (&bp);
      /* Now stream out only those non-zero entries.  */
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        {
          if (!profile_info->histogram[h_ix].num_counters)
            continue;
          streamer_write_gcov_count_stream (ob->main_stream,
                                      profile_info->histogram[h_ix].num_counters);
          streamer_write_gcov_count_stream (ob->main_stream,
                                      profile_info->histogram[h_ix].min_value);
          streamer_write_gcov_count_stream (ob->main_stream,
                                      profile_info->histogram[h_ix].cum_value);
         }
      /* IPA-profile computes hot bb threshold based on cumulated
	 whole program profile.  We need to stream it down to ltrans.  */
       if (flag_wpa)
         streamer_write_gcov_count_stream (ob->main_stream,
					   get_hot_bb_threshold ());
    }
  else
    streamer_write_uhwi_stream (ob->main_stream, 0);
}

/* Output all callees or indirect outgoing edges.  EDGE must be the first such
   edge.  */

static void
output_outgoing_cgraph_edges (struct cgraph_edge *edge,
			      struct lto_simple_output_block *ob,
			      lto_symtab_encoder_t encoder)
{
  if (!edge)
    return;

  /* Output edges in backward direction, so the reconstructed callgraph match
     and it is easy to associate call sites in the IPA pass summaries.  */
  while (edge->next_callee)
    edge = edge->next_callee;
  for (; edge; edge = edge->prev_callee)
    lto_output_edge (ob, edge, encoder);
}

/* Output the part of the cgraph in SET.  */

static void
output_refs (lto_symtab_encoder_t encoder)
{
  lto_symtab_encoder_iterator lsei;
  struct lto_simple_output_block *ob;
  int count;
  struct ipa_ref *ref;
  int i;

  ob = lto_create_simple_output_block (LTO_section_refs);

  for (lsei = lsei_start_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_in_partition (&lsei))
    {
      symtab_node node = lsei_node (lsei);

      count = ipa_ref_list_nreferences (&node->symbol.ref_list);
      if (count)
	{
	  streamer_write_gcov_count_stream (ob->main_stream, count);
	  streamer_write_uhwi_stream (ob->main_stream,
				     lto_symtab_encoder_lookup (encoder, node));
	  for (i = 0; ipa_ref_list_reference_iterate (&node->symbol.ref_list,
						      i, ref); i++)
	    lto_output_ref (ob, ref, encoder);
	}
    }

  streamer_write_uhwi_stream (ob->main_stream, 0);

  lto_destroy_simple_output_block (ob);
}

/* Add NODE into encoder as well as nodes it is cloned from.
   Do it in a way so clones appear first.  */

static void
add_node_to (lto_symtab_encoder_t encoder, struct cgraph_node *node,
	     bool include_body)
{
  if (node->clone_of)
    add_node_to (encoder, node->clone_of, include_body);
  else if (include_body)
    lto_set_symtab_encoder_encode_body (encoder, node);
  lto_symtab_encoder_encode (encoder, (symtab_node)node);
}

/* Add all references in LIST to encoders.  */

static void
add_references (lto_symtab_encoder_t encoder,
		struct ipa_ref_list *list)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
    if (is_a <cgraph_node> (ref->referred))
      add_node_to (encoder, ipa_ref_node (ref), false);
    else
      lto_symtab_encoder_encode (encoder, ref->referred);
}

/* Find all symbols we want to stream into given partition and insert them
   to encoders.

   The function actually replaces IN_ENCODER by new one.  The reason is that
   streaming code needs clone's origin to be streamed before clone.  This
   means that we need to insert the nodes in specific order.  This order is
   ignored by the partitioning logic earlier.  */

lto_symtab_encoder_t 
compute_ltrans_boundary (lto_symtab_encoder_t in_encoder)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;
  int i;
  lto_symtab_encoder_t encoder;
  lto_symtab_encoder_iterator lsei;
  struct pointer_set_t *reachable_call_targets = pointer_set_create ();

  encoder = lto_symtab_encoder_new (false);

  /* Go over all entries in the IN_ENCODER and duplicate them to
     ENCODER. At the same time insert masters of clones so
     every master appears before clone.  */
  for (lsei = lsei_start_function_in_partition (in_encoder);
       !lsei_end_p (lsei); lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      add_node_to (encoder, node, true);
      lto_set_symtab_encoder_in_partition (encoder, (symtab_node)node);
      add_references (encoder, &node->symbol.ref_list);
      /* For proper debug info, we need to ship the origins, too.  */
      if (DECL_ABSTRACT_ORIGIN (node->symbol.decl))
	{
	  struct cgraph_node *origin_node
	  = cgraph_get_node (DECL_ABSTRACT_ORIGIN (node->symbol.decl));
	  add_node_to (encoder, origin_node, true);
	}
    }
  for (lsei = lsei_start_variable_in_partition (in_encoder);
       !lsei_end_p (lsei); lsei_next_variable_in_partition (&lsei))
    {
      struct varpool_node *vnode = lsei_varpool_node (lsei);

      lto_set_symtab_encoder_in_partition (encoder, (symtab_node)vnode);
      lto_set_symtab_encoder_encode_initializer (encoder, vnode);
      add_references (encoder, &vnode->symbol.ref_list);
      /* For proper debug info, we need to ship the origins, too.  */
      if (DECL_ABSTRACT_ORIGIN (vnode->symbol.decl))
	{
	  struct varpool_node *origin_node
	  = varpool_get_node (DECL_ABSTRACT_ORIGIN (node->symbol.decl));
	  lto_set_symtab_encoder_in_partition (encoder, (symtab_node)origin_node);
	}
    }
  /* Pickle in also the initializer of all referenced readonly variables
     to help folding.  Constant pool variables are not shared, so we must
     pickle those too.  */
  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node node = lto_symtab_encoder_deref (encoder, i);
      if (varpool_node *vnode = dyn_cast <varpool_node> (node))
	{
	  if (!lto_symtab_encoder_encode_initializer_p (encoder,
							vnode)
	      && ctor_for_folding (vnode->symbol.decl) != error_mark_node)
	    {
	      lto_set_symtab_encoder_encode_initializer (encoder, vnode);
	      add_references (encoder, &vnode->symbol.ref_list);
	    }
       }
    }

  /* Go over all the nodes again to include callees that are not in
     SET.  */
  for (lsei = lsei_start_function_in_partition (encoder);
       !lsei_end_p (lsei); lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  struct cgraph_node *callee = edge->callee;
	  if (!lto_symtab_encoder_in_partition_p (encoder, (symtab_node)callee))
	    {
	      /* We should have moved all the inlines.  */
	      gcc_assert (!callee->global.inlined_to);
	      add_node_to (encoder, callee, false);
	    }
	}
      /* Add all possible targets for late devirtualization.  */
      if (flag_devirtualize)
	for (edge = node->indirect_calls; edge; edge = edge->next_callee)
	  if (edge->indirect_info->polymorphic)
	    {
	      unsigned int i;
	      void *cache_token;
	      bool final;
	      vec <cgraph_node *>targets
		= possible_polymorphic_call_targets
		    (edge, &final, &cache_token);
	      if (!pointer_set_insert (reachable_call_targets,
				       cache_token))
		{
		  for (i = 0; i < targets.length (); i++)
		    {
		      struct cgraph_node *callee = targets[i];

		      /* Adding an external declarations into the unit serves
			 no purpose and just increases its boundary.  */
		      if (callee->symbol.definition
			  && !lto_symtab_encoder_in_partition_p
			       (encoder, (symtab_node)callee))
			{
			  gcc_assert (!callee->global.inlined_to);
			  add_node_to (encoder, callee, false);
			}
		    }
		}
	    }
    }
  lto_symtab_encoder_delete (in_encoder);
  pointer_set_destroy (reachable_call_targets);
  return encoder;
}

/* Output the part of the symtab in SET and VSET.  */

void
output_symtab (void)
{
  struct cgraph_node *node;
  struct lto_simple_output_block *ob;
  lto_symtab_encoder_iterator lsei;
  int i, n_nodes;
  lto_symtab_encoder_t encoder;
  static bool asm_nodes_output = false;

  if (flag_wpa)
    output_cgraph_opt_summary ();

  ob = lto_create_simple_output_block (LTO_section_symtab_nodes);

  output_profile_summary (ob);

  /* An encoder for cgraph nodes should have been created by
     ipa_write_summaries_1.  */
  gcc_assert (ob->decl_state->symtab_node_encoder);
  encoder = ob->decl_state->symtab_node_encoder;

  /* Write out the nodes.  We must first output a node and then its clones,
     otherwise at a time reading back the node there would be nothing to clone
     from.  */
  n_nodes = lto_symtab_encoder_size (encoder);
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node node = lto_symtab_encoder_deref (encoder, i);
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
        lto_output_node (ob, cnode, encoder);
      else
        lto_output_varpool_node (ob, varpool (node), encoder);
	
    }

  /* Go over the nodes in SET again to write edges.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      output_outgoing_cgraph_edges (node->callees, ob, encoder);
      output_outgoing_cgraph_edges (node->indirect_calls, ob, encoder);
    }

  streamer_write_uhwi_stream (ob->main_stream, 0);

  lto_destroy_simple_output_block (ob);

  /* Emit toplevel asms.
     When doing WPA we must output every asm just once.  Since we do not partition asm
     nodes at all, output them to first output.  This is kind of hack, but should work
     well.  */
  if (!asm_nodes_output)
    {
      asm_nodes_output = true;
      lto_output_toplevel_asms ();
    }

  output_refs (encoder);
}

/* Overwrite the information in NODE based on FILE_DATA, TAG, FLAGS,
   STACK_SIZE, SELF_TIME and SELF_SIZE.  This is called either to initialize
   NODE or to replace the values in it, for instance because the first
   time we saw it, the function body was not available but now it
   is.  BP is a bitpack with all the bitflags for NODE read from the
   stream.  */

static void
input_overwrite_node (struct lto_file_decl_data *file_data,
		      struct cgraph_node *node,
		      enum LTO_symtab_tags tag,
		      struct bitpack_d *bp)
{
  node->symbol.aux = (void *) tag;
  node->symbol.lto_file_data = file_data;

  node->local.local = bp_unpack_value (bp, 1);
  node->symbol.externally_visible = bp_unpack_value (bp, 1);
  node->symbol.definition = bp_unpack_value (bp, 1);
  node->local.versionable = bp_unpack_value (bp, 1);
  node->local.can_change_signature = bp_unpack_value (bp, 1);
  node->local.redefined_extern_inline = bp_unpack_value (bp, 1);
  node->symbol.force_output = bp_unpack_value (bp, 1);
  node->symbol.forced_by_abi = bp_unpack_value (bp, 1);
  node->symbol.unique_name = bp_unpack_value (bp, 1);
  node->symbol.address_taken = bp_unpack_value (bp, 1);
  node->symbol.used_from_other_partition = bp_unpack_value (bp, 1);
  node->lowered = bp_unpack_value (bp, 1);
  node->symbol.analyzed = tag == LTO_symtab_analyzed_node;
  node->symbol.in_other_partition = bp_unpack_value (bp, 1);
  if (node->symbol.in_other_partition
      /* Avoid updating decl when we are seeing just inline clone.
	 When inlining function that has functions already inlined into it,
	 we produce clones of inline clones.

	 WPA partitioning might put each clone into different unit and
	 we might end up streaming inline clone from other partition
	 to support clone we are interested in. */
      && (!node->clone_of
	  || node->clone_of->symbol.decl != node->symbol.decl))
    {
      DECL_EXTERNAL (node->symbol.decl) = 1;
      TREE_STATIC (node->symbol.decl) = 0;
    }
  node->symbol.alias = bp_unpack_value (bp, 1);
  node->symbol.weakref = bp_unpack_value (bp, 1);
  node->frequency = (enum node_frequency)bp_unpack_value (bp, 2);
  node->only_called_at_startup = bp_unpack_value (bp, 1);
  node->only_called_at_exit = bp_unpack_value (bp, 1);
  node->tm_clone = bp_unpack_value (bp, 1);
  node->thunk.thunk_p = bp_unpack_value (bp, 1);
  node->symbol.resolution = bp_unpack_enum (bp, ld_plugin_symbol_resolution,
				     LDPR_NUM_KNOWN);
}

/* Return string alias is alias of.  */

static tree
get_alias_symbol (tree decl)
{
  tree alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
  return get_identifier (TREE_STRING_POINTER
			  (TREE_VALUE (TREE_VALUE (alias))));
}

/* Read a node from input_block IB.  TAG is the node's tag just read.
   Return the node read or overwriten.  */

static struct cgraph_node *
input_node (struct lto_file_decl_data *file_data,
	    struct lto_input_block *ib,
	    enum LTO_symtab_tags tag,
	    vec<symtab_node> nodes)
{
  gcc::pass_manager *passes = g->get_passes ();
  tree fn_decl;
  struct cgraph_node *node;
  struct bitpack_d bp;
  unsigned decl_index;
  int ref = LCC_NOT_FOUND, ref2 = LCC_NOT_FOUND;
  int clone_ref;
  int order;
  int i, count;

  order = streamer_read_hwi (ib) + order_base;
  clone_ref = streamer_read_hwi (ib);

  decl_index = streamer_read_uhwi (ib);
  fn_decl = lto_file_decl_data_get_fn_decl (file_data, decl_index);

  if (clone_ref != LCC_NOT_FOUND)
    {
      node = cgraph_clone_node (cgraph (nodes[clone_ref]), fn_decl,
				0, CGRAPH_FREQ_BASE, false,
				vNULL, false, NULL);
    }
  else
    {
      /* Declaration of functions can be already merged with a declaration
	 from other input file.  We keep cgraph unmerged until after streaming
	 of ipa passes is done.  Alays forcingly create a fresh node.  */
      node = cgraph_create_empty_node ();
      node->symbol.decl = fn_decl;
      symtab_register_node ((symtab_node)node);
    }

  node->symbol.order = order;
  if (order >= symtab_order)
    symtab_order = order + 1;

  node->count = streamer_read_gcov_count (ib);
  node->count_materialization_scale = streamer_read_hwi (ib);

  count = streamer_read_hwi (ib);
  node->ipa_transforms_to_apply = vNULL;
  for (i = 0; i < count; i++)
    {
      struct opt_pass *pass;
      int pid = streamer_read_hwi (ib);

      gcc_assert (pid < passes->passes_by_id_size);
      pass = passes->passes_by_id[pid];
      node->ipa_transforms_to_apply.safe_push ((struct ipa_opt_pass_d *) pass);
    }

  if (tag == LTO_symtab_analyzed_node)
    ref = streamer_read_hwi (ib);

  ref2 = streamer_read_hwi (ib);

  /* Make sure that we have not read this node before.  Nodes that
     have already been read will have their tag stored in the 'aux'
     field.  Since built-in functions can be referenced in multiple
     functions, they are expected to be read more than once.  */
  if (node->symbol.aux && !DECL_BUILT_IN (node->symbol.decl))
    internal_error ("bytecode stream: found multiple instances of cgraph "
		    "node with uid %d", node->uid);

  bp = streamer_read_bitpack (ib);
  input_overwrite_node (file_data, node, tag, &bp);

  /* Store a reference for now, and fix up later to be a pointer.  */
  node->global.inlined_to = (cgraph_node_ptr) (intptr_t) ref;

  /* Store a reference for now, and fix up later to be a pointer.  */
  node->symbol.same_comdat_group = (symtab_node) (intptr_t) ref2;

  if (node->thunk.thunk_p)
    {
      int type = streamer_read_uhwi (ib);
      HOST_WIDE_INT fixed_offset = streamer_read_uhwi (ib);
      HOST_WIDE_INT virtual_value = streamer_read_uhwi (ib);

      node->thunk.fixed_offset = fixed_offset;
      node->thunk.this_adjusting = (type & 2);
      node->thunk.virtual_value = virtual_value;
      node->thunk.virtual_offset_p = (type & 4);
    }
  if (node->symbol.alias && !node->symbol.analyzed && node->symbol.weakref)
    node->symbol.alias_target = get_alias_symbol (node->symbol.decl);
  node->profile_id = streamer_read_hwi (ib);
  return node;
}

/* Read a node from input_block IB.  TAG is the node's tag just read.
   Return the node read or overwriten.  */

static struct varpool_node *
input_varpool_node (struct lto_file_decl_data *file_data,
		    struct lto_input_block *ib)
{
  int decl_index;
  tree var_decl;
  struct varpool_node *node;
  struct bitpack_d bp;
  int ref = LCC_NOT_FOUND;
  int order;

  order = streamer_read_hwi (ib) + order_base;
  decl_index = streamer_read_uhwi (ib);
  var_decl = lto_file_decl_data_get_var_decl (file_data, decl_index);

  /* Declaration of functions can be already merged with a declaration
     from other input file.  We keep cgraph unmerged until after streaming
     of ipa passes is done.  Alays forcingly create a fresh node.  */
  node = varpool_create_empty_node ();
  node->symbol.decl = var_decl;
  symtab_register_node ((symtab_node)node);

  node->symbol.order = order;
  if (order >= symtab_order)
    symtab_order = order + 1;
  node->symbol.lto_file_data = file_data;

  bp = streamer_read_bitpack (ib);
  node->symbol.externally_visible = bp_unpack_value (&bp, 1);
  node->symbol.force_output = bp_unpack_value (&bp, 1);
  node->symbol.forced_by_abi = bp_unpack_value (&bp, 1);
  node->symbol.unique_name = bp_unpack_value (&bp, 1);
  node->symbol.definition = bp_unpack_value (&bp, 1);
  node->symbol.alias = bp_unpack_value (&bp, 1);
  node->symbol.weakref = bp_unpack_value (&bp, 1);
  node->symbol.analyzed = bp_unpack_value (&bp, 1);
  node->symbol.used_from_other_partition = bp_unpack_value (&bp, 1);
  node->symbol.in_other_partition = bp_unpack_value (&bp, 1);
  if (node->symbol.in_other_partition)
    {
      DECL_EXTERNAL (node->symbol.decl) = 1;
      TREE_STATIC (node->symbol.decl) = 0;
    }
  if (node->symbol.alias && !node->symbol.analyzed && node->symbol.weakref)
    node->symbol.alias_target = get_alias_symbol (node->symbol.decl);
  ref = streamer_read_hwi (ib);
  /* Store a reference for now, and fix up later to be a pointer.  */
  node->symbol.same_comdat_group = (symtab_node) (intptr_t) ref;
  node->symbol.resolution = streamer_read_enum (ib, ld_plugin_symbol_resolution,
					        LDPR_NUM_KNOWN);

  return node;
}

/* Read a node from input_block IB.  TAG is the node's tag just read.
   Return the node read or overwriten.  */

static void
input_ref (struct lto_input_block *ib,
	   symtab_node referring_node,
	   vec<symtab_node> nodes)
{
  symtab_node node = NULL;
  struct bitpack_d bp;
  enum ipa_ref_use use;
  bool speculative;
  struct ipa_ref *ref;

  bp = streamer_read_bitpack (ib);
  use = (enum ipa_ref_use) bp_unpack_value (&bp, 2);
  speculative = (enum ipa_ref_use) bp_unpack_value (&bp, 1);
  node = nodes[streamer_read_hwi (ib)];
  ref = ipa_record_reference (referring_node, node, use, NULL);
  ref->speculative = speculative;
  if (is_a <cgraph_node> (referring_node))
    ref->lto_stmt_uid = streamer_read_hwi (ib);
}

/* Read an edge from IB.  NODES points to a vector of previously read nodes for
   decoding caller and callee of the edge to be read.  If INDIRECT is true, the
   edge being read is indirect (in the sense that it has
   indirect_unknown_callee set).  */

static void
input_edge (struct lto_input_block *ib, vec<symtab_node> nodes,
	    bool indirect)
{
  struct cgraph_node *caller, *callee;
  struct cgraph_edge *edge;
  unsigned int stmt_id;
  gcov_type count;
  int freq;
  cgraph_inline_failed_t inline_failed;
  struct bitpack_d bp;
  int ecf_flags = 0;

  caller = cgraph (nodes[streamer_read_hwi (ib)]);
  if (caller == NULL || caller->symbol.decl == NULL_TREE)
    internal_error ("bytecode stream: no caller found while reading edge");

  if (!indirect)
    {
      callee = cgraph (nodes[streamer_read_hwi (ib)]);
      if (callee == NULL || callee->symbol.decl == NULL_TREE)
	internal_error ("bytecode stream: no callee found while reading edge");
    }
  else
    callee = NULL;

  count = streamer_read_gcov_count (ib);

  bp = streamer_read_bitpack (ib);
  inline_failed = bp_unpack_enum (&bp, cgraph_inline_failed_enum, CIF_N_REASONS);
  stmt_id = bp_unpack_var_len_unsigned (&bp);
  freq = (int) bp_unpack_var_len_unsigned (&bp);

  if (indirect)
    edge = cgraph_create_indirect_edge (caller, NULL, 0, count, freq);
  else
    edge = cgraph_create_edge (caller, callee, NULL, count, freq);

  edge->indirect_inlining_edge = bp_unpack_value (&bp, 1);
  edge->speculative = bp_unpack_value (&bp, 1);
  edge->lto_stmt_uid = stmt_id;
  edge->inline_failed = inline_failed;
  edge->call_stmt_cannot_inline_p = bp_unpack_value (&bp, 1);
  edge->can_throw_external = bp_unpack_value (&bp, 1);
  if (indirect)
    {
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_CONST;
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_PURE;
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_NORETURN;
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_MALLOC;
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_NOTHROW;
      if (bp_unpack_value (&bp, 1))
	ecf_flags |= ECF_RETURNS_TWICE;
      edge->indirect_info->ecf_flags = ecf_flags;
      edge->indirect_info->common_target_id = streamer_read_hwi (ib);
      if (edge->indirect_info->common_target_id)
        edge->indirect_info->common_target_probability = streamer_read_hwi (ib);
    }
}


/* Read a cgraph from IB using the info in FILE_DATA.  */

static vec<symtab_node> 
input_cgraph_1 (struct lto_file_decl_data *file_data,
		struct lto_input_block *ib)
{
  enum LTO_symtab_tags tag;
  vec<symtab_node> nodes = vNULL;
  symtab_node node;
  unsigned i;

  tag = streamer_read_enum (ib, LTO_symtab_tags, LTO_symtab_last_tag);
  order_base = symtab_order;
  while (tag)
    {
      if (tag == LTO_symtab_edge)
        input_edge (ib, nodes, false);
      else if (tag == LTO_symtab_indirect_edge)
        input_edge (ib, nodes, true);
      else if (tag == LTO_symtab_variable)
        {
	  node = (symtab_node)input_varpool_node (file_data, ib);
          nodes.safe_push (node);
	  lto_symtab_encoder_encode (file_data->symtab_node_encoder, node);
        }
      else
	{
	  node = (symtab_node)input_node (file_data, ib, tag, nodes);
	  if (node == NULL || node->symbol.decl == NULL_TREE)
	    internal_error ("bytecode stream: found empty cgraph node");
	  nodes.safe_push (node);
	  lto_symtab_encoder_encode (file_data->symtab_node_encoder, node);
	}

      tag = streamer_read_enum (ib, LTO_symtab_tags, LTO_symtab_last_tag);
    }

  lto_input_toplevel_asms (file_data, order_base);

  /* AUX pointers should be all non-zero for function nodes read from the stream.  */
#ifdef ENABLE_CHECKING
  FOR_EACH_VEC_ELT (nodes, i, node)
    gcc_assert (node->symbol.aux || !is_a <cgraph_node> (node));
#endif
  FOR_EACH_VEC_ELT (nodes, i, node)
    {
      int ref;
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	{
	  ref = (int) (intptr_t) cnode->global.inlined_to;

	  /* We share declaration of builtins, so we may read same node twice.  */
	  if (!node->symbol.aux)
	    continue;
	  node->symbol.aux = NULL;

	  /* Fixup inlined_to from reference to pointer.  */
	  if (ref != LCC_NOT_FOUND)
	    cgraph (node)->global.inlined_to = cgraph (nodes[ref]);
	  else
	    cnode->global.inlined_to = NULL;
	}

      ref = (int) (intptr_t) node->symbol.same_comdat_group;

      /* Fixup same_comdat_group from reference to pointer.  */
      if (ref != LCC_NOT_FOUND)
	node->symbol.same_comdat_group = nodes[ref];
      else
	node->symbol.same_comdat_group = NULL;
    }
  FOR_EACH_VEC_ELT (nodes, i, node)
    node->symbol.aux = is_a <cgraph_node> (node) ? (void *)1 : NULL;
  return nodes;
}

/* Input ipa_refs.  */

static void
input_refs (struct lto_input_block *ib,
	    vec<symtab_node> nodes)
{
  int count;
  int idx;
  while (true)
    {
      symtab_node node;
      count = streamer_read_uhwi (ib);
      if (!count)
	break;
      idx = streamer_read_uhwi (ib);
      node = nodes[idx];
      while (count)
	{
	  input_ref (ib, node, nodes);
	  count--;
	}
    }
}
	    

static struct gcov_ctr_summary lto_gcov_summary;

/* Input profile_info from IB.  */
static void
input_profile_summary (struct lto_input_block *ib,
		       struct lto_file_decl_data *file_data)
{
  unsigned h_ix;
  struct bitpack_d bp;
  unsigned int runs = streamer_read_uhwi (ib);
  if (runs)
    {
      file_data->profile_info.runs = runs;
      file_data->profile_info.sum_max = streamer_read_gcov_count (ib);
      file_data->profile_info.sum_all = streamer_read_gcov_count (ib);

      memset (file_data->profile_info.histogram, 0,
              sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);
      /* Input the bitpack of non-zero histogram indices.  */
      bp = streamer_read_bitpack (ib);
      /* Read in and unpack the full bitpack, flagging non-zero
         histogram entries by setting the num_counters non-zero.  */
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        {
          file_data->profile_info.histogram[h_ix].num_counters
              = bp_unpack_value (&bp, 1);
        }
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        {
          if (!file_data->profile_info.histogram[h_ix].num_counters)
            continue;

          file_data->profile_info.histogram[h_ix].num_counters
              = streamer_read_gcov_count (ib);
          file_data->profile_info.histogram[h_ix].min_value
              = streamer_read_gcov_count (ib);
          file_data->profile_info.histogram[h_ix].cum_value
              = streamer_read_gcov_count (ib);
        }
      /* IPA-profile computes hot bb threshold based on cumulated
	 whole program profile.  We need to stream it down to ltrans.  */
      if (flag_ltrans)
	set_hot_bb_threshold (streamer_read_gcov_count (ib));
    }

}

/* Rescale profile summaries to the same number of runs in the whole unit.  */

static void
merge_profile_summaries (struct lto_file_decl_data **file_data_vec)
{
  struct lto_file_decl_data *file_data;
  unsigned int j, h_ix;
  gcov_unsigned_t max_runs = 0;
  struct cgraph_node *node;
  struct cgraph_edge *edge;
  gcov_type saved_sum_all = 0;
  gcov_ctr_summary *saved_profile_info = 0;
  int saved_scale = 0;

  /* Find unit with maximal number of runs.  If we ever get serious about
     roundoff errors, we might also consider computing smallest common
     multiply.  */
  for (j = 0; (file_data = file_data_vec[j]) != NULL; j++)
    if (max_runs < file_data->profile_info.runs)
      max_runs = file_data->profile_info.runs;

  if (!max_runs)
    return;

  /* Simple overflow check.  We probably don't need to support that many train
     runs. Such a large value probably imply data corruption anyway.  */
  if (max_runs > INT_MAX / REG_BR_PROB_BASE)
    {
      sorry ("At most %i profile runs is supported. Perhaps corrupted profile?",
	     INT_MAX / REG_BR_PROB_BASE);
      return;
    }

  profile_info = &lto_gcov_summary;
  lto_gcov_summary.runs = max_runs;
  lto_gcov_summary.sum_max = 0;
  memset (lto_gcov_summary.histogram, 0,
          sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);

  /* Rescale all units to the maximal number of runs.
     sum_max can not be easily merged, as we have no idea what files come from
     the same run.  We do not use the info anyway, so leave it 0.  */
  for (j = 0; (file_data = file_data_vec[j]) != NULL; j++)
    if (file_data->profile_info.runs)
      {
	int scale = GCOV_COMPUTE_SCALE (max_runs,
                                        file_data->profile_info.runs);
	lto_gcov_summary.sum_max
            = MAX (lto_gcov_summary.sum_max,
                   apply_scale (file_data->profile_info.sum_max, scale));
	lto_gcov_summary.sum_all
            = MAX (lto_gcov_summary.sum_all,
                   apply_scale (file_data->profile_info.sum_all, scale));
        /* Save a pointer to the profile_info with the largest
           scaled sum_all and the scale for use in merging the
           histogram.  */
        if (!saved_profile_info
            || lto_gcov_summary.sum_all > saved_sum_all)
          {
            saved_profile_info = &file_data->profile_info;
            saved_sum_all = lto_gcov_summary.sum_all;
            saved_scale = scale;
          }
      }

  gcc_assert (saved_profile_info);

  /* Scale up the histogram from the profile that had the largest
     scaled sum_all above.  */
  for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
    {
      /* Scale up the min value as we did the corresponding sum_all
         above. Use that to find the new histogram index.  */
      gcov_type scaled_min
          = apply_scale (saved_profile_info->histogram[h_ix].min_value,
                         saved_scale);
      /* The new index may be shared with another scaled histogram entry,
         so we need to account for a non-zero histogram entry at new_ix.  */
      unsigned new_ix = gcov_histo_index (scaled_min);
      lto_gcov_summary.histogram[new_ix].min_value
          = (lto_gcov_summary.histogram[new_ix].num_counters
             ? MIN (lto_gcov_summary.histogram[new_ix].min_value, scaled_min)
             : scaled_min);
      /* Some of the scaled counter values would ostensibly need to be placed
         into different (larger) histogram buckets, but we keep things simple
         here and place the scaled cumulative counter value in the bucket
         corresponding to the scaled minimum counter value.  */
      lto_gcov_summary.histogram[new_ix].cum_value
          += apply_scale (saved_profile_info->histogram[h_ix].cum_value,
                          saved_scale);
      lto_gcov_summary.histogram[new_ix].num_counters
          += saved_profile_info->histogram[h_ix].num_counters;
    }

  /* Watch roundoff errors.  */
  if (lto_gcov_summary.sum_max < max_runs)
    lto_gcov_summary.sum_max = max_runs;

  /* If merging already happent at WPA time, we are done.  */
  if (flag_ltrans)
    return;

  /* Now compute count_materialization_scale of each node.
     During LTRANS we already have values of count_materialization_scale
     computed, so just update them.  */
  FOR_EACH_FUNCTION (node)
    if (node->symbol.lto_file_data
	&& node->symbol.lto_file_data->profile_info.runs)
      {
	int scale;

	scale = RDIV (node->count_materialization_scale * max_runs,
                      node->symbol.lto_file_data->profile_info.runs);
	node->count_materialization_scale = scale;
	if (scale < 0)
	  fatal_error ("Profile information in %s corrupted",
		       file_data->file_name);

	if (scale == REG_BR_PROB_BASE)
	  continue;
	for (edge = node->callees; edge; edge = edge->next_callee)
	  edge->count = apply_scale (edge->count, scale);
	node->count = apply_scale (node->count, scale);
      }
}

/* Input and merge the symtab from each of the .o files passed to
   lto1.  */

void
input_symtab (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;
  struct cgraph_node *node;

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      struct lto_input_block *ib;
      vec<symtab_node> nodes;

      ib = lto_create_simple_input_block (file_data, LTO_section_symtab_nodes,
					  &data, &len);
      if (!ib) 
	fatal_error ("cannot find LTO cgraph in %s", file_data->file_name);
      input_profile_summary (ib, file_data);
      file_data->symtab_node_encoder = lto_symtab_encoder_new (true);
      nodes = input_cgraph_1 (file_data, ib);
      lto_destroy_simple_input_block (file_data, LTO_section_symtab_nodes,
				      ib, data, len);

      ib = lto_create_simple_input_block (file_data, LTO_section_refs,
					  &data, &len);
      if (!ib)
	fatal_error ("cannot find LTO section refs in %s",
		     file_data->file_name);
      input_refs (ib, nodes);
      lto_destroy_simple_input_block (file_data, LTO_section_refs,
				      ib, data, len);
      if (flag_ltrans)
	input_cgraph_opt_summary (nodes);
      nodes.release ();
    }

  merge_profile_summaries (file_data_vec);
  get_working_sets ();


  /* Clear out the aux field that was used to store enough state to
     tell which nodes should be overwritten.  */
  FOR_EACH_FUNCTION (node)
    {
      /* Some nodes may have been created by cgraph_node.  This
	 happens when the callgraph contains nested functions.  If the
	 node for the parent function was never emitted to the gimple
	 file, cgraph_node will create a node for it when setting the
	 context of the nested function.  */
      if (node->symbol.lto_file_data)
	node->symbol.aux = NULL;
    }
}

/* True when we need optimization summary for NODE.  */

static int
output_cgraph_opt_summary_p (struct cgraph_node *node)
{
  return (node->clone_of
	  && (node->clone.tree_map
	      || node->clone.args_to_skip
	      || node->clone.combined_args_to_skip));
}

/* Output optimization summary for EDGE to OB.  */
static void
output_edge_opt_summary (struct output_block *ob ATTRIBUTE_UNUSED,
			 struct cgraph_edge *edge ATTRIBUTE_UNUSED)
{
}

/* Output optimization summary for NODE to OB.  */

static void
output_node_opt_summary (struct output_block *ob,
			 struct cgraph_node *node,
			 lto_symtab_encoder_t encoder)
{
  unsigned int index;
  bitmap_iterator bi;
  struct ipa_replace_map *map;
  struct bitpack_d bp;
  int i;
  struct cgraph_edge *e;

  if (node->clone.args_to_skip)
    {
      streamer_write_uhwi (ob, bitmap_count_bits (node->clone.args_to_skip));
      EXECUTE_IF_SET_IN_BITMAP (node->clone.args_to_skip, 0, index, bi)
	streamer_write_uhwi (ob, index);
    }
  else
    streamer_write_uhwi (ob, 0);
  if (node->clone.combined_args_to_skip)
    {
      streamer_write_uhwi (ob, bitmap_count_bits (node->clone.combined_args_to_skip));
      EXECUTE_IF_SET_IN_BITMAP (node->clone.combined_args_to_skip, 0, index, bi)
	streamer_write_uhwi (ob, index);
    }
  else
    streamer_write_uhwi (ob, 0);
  streamer_write_uhwi (ob, vec_safe_length (node->clone.tree_map));
  FOR_EACH_VEC_SAFE_ELT (node->clone.tree_map, i, map)
    {
      /* At the moment we assume all old trees to be PARM_DECLs, because we have no
         mechanism to store function local declarations into summaries.  */
      gcc_assert (!map->old_tree);
      streamer_write_uhwi (ob, map->parm_num);
      gcc_assert (EXPR_LOCATION (map->new_tree) == UNKNOWN_LOCATION);
      stream_write_tree (ob, map->new_tree, true);
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, map->replace_p, 1);
      bp_pack_value (&bp, map->ref_p, 1);
      streamer_write_bitpack (&bp);
    }

  if (lto_symtab_encoder_in_partition_p (encoder, (symtab_node) node))
    {
      for (e = node->callees; e; e = e->next_callee)
	output_edge_opt_summary (ob, e);
      for (e = node->indirect_calls; e; e = e->next_callee)
	output_edge_opt_summary (ob, e);
    }
}

/* Output optimization summaries stored in callgraph.
   At the moment it is the clone info structure.  */

static void
output_cgraph_opt_summary (void)
{
  int i, n_nodes;
  lto_symtab_encoder_t encoder;
  struct output_block *ob = create_output_block (LTO_section_cgraph_opt_sum);
  unsigned count = 0;

  ob->cgraph_node = NULL;
  encoder = ob->decl_state->symtab_node_encoder;
  n_nodes = lto_symtab_encoder_size (encoder);
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node node = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node> (node);
      if (cnode && output_cgraph_opt_summary_p (cnode))
	count++;
    }
  streamer_write_uhwi (ob, count);
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node node = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node> (node);
      if (cnode && output_cgraph_opt_summary_p (cnode))
	{
	  streamer_write_uhwi (ob, i);
	  output_node_opt_summary (ob, cnode, encoder);
	}
    }
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

/* Input optimisation summary of EDGE.  */

static void
input_edge_opt_summary (struct cgraph_edge *edge ATTRIBUTE_UNUSED,
			struct lto_input_block *ib_main ATTRIBUTE_UNUSED)
{
}

/* Input optimisation summary of NODE.  */

static void
input_node_opt_summary (struct cgraph_node *node,
			struct lto_input_block *ib_main,
			struct data_in *data_in)
{
  int i;
  int count;
  int bit;
  struct bitpack_d bp;
  struct cgraph_edge *e;

  count = streamer_read_uhwi (ib_main);
  if (count)
    node->clone.args_to_skip = BITMAP_GGC_ALLOC ();
  for (i = 0; i < count; i++)
    {
      bit = streamer_read_uhwi (ib_main);
      bitmap_set_bit (node->clone.args_to_skip, bit);
    }
  count = streamer_read_uhwi (ib_main);
  if (count)
    node->clone.combined_args_to_skip = BITMAP_GGC_ALLOC ();
  for (i = 0; i < count; i++)
    {
      bit = streamer_read_uhwi (ib_main);
      bitmap_set_bit (node->clone.combined_args_to_skip, bit);
    }
  count = streamer_read_uhwi (ib_main);
  for (i = 0; i < count; i++)
    {
      struct ipa_replace_map *map = ggc_alloc_ipa_replace_map ();

      vec_safe_push (node->clone.tree_map, map);
      map->parm_num = streamer_read_uhwi (ib_main);
      map->old_tree = NULL;
      map->new_tree = stream_read_tree (ib_main, data_in);
      bp = streamer_read_bitpack (ib_main);
      map->replace_p = bp_unpack_value (&bp, 1);
      map->ref_p = bp_unpack_value (&bp, 1);
    }
  for (e = node->callees; e; e = e->next_callee)
    input_edge_opt_summary (e, ib_main);
  for (e = node->indirect_calls; e; e = e->next_callee)
    input_edge_opt_summary (e, ib_main);
}

/* Read section in file FILE_DATA of length LEN with data DATA.  */

static void
input_cgraph_opt_section (struct lto_file_decl_data *file_data,
			  const char *data, size_t len,
			  vec<symtab_node> nodes)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib_main;
  unsigned int i;
  unsigned int count;

  LTO_INIT_INPUT_BLOCK (ib_main, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      int ref = streamer_read_uhwi (&ib_main);
      input_node_opt_summary (cgraph (nodes[ref]),
			      &ib_main, data_in);
    }
  lto_free_section_data (file_data, LTO_section_cgraph_opt_sum, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Input optimization summary of cgraph.  */

static void
input_cgraph_opt_summary (vec<symtab_node> nodes)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data =
	lto_get_section_data (file_data, LTO_section_cgraph_opt_sum, NULL,
			      &len);

      if (data)
	input_cgraph_opt_section (file_data, data, len, nodes);
    }
}
