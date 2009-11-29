/* Write and read the cgraph to the memory mapped representation of a
   .o file.

   Copyright 2009 Free Software Foundation, Inc.
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
#include "toplev.h"
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "varray.h"
#include "hashtab.h"
#include "langhooks.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "except.h"
#include "vec.h"
#include "timevar.h"
#include "output.h"
#include "pointer-set.h"
#include "lto-streamer.h"
#include "gcov-io.h"

/* Create a new cgraph encoder.  */

lto_cgraph_encoder_t
lto_cgraph_encoder_new (void)
{
  lto_cgraph_encoder_t encoder = XCNEW (struct lto_cgraph_encoder_d);
  encoder->map = pointer_map_create ();
  encoder->nodes = NULL;
  return encoder;
}


/* Delete ENCODER and its components.  */

void
lto_cgraph_encoder_delete (lto_cgraph_encoder_t encoder)
{
   VEC_free (cgraph_node_ptr, heap, encoder->nodes);
   pointer_map_destroy (encoder->map);
   free (encoder);
}


/* Return the existing reference number of NODE in the cgraph encoder in
   output block OB.  Assign a new reference if this is the first time
   NODE is encoded.  */

int
lto_cgraph_encoder_encode (lto_cgraph_encoder_t encoder,
			   struct cgraph_node *node)
{
  int ref;
  void **slot;

  slot = pointer_map_contains (encoder->map, node);
  if (!slot)
    {
      ref = VEC_length (cgraph_node_ptr, encoder->nodes);
      slot = pointer_map_insert (encoder->map, node);
      *slot = (void *) (intptr_t) ref;
      VEC_safe_push (cgraph_node_ptr, heap, encoder->nodes, node);
    }
  else
    ref = (int) (intptr_t) *slot;

  return ref;
}


/* Look up NODE in encoder.  Return NODE's reference if it has been encoded
   or LCC_NOT_FOUND if it is not there.  */

int
lto_cgraph_encoder_lookup (lto_cgraph_encoder_t encoder,
			   struct cgraph_node *node)
{
  void **slot = pointer_map_contains (encoder->map, node);
  return (slot ? (int) (intptr_t) *slot : LCC_NOT_FOUND);
}


/* Return the cgraph node corresponding to REF using ENCODER.  */

struct cgraph_node *
lto_cgraph_encoder_deref (lto_cgraph_encoder_t encoder, int ref)
{
  if (ref == LCC_NOT_FOUND)
    return NULL;

  return VEC_index (cgraph_node_ptr, encoder->nodes, ref);
}


/* Return number of encoded nodes in ENCODER.  */

static int
lto_cgraph_encoder_size (lto_cgraph_encoder_t encoder)
{
  return VEC_length (cgraph_node_ptr, encoder->nodes);
}


/* Output the cgraph EDGE to OB using ENCODER.  */

static void
lto_output_edge (struct lto_simple_output_block *ob, struct cgraph_edge *edge,
		 lto_cgraph_encoder_t encoder)
{
  unsigned int uid;
  intptr_t ref;
  struct bitpack_d *bp;

  lto_output_uleb128_stream (ob->main_stream, LTO_cgraph_edge);

  ref = lto_cgraph_encoder_lookup (encoder, edge->caller);
  gcc_assert (ref != LCC_NOT_FOUND);
  lto_output_sleb128_stream (ob->main_stream, ref);

  ref = lto_cgraph_encoder_lookup (encoder, edge->callee);
  gcc_assert (ref != LCC_NOT_FOUND);
  lto_output_sleb128_stream (ob->main_stream, ref);

  lto_output_sleb128_stream (ob->main_stream, edge->count);

  bp = bitpack_create ();
  uid = flag_wpa ? edge->lto_stmt_uid : gimple_uid (edge->call_stmt);
  bp_pack_value (bp, uid, HOST_BITS_PER_INT);
  bp_pack_value (bp, edge->inline_failed, HOST_BITS_PER_INT);
  bp_pack_value (bp, edge->frequency, HOST_BITS_PER_INT);
  bp_pack_value (bp, edge->loop_nest, 30);
  bp_pack_value (bp, edge->indirect_call, 1);
  bp_pack_value (bp, edge->call_stmt_cannot_inline_p, 1);
  bp_pack_value (bp, edge->can_throw_external, 1);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);
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
		 lto_cgraph_encoder_t encoder, cgraph_node_set set,
		 bitmap written_decls)
{
  unsigned int tag;
  struct bitpack_d *bp;
  unsigned local, externally_visible, inlinable, analyzed;
  bool boundary_p, wrote_decl_p;
  intptr_t ref;

  boundary_p = !cgraph_node_in_set_p (node, set);
  wrote_decl_p = bitmap_bit_p (written_decls, DECL_UID (node->decl));

  switch (cgraph_function_body_availability (node))
    {
    case AVAIL_NOT_AVAILABLE:
      tag = LTO_cgraph_unavail_node;
      break;

    case AVAIL_AVAILABLE:
    case AVAIL_LOCAL:
      tag = LTO_cgraph_avail_node;
      break;

    case AVAIL_OVERWRITABLE:
      tag = LTO_cgraph_overwritable_node;
      break;

    default:
      gcc_unreachable ();
    }

  if (boundary_p)
    tag = LTO_cgraph_unavail_node;

  lto_output_uleb128_stream (ob->main_stream, tag);

  local = node->local.local;
  externally_visible = node->local.externally_visible;
  inlinable = node->local.inlinable;
  analyzed = node->analyzed;

  /* In WPA mode, we only output part of the call-graph.  Also, we
     fake cgraph node attributes.  There are two cases that we care.

     Boundary nodes: There are nodes that are not part of SET but are
     called from within SET.  We artificially make them look like
     externally visible nodes with no function body.

     Cherry-picked nodes:  These are nodes we pulled from other
     translation units into SET during IPA-inlining.  We make them as
     local static nodes to prevent clashes with other local statics.  */
  if (boundary_p)
    {
      /* Inline clones can not be part of boundary.  */
      gcc_assert (!node->global.inlined_to);
      local = 0;
      externally_visible = 1;
      inlinable = 0;
      analyzed = 0;
    }
  else if (lto_forced_extern_inline_p (node->decl))
    {
      local = 1;
      externally_visible = 0;
      inlinable = 1;
    }

  lto_output_uleb128_stream (ob->main_stream, wrote_decl_p);

  if (!wrote_decl_p)
    bitmap_set_bit (written_decls, DECL_UID (node->decl));

  lto_output_fn_decl_index (ob->decl_state, ob->main_stream, node->decl);
  lto_output_sleb128_stream (ob->main_stream, node->count);

  bp = bitpack_create ();
  bp_pack_value (bp, local, 1);
  bp_pack_value (bp, externally_visible, 1);
  bp_pack_value (bp, node->local.finalized, 1);
  bp_pack_value (bp, inlinable, 1);
  bp_pack_value (bp, node->local.disregard_inline_limits, 1);
  bp_pack_value (bp, node->local.redefined_extern_inline, 1);
  bp_pack_value (bp, node->local.for_functions_valid, 1);
  bp_pack_value (bp, node->local.vtable_method, 1);
  bp_pack_value (bp, node->needed, 1);
  bp_pack_value (bp, node->address_taken, 1);
  bp_pack_value (bp, node->abstract_and_needed, 1);
  bp_pack_value (bp, node->reachable, 1);
  bp_pack_value (bp, node->lowered, 1);
  bp_pack_value (bp, analyzed, 1);
  bp_pack_value (bp, node->process, 1);
  bp_pack_value (bp, node->alias, 1);
  bp_pack_value (bp, node->finalized_by_frontend, 1);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);

  if (tag != LTO_cgraph_unavail_node)
    {
      lto_output_sleb128_stream (ob->main_stream,
				 node->local.inline_summary.estimated_self_stack_size);
      lto_output_sleb128_stream (ob->main_stream,
				 node->local.inline_summary.self_size);
      lto_output_sleb128_stream (ob->main_stream,
				 node->local.inline_summary.size_inlining_benefit);
      lto_output_sleb128_stream (ob->main_stream,
				 node->local.inline_summary.self_time);
      lto_output_sleb128_stream (ob->main_stream,
				 node->local.inline_summary.time_inlining_benefit);
    }

  /* FIXME lto: Outputting global info is not neccesary until after
     inliner was run.  Global structure holds results of propagation
     done by inliner.  */
  lto_output_sleb128_stream (ob->main_stream,
			     node->global.estimated_stack_size);
  lto_output_sleb128_stream (ob->main_stream,
			     node->global.stack_frame_offset);
  if (node->global.inlined_to && !boundary_p)
    {
      ref = lto_cgraph_encoder_lookup (encoder, node->global.inlined_to);
      gcc_assert (ref != LCC_NOT_FOUND);
    }
  else
    ref = LCC_NOT_FOUND;
  lto_output_sleb128_stream (ob->main_stream, ref);

  lto_output_sleb128_stream (ob->main_stream, node->global.time);
  lto_output_sleb128_stream (ob->main_stream, node->global.size);
  lto_output_sleb128_stream (ob->main_stream,
			     node->global.estimated_growth);
  lto_output_uleb128_stream (ob->main_stream, node->global.inlined);
  if (node->same_body)
    {
      struct cgraph_node *alias;
      unsigned long alias_count = 1;
      for (alias = node->same_body; alias->next; alias = alias->next)
	alias_count++;
      lto_output_uleb128_stream (ob->main_stream, alias_count);
      do
	{
	  lto_output_fn_decl_index (ob->decl_state, ob->main_stream,
				    alias->decl);
	  if (alias->thunk.thunk_p)
	    {
              lto_output_uleb128_stream
	         (ob->main_stream,
	      	  1 + (alias->thunk.this_adjusting != 0) * 2
		  + (alias->thunk.virtual_offset_p != 0) * 4);
	      lto_output_uleb128_stream (ob->main_stream,
	      				 alias->thunk.fixed_offset);
	      lto_output_uleb128_stream (ob->main_stream,
	      				 alias->thunk.virtual_value);
	      lto_output_fn_decl_index (ob->decl_state, ob->main_stream,
					alias->thunk.alias);
	    }
	  else
            lto_output_uleb128_stream (ob->main_stream, 0);
	  alias = alias->previous;
	}
      while (alias);
    }
  else
    lto_output_uleb128_stream (ob->main_stream, 0);
}

/* Stream out profile_summary to OB.  */

static void
output_profile_summary (struct lto_simple_output_block *ob)
{
  if (profile_info)
    {
      /* We do not output num, it is not terribly useful.  */
      gcc_assert (profile_info->runs);
      lto_output_uleb128_stream (ob->main_stream, profile_info->runs);
      lto_output_sleb128_stream (ob->main_stream, profile_info->sum_all);
      lto_output_sleb128_stream (ob->main_stream, profile_info->run_max);
      lto_output_sleb128_stream (ob->main_stream, profile_info->sum_max);
    }
  else
    lto_output_uleb128_stream (ob->main_stream, 0);
}


/* Output the part of the cgraph in SET.  */

void
output_cgraph (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct lto_simple_output_block *ob;
  cgraph_node_set_iterator csi;
  struct cgraph_edge *edge;
  int i, n_nodes;
  bitmap written_decls;
  lto_cgraph_encoder_t encoder;
  struct cgraph_asm_node *can;

  ob = lto_create_simple_output_block (LTO_section_cgraph);

  output_profile_summary (ob);

  /* An encoder for cgraph nodes should have been created by
     ipa_write_summaries_1.  */
  gcc_assert (ob->decl_state->cgraph_node_encoder);
  encoder = ob->decl_state->cgraph_node_encoder;

  /* The FUNCTION_DECLs for which we have written a node.  The first
     node found is written as the "original" node, the remaining nodes
     are considered its clones.  */
  written_decls = lto_bitmap_alloc ();

  /* Go over all the nodes in SET and assign references.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      lto_cgraph_encoder_encode (encoder, node);
    }

  /* Go over all the nodes again to include callees that are not in
     SET.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  struct cgraph_node *callee = edge->callee;
	  if (!cgraph_node_in_set_p (callee, set))
	    {
	      /* We should have moved all the inlines.  */
	      gcc_assert (!callee->global.inlined_to);
	      lto_cgraph_encoder_encode (encoder, callee);
	    }
	}
    }

  /* Write out the nodes.  */
  n_nodes = lto_cgraph_encoder_size (encoder);
  for (i = 0; i < n_nodes; i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      lto_output_node (ob, node, encoder, set, written_decls);
    }

  lto_bitmap_free (written_decls);

  /* Go over the nodes in SET again to write edges.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->callees)
        {
	  /* Output edges in backward direction, so the reconstructed callgraph
	     match and it is easy to associate call sites in the IPA pass summaries.  */
	  edge = node->callees;
	  while (edge->next_callee)
	    edge = edge->next_callee;
	  for (; edge; edge = edge->prev_callee)
	    lto_output_edge (ob, edge, encoder);
	}
    }

  lto_output_uleb128_stream (ob->main_stream, 0);

  /* Emit toplevel asms.  */
  for (can = cgraph_asm_nodes; can; can = can->next)
    {
      int len = TREE_STRING_LENGTH (can->asm_str);
      lto_output_uleb128_stream (ob->main_stream, len);
      for (i = 0; i < len; ++i)
	lto_output_1_stream (ob->main_stream,
			     TREE_STRING_POINTER (can->asm_str)[i]);
    }

  lto_output_uleb128_stream (ob->main_stream, 0);

  lto_destroy_simple_output_block (ob);
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
		      enum LTO_cgraph_tags tag,
		      struct bitpack_d *bp,
		      unsigned int stack_size,
		      unsigned int self_time,
		      unsigned int time_inlining_benefit,
		      unsigned int self_size,
		      unsigned int size_inlining_benefit)
{
  node->aux = (void *) tag;
  node->local.inline_summary.estimated_self_stack_size = stack_size;
  node->local.inline_summary.self_time = self_time;
  node->local.inline_summary.time_inlining_benefit = time_inlining_benefit;
  node->local.inline_summary.self_size = self_size;
  node->local.inline_summary.size_inlining_benefit = size_inlining_benefit;
  node->global.time = self_time;
  node->global.size = self_size;
  node->local.lto_file_data = file_data;

  node->local.local = bp_unpack_value (bp, 1);
  node->local.externally_visible = bp_unpack_value (bp, 1);
  node->local.finalized = bp_unpack_value (bp, 1);
  node->local.inlinable = bp_unpack_value (bp, 1);
  node->local.disregard_inline_limits = bp_unpack_value (bp, 1);
  node->local.redefined_extern_inline = bp_unpack_value (bp, 1);
  node->local.for_functions_valid = bp_unpack_value (bp, 1);
  node->local.vtable_method = bp_unpack_value (bp, 1);
  node->needed = bp_unpack_value (bp, 1);
  node->address_taken = bp_unpack_value (bp, 1);
  node->abstract_and_needed = bp_unpack_value (bp, 1);
  node->reachable = bp_unpack_value (bp, 1);
  node->lowered = bp_unpack_value (bp, 1);
  node->analyzed = bp_unpack_value (bp, 1);
  node->process = bp_unpack_value (bp, 1);
  node->alias = bp_unpack_value (bp, 1);
  node->finalized_by_frontend = bp_unpack_value (bp, 1);
}


/* Read a node from input_block IB.  TAG is the node's tag just read.
   Return the node read or overwriten.  */

static struct cgraph_node *
input_node (struct lto_file_decl_data *file_data,
	    struct lto_input_block *ib,
	    enum LTO_cgraph_tags tag)
{
  tree fn_decl;
  struct cgraph_node *node;
  struct bitpack_d *bp;
  int stack_size = 0;
  unsigned decl_index;
  bool clone_p;
  int estimated_stack_size = 0;
  int stack_frame_offset = 0;
  int ref = LCC_NOT_FOUND;
  int estimated_growth = 0;
  int time = 0;
  int size = 0;
  int self_time = 0;
  int self_size = 0;
  int time_inlining_benefit = 0;
  int size_inlining_benefit = 0;
  unsigned long same_body_count = 0;
  bool inlined = false;

  clone_p = (lto_input_uleb128 (ib) != 0);

  decl_index = lto_input_uleb128 (ib);
  fn_decl = lto_file_decl_data_get_fn_decl (file_data, decl_index);

  if (clone_p)
    node = cgraph_clone_node (cgraph_node (fn_decl), 0,
			      CGRAPH_FREQ_BASE, 0, false, NULL);

  else
    node = cgraph_node (fn_decl);

  node->count = lto_input_sleb128 (ib);
  bp = lto_input_bitpack (ib);

  if (tag != LTO_cgraph_unavail_node)
    {
      stack_size = lto_input_sleb128 (ib);
      self_size = lto_input_sleb128 (ib);
      size_inlining_benefit = lto_input_sleb128 (ib);
      self_time = lto_input_sleb128 (ib);
      time_inlining_benefit = lto_input_sleb128 (ib);
    }

  estimated_stack_size = lto_input_sleb128 (ib);
  stack_frame_offset = lto_input_sleb128 (ib);
  ref = lto_input_sleb128 (ib);
  time = lto_input_sleb128 (ib);
  size = lto_input_sleb128 (ib);
  estimated_growth = lto_input_sleb128 (ib);
  inlined = lto_input_uleb128 (ib);
  same_body_count = lto_input_uleb128 (ib);

  /* Make sure that we have not read this node before.  Nodes that
     have already been read will have their tag stored in the 'aux'
     field.  Since built-in functions can be referenced in multiple
     functions, they are expected to be read more than once.  */
  if (node->aux && !DECL_IS_BUILTIN (node->decl))
    internal_error ("bytecode stream: found multiple instances of cgraph "
		    "node %d", node->uid);

  input_overwrite_node (file_data, node, tag, bp, stack_size, self_time,
  			time_inlining_benefit, self_size,
			size_inlining_benefit);
  bitpack_delete (bp);

  node->global.estimated_stack_size = estimated_stack_size;
  node->global.stack_frame_offset = stack_frame_offset;
  node->global.time = time;
  node->global.size = size;

  /* Store a reference for now, and fix up later to be a pointer.  */
  node->global.inlined_to = (cgraph_node_ptr) (intptr_t) ref;

  node->global.estimated_growth = estimated_growth;
  node->global.inlined = inlined;

  while (same_body_count-- > 0)
    {
      tree alias_decl;
      int type;
      decl_index = lto_input_uleb128 (ib);
      alias_decl = lto_file_decl_data_get_fn_decl (file_data, decl_index);
      type = lto_input_uleb128 (ib);
      if (!type)
        cgraph_same_body_alias (alias_decl, fn_decl);
      else
        {
	  HOST_WIDE_INT fixed_offset = lto_input_uleb128 (ib);
	  HOST_WIDE_INT virtual_value = lto_input_uleb128 (ib);
	  tree real_alias;
	  decl_index = lto_input_uleb128 (ib);
	  real_alias = lto_file_decl_data_get_fn_decl (file_data, decl_index);
	  cgraph_add_thunk (alias_decl, fn_decl, type & 2, fixed_offset,
	  		    virtual_value,
			    (type & 4) ? size_int (virtual_value) : NULL_TREE,
			    real_alias);
	}
    }
  return node;
}


/* Read an edge from IB.  NODES points to a vector of previously read
   nodes for decoding caller and callee of the edge to be read.  */

static void
input_edge (struct lto_input_block *ib, VEC(cgraph_node_ptr, heap) *nodes)
{
  struct cgraph_node *caller, *callee;
  struct cgraph_edge *edge;
  unsigned int stmt_id;
  gcov_type count;
  int freq;
  unsigned int nest;
  cgraph_inline_failed_t inline_failed;
  struct bitpack_d *bp;
  enum ld_plugin_symbol_resolution caller_resolution;

  caller = VEC_index (cgraph_node_ptr, nodes, lto_input_sleb128 (ib));
  if (caller == NULL || caller->decl == NULL_TREE)
    internal_error ("bytecode stream: no caller found while reading edge");

  callee = VEC_index (cgraph_node_ptr, nodes, lto_input_sleb128 (ib));
  if (callee == NULL || callee->decl == NULL_TREE)
    internal_error ("bytecode stream: no callee found while reading edge");

  count = (gcov_type) lto_input_sleb128 (ib);

  bp = lto_input_bitpack (ib);
  stmt_id = (unsigned int) bp_unpack_value (bp, HOST_BITS_PER_INT);
  inline_failed = (cgraph_inline_failed_t) bp_unpack_value (bp,
							    HOST_BITS_PER_INT);
  freq = (int) bp_unpack_value (bp, HOST_BITS_PER_INT);
  nest = (unsigned) bp_unpack_value (bp, 30);

  /* If the caller was preempted, don't create the edge.
     ???  Should we ever have edges from a preempted caller?  */
  caller_resolution = lto_symtab_get_resolution (caller->decl);
  if (caller_resolution == LDPR_PREEMPTED_REG
      || caller_resolution == LDPR_PREEMPTED_IR)
    return;

  edge = cgraph_create_edge (caller, callee, NULL, count, freq, nest);
  edge->lto_stmt_uid = stmt_id;
  edge->inline_failed = inline_failed;
  edge->indirect_call = bp_unpack_value (bp, 1);
  edge->call_stmt_cannot_inline_p = bp_unpack_value (bp, 1);
  edge->can_throw_external = bp_unpack_value (bp, 1);
  bitpack_delete (bp);
}


/* Read a cgraph from IB using the info in FILE_DATA.  */

static void
input_cgraph_1 (struct lto_file_decl_data *file_data,
		struct lto_input_block *ib)
{
  enum LTO_cgraph_tags tag;
  VEC(cgraph_node_ptr, heap) *nodes = NULL;
  struct cgraph_node *node;
  unsigned i;
  unsigned HOST_WIDE_INT len;

  tag = (enum LTO_cgraph_tags) lto_input_uleb128 (ib);
  while (tag)
    {
      if (tag == LTO_cgraph_edge)
        input_edge (ib, nodes);
      else
	{
	  node = input_node (file_data, ib, tag);
	  if (node == NULL || node->decl == NULL_TREE)
	    internal_error ("bytecode stream: found empty cgraph node");
	  VEC_safe_push (cgraph_node_ptr, heap, nodes, node);
	  lto_cgraph_encoder_encode (file_data->cgraph_node_encoder, node);
	}

      tag = (enum LTO_cgraph_tags) lto_input_uleb128 (ib);
    }

  /* Input toplevel asms.  */
  len = lto_input_uleb128 (ib);
  while (len)
    {
      char *str = (char *)xmalloc (len + 1);
      for (i = 0; i < len; ++i)
	str[i] = lto_input_1_unsigned (ib);
      cgraph_add_asm_node (build_string (len, str));
      free (str);

      len = lto_input_uleb128 (ib);
    }

  for (i = 0; VEC_iterate (cgraph_node_ptr, nodes, i, node); i++)
    {
      const int ref = (int) (intptr_t) node->global.inlined_to;

      /* Fixup inlined_to from reference to pointer.  */
      if (ref != LCC_NOT_FOUND)
	node->global.inlined_to = VEC_index (cgraph_node_ptr, nodes, ref);
      else
	node->global.inlined_to = NULL;
    }

  VEC_free (cgraph_node_ptr, heap, nodes);
}

static struct gcov_ctr_summary lto_gcov_summary;

/* Input profile_info from IB.  */
static void
input_profile_summary (struct lto_input_block *ib)
{
  unsigned int runs = lto_input_uleb128 (ib);
  if (runs)
    {
      if (!profile_info)
        {
	  profile_info = &lto_gcov_summary;
	  lto_gcov_summary.runs = runs;
	  lto_gcov_summary.sum_all = lto_input_sleb128 (ib);
	  lto_gcov_summary.run_max = lto_input_sleb128 (ib);
	  lto_gcov_summary.sum_max = lto_input_sleb128 (ib);
	}
      /* We can support this by scaling all counts to nearest common multiple
         of all different runs, but it is perhaps not worth the effort.  */
      else if (profile_info->runs != runs
	       || profile_info->sum_all != lto_input_sleb128 (ib)
	       || profile_info->run_max != lto_input_sleb128 (ib)
	       || profile_info->sum_max != lto_input_sleb128 (ib))
	sorry ("Combining units with different profiles is not supported.");
      /* We allow some units to have profile and other to not have one.  This will
         just make unprofiled units to be size optimized that is sane.  */
    }

}

/* Input and merge the cgraph from each of the .o files passed to
   lto1.  */

void
input_cgraph (void)
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

      ib = lto_create_simple_input_block (file_data, LTO_section_cgraph,
					  &data, &len);
      input_profile_summary (ib);
      file_data->cgraph_node_encoder = lto_cgraph_encoder_new ();
      input_cgraph_1 (file_data, ib);
      lto_destroy_simple_input_block (file_data, LTO_section_cgraph,
				      ib, data, len);

      /* Assume that every file read needs to be processed by LTRANS.  */
      if (flag_wpa)
	lto_mark_file_for_ltrans (file_data);
    }

  /* Clear out the aux field that was used to store enough state to
     tell which nodes should be overwritten.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      /* Some nodes may have been created by cgraph_node.  This
	 happens when the callgraph contains nested functions.  If the
	 node for the parent function was never emitted to the gimple
	 file, cgraph_node will create a node for it when setting the
	 context of the nested function.  */
      if (node->local.lto_file_data)
	node->aux = NULL;
    }
}
