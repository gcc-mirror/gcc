/* Read the GIMPLE representation from a file stream.

   Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
#include "toplev.h"
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
#include "diagnostic.h"
#include "except.h"
#include "debug.h"
#include "vec.h"
#include "ipa-utils.h"
#include "data-streamer.h"
#include "gimple-streamer.h"
#include "lto-streamer.h"
#include "tree-streamer.h"
#include "tree-pass.h"
#include "streamer-hooks.h"
#include "cfgloop.h"


struct freeing_string_slot_hasher : string_slot_hasher
{
  static inline void remove (value_type *);
};

inline void
freeing_string_slot_hasher::remove (value_type *v)
{
  free (v);
}

/* The table to hold the file names.  */
static hash_table <freeing_string_slot_hasher> file_name_hash_table;


/* Check that tag ACTUAL has one of the given values.  NUM_TAGS is the
   number of valid tag values to check.  */

void
lto_tag_check_set (enum LTO_tags actual, int ntags, ...)
{
  va_list ap;
  int i;

  va_start (ap, ntags);
  for (i = 0; i < ntags; i++)
    if ((unsigned) actual == va_arg (ap, unsigned))
      {
	va_end (ap);
	return;
      }

  va_end (ap);
  internal_error ("bytecode stream: unexpected tag %s", lto_tag_name (actual));
}


/* Read LENGTH bytes from STREAM to ADDR.  */

void
lto_input_data_block (struct lto_input_block *ib, void *addr, size_t length)
{
  size_t i;
  unsigned char *const buffer = (unsigned char *const) addr;

  for (i = 0; i < length; i++)
    buffer[i] = streamer_read_uchar (ib);
}


/* Lookup STRING in file_name_hash_table.  If found, return the existing
   string, otherwise insert STRING as the canonical version.  */

static const char *
canon_file_name (const char *string)
{
  string_slot **slot;
  struct string_slot s_slot;
  size_t len = strlen (string);

  s_slot.s = string;
  s_slot.len = len;

  slot = file_name_hash_table.find_slot (&s_slot, INSERT);
  if (*slot == NULL)
    {
      char *saved_string;
      struct string_slot *new_slot;

      saved_string = (char *) xmalloc (len + 1);
      new_slot = XCNEW (struct string_slot);
      memcpy (saved_string, string, len + 1);
      new_slot->s = saved_string;
      new_slot->len = len;
      *slot = new_slot;
      return saved_string;
    }
  else
    {
      struct string_slot *old_slot = *slot;
      return old_slot->s;
    }
}


/* Read a location bitpack from input block IB.  */

location_t
lto_input_location (struct bitpack_d *bp, struct data_in *data_in)
{
  static const char *current_file;
  static int current_line;
  static int current_col;
  bool file_change, line_change, column_change;
  unsigned len;
  bool prev_file = current_file != NULL;

  if (bp_unpack_value (bp, 1))
    return UNKNOWN_LOCATION;

  file_change = bp_unpack_value (bp, 1);
  line_change = bp_unpack_value (bp, 1);
  column_change = bp_unpack_value (bp, 1);

  if (file_change)
    current_file = canon_file_name
		     (string_for_index (data_in,
					bp_unpack_var_len_unsigned (bp),
					&len));

  if (line_change)
    current_line = bp_unpack_var_len_unsigned (bp);

  if (column_change)
    current_col = bp_unpack_var_len_unsigned (bp);

  if (file_change)
    {
      if (prev_file)
	linemap_add (line_table, LC_LEAVE, false, NULL, 0);

      linemap_add (line_table, LC_ENTER, false, current_file, current_line);
    }
  else if (line_change)
    linemap_line_start (line_table, current_line, current_col);

  return linemap_position_for_column (line_table, current_col);
}


/* Read a reference to a tree node from DATA_IN using input block IB.
   TAG is the expected node that should be found in IB, if TAG belongs
   to one of the indexable trees, expect to read a reference index to
   be looked up in one of the symbol tables, otherwise read the pysical
   representation of the tree using stream_read_tree.  FN is the
   function scope for the read tree.  */

tree
lto_input_tree_ref (struct lto_input_block *ib, struct data_in *data_in,
		    struct function *fn, enum LTO_tags tag)
{
  unsigned HOST_WIDE_INT ix_u;
  tree result = NULL_TREE;

  lto_tag_check_range (tag, LTO_field_decl_ref, LTO_global_decl_ref);

  switch (tag)
    {
    case LTO_type_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_type (data_in->file_data, ix_u);
      break;

    case LTO_ssa_name_ref:
      ix_u = streamer_read_uhwi (ib);
      result = (*SSANAMES (fn))[ix_u];
      break;

    case LTO_field_decl_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_field_decl (data_in->file_data, ix_u);
      break;

    case LTO_function_decl_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_fn_decl (data_in->file_data, ix_u);
      break;

    case LTO_type_decl_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_type_decl (data_in->file_data, ix_u);
      break;

    case LTO_namespace_decl_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_namespace_decl (data_in->file_data, ix_u);
      break;

    case LTO_global_decl_ref:
    case LTO_result_decl_ref:
    case LTO_const_decl_ref:
    case LTO_imported_decl_ref:
    case LTO_label_decl_ref:
    case LTO_translation_unit_decl_ref:
      ix_u = streamer_read_uhwi (ib);
      result = lto_file_decl_data_get_var_decl (data_in->file_data, ix_u);
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (result);

  return result;
}


/* Read and return a double-linked list of catch handlers from input
   block IB, using descriptors in DATA_IN.  */

static struct eh_catch_d *
lto_input_eh_catch_list (struct lto_input_block *ib, struct data_in *data_in,
			 eh_catch *last_p)
{
  eh_catch first;
  enum LTO_tags tag;

  *last_p = first = NULL;
  tag = streamer_read_record_start (ib);
  while (tag)
    {
      tree list;
      eh_catch n;

      lto_tag_check_range (tag, LTO_eh_catch, LTO_eh_catch);

      /* Read the catch node.  */
      n = ggc_alloc_cleared_eh_catch_d ();
      n->type_list = stream_read_tree (ib, data_in);
      n->filter_list = stream_read_tree (ib, data_in);
      n->label = stream_read_tree (ib, data_in);

      /* Register all the types in N->FILTER_LIST.  */
      for (list = n->filter_list; list; list = TREE_CHAIN (list))
	add_type_for_runtime (TREE_VALUE (list));

      /* Chain N to the end of the list.  */
      if (*last_p)
	(*last_p)->next_catch = n;
      n->prev_catch = *last_p;
      *last_p = n;

      /* Set the head of the list the first time through the loop.  */
      if (first == NULL)
	first = n;

      tag = streamer_read_record_start (ib);
    }

  return first;
}


/* Read and return EH region IX from input block IB, using descriptors
   in DATA_IN.  */

static eh_region
input_eh_region (struct lto_input_block *ib, struct data_in *data_in, int ix)
{
  enum LTO_tags tag;
  eh_region r;

  /* Read the region header.  */
  tag = streamer_read_record_start (ib);
  if (tag == LTO_null)
    return NULL;

  r = ggc_alloc_cleared_eh_region_d ();
  r->index = streamer_read_hwi (ib);

  gcc_assert (r->index == ix);

  /* Read all the region pointers as region numbers.  We'll fix up
     the pointers once the whole array has been read.  */
  r->outer = (eh_region) (intptr_t) streamer_read_hwi (ib);
  r->inner = (eh_region) (intptr_t) streamer_read_hwi (ib);
  r->next_peer = (eh_region) (intptr_t) streamer_read_hwi (ib);

  switch (tag)
    {
      case LTO_ert_cleanup:
	r->type = ERT_CLEANUP;
	break;

      case LTO_ert_try:
	{
	  struct eh_catch_d *last_catch;
	  r->type = ERT_TRY;
	  r->u.eh_try.first_catch = lto_input_eh_catch_list (ib, data_in,
							     &last_catch);
	  r->u.eh_try.last_catch = last_catch;
	  break;
	}

      case LTO_ert_allowed_exceptions:
	{
	  tree l;

	  r->type = ERT_ALLOWED_EXCEPTIONS;
	  r->u.allowed.type_list = stream_read_tree (ib, data_in);
	  r->u.allowed.label = stream_read_tree (ib, data_in);
	  r->u.allowed.filter = streamer_read_uhwi (ib);

	  for (l = r->u.allowed.type_list; l ; l = TREE_CHAIN (l))
	    add_type_for_runtime (TREE_VALUE (l));
	}
	break;

      case LTO_ert_must_not_throw:
	{
	  r->type = ERT_MUST_NOT_THROW;
	  r->u.must_not_throw.failure_decl = stream_read_tree (ib, data_in);
	  bitpack_d bp = streamer_read_bitpack (ib);
	  r->u.must_not_throw.failure_loc
	   = stream_input_location (&bp, data_in);
	}
	break;

      default:
	gcc_unreachable ();
    }

  r->landing_pads = (eh_landing_pad) (intptr_t) streamer_read_hwi (ib);

  return r;
}


/* Read and return EH landing pad IX from input block IB, using descriptors
   in DATA_IN.  */

static eh_landing_pad
input_eh_lp (struct lto_input_block *ib, struct data_in *data_in, int ix)
{
  enum LTO_tags tag;
  eh_landing_pad lp;

  /* Read the landing pad header.  */
  tag = streamer_read_record_start (ib);
  if (tag == LTO_null)
    return NULL;

  lto_tag_check_range (tag, LTO_eh_landing_pad, LTO_eh_landing_pad);

  lp = ggc_alloc_cleared_eh_landing_pad_d ();
  lp->index = streamer_read_hwi (ib);
  gcc_assert (lp->index == ix);
  lp->next_lp = (eh_landing_pad) (intptr_t) streamer_read_hwi (ib);
  lp->region = (eh_region) (intptr_t) streamer_read_hwi (ib);
  lp->post_landing_pad = stream_read_tree (ib, data_in);

  return lp;
}


/* After reading the EH regions, pointers to peer and children regions
   are region numbers.  This converts all these region numbers into
   real pointers into the rematerialized regions for FN.  ROOT_REGION
   is the region number for the root EH region in FN.  */

static void
fixup_eh_region_pointers (struct function *fn, HOST_WIDE_INT root_region)
{
  unsigned i;
  vec<eh_region, va_gc> *eh_array = fn->eh->region_array;
  vec<eh_landing_pad, va_gc> *lp_array = fn->eh->lp_array;
  eh_region r;
  eh_landing_pad lp;

  gcc_assert (eh_array && lp_array);

  gcc_assert (root_region >= 0);
  fn->eh->region_tree = (*eh_array)[root_region];

#define FIXUP_EH_REGION(r) (r) = (*eh_array)[(HOST_WIDE_INT) (intptr_t) (r)]
#define FIXUP_EH_LP(p) (p) = (*lp_array)[(HOST_WIDE_INT) (intptr_t) (p)]

  /* Convert all the index numbers stored in pointer fields into
     pointers to the corresponding slots in the EH region array.  */
  FOR_EACH_VEC_ELT (*eh_array, i, r)
    {
      /* The array may contain NULL regions.  */
      if (r == NULL)
	continue;

      gcc_assert (i == (unsigned) r->index);
      FIXUP_EH_REGION (r->outer);
      FIXUP_EH_REGION (r->inner);
      FIXUP_EH_REGION (r->next_peer);
      FIXUP_EH_LP (r->landing_pads);
    }

  /* Convert all the index numbers stored in pointer fields into
     pointers to the corresponding slots in the EH landing pad array.  */
  FOR_EACH_VEC_ELT (*lp_array, i, lp)
    {
      /* The array may contain NULL landing pads.  */
      if (lp == NULL)
	continue;

      gcc_assert (i == (unsigned) lp->index);
      FIXUP_EH_LP (lp->next_lp);
      FIXUP_EH_REGION (lp->region);
    }

#undef FIXUP_EH_REGION
#undef FIXUP_EH_LP
}


/* Initialize EH support.  */

void
lto_init_eh (void)
{
  static bool eh_initialized_p = false;

  if (eh_initialized_p)
    return;

  /* Contrary to most other FEs, we only initialize EH support when at
     least one of the files in the set contains exception regions in
     it.  Since this happens much later than the call to init_eh in
     lang_dependent_init, we have to set flag_exceptions and call
     init_eh again to initialize the EH tables.  */
  flag_exceptions = 1;
  init_eh ();

  eh_initialized_p = true;
}


/* Read the exception table for FN from IB using the data descriptors
   in DATA_IN.  */

static void
input_eh_regions (struct lto_input_block *ib, struct data_in *data_in,
		  struct function *fn)
{
  HOST_WIDE_INT i, root_region, len;
  enum LTO_tags tag;

  tag = streamer_read_record_start (ib);
  if (tag == LTO_null)
    return;

  lto_tag_check_range (tag, LTO_eh_table, LTO_eh_table);

  /* If the file contains EH regions, then it was compiled with
     -fexceptions.  In that case, initialize the backend EH
     machinery.  */
  lto_init_eh ();

  gcc_assert (fn->eh);

  root_region = streamer_read_hwi (ib);
  gcc_assert (root_region == (int) root_region);

  /* Read the EH region array.  */
  len = streamer_read_hwi (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      vec_safe_grow_cleared (fn->eh->region_array, len);
      for (i = 0; i < len; i++)
	{
	  eh_region r = input_eh_region (ib, data_in, i);
	  (*fn->eh->region_array)[i] = r;
	}
    }

  /* Read the landing pads.  */
  len = streamer_read_hwi (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      vec_safe_grow_cleared (fn->eh->lp_array, len);
      for (i = 0; i < len; i++)
	{
	  eh_landing_pad lp = input_eh_lp (ib, data_in, i);
	  (*fn->eh->lp_array)[i] = lp;
	}
    }

  /* Read the runtime type data.  */
  len = streamer_read_hwi (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      vec_safe_grow_cleared (fn->eh->ttype_data, len);
      for (i = 0; i < len; i++)
	{
	  tree ttype = stream_read_tree (ib, data_in);
	  (*fn->eh->ttype_data)[i] = ttype;
	}
    }

  /* Read the table of action chains.  */
  len = streamer_read_hwi (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      if (targetm.arm_eabi_unwinder)
	{
	  vec_safe_grow_cleared (fn->eh->ehspec_data.arm_eabi, len);
	  for (i = 0; i < len; i++)
	    {
	      tree t = stream_read_tree (ib, data_in);
	      (*fn->eh->ehspec_data.arm_eabi)[i] = t;
	    }
	}
      else
	{
	  vec_safe_grow_cleared (fn->eh->ehspec_data.other, len);
	  for (i = 0; i < len; i++)
	    {
	      uchar c = streamer_read_uchar (ib);
	      (*fn->eh->ehspec_data.other)[i] = c;
	    }
	}
    }

  /* Reconstruct the EH region tree by fixing up the peer/children
     pointers.  */
  fixup_eh_region_pointers (fn, root_region);

  tag = streamer_read_record_start (ib);
  lto_tag_check_range (tag, LTO_null, LTO_null);
}


/* Make a new basic block with index INDEX in function FN.  */

static basic_block
make_new_block (struct function *fn, unsigned int index)
{
  basic_block bb = alloc_block ();
  bb->index = index;
  SET_BASIC_BLOCK_FOR_FUNCTION (fn, index, bb);
  n_basic_blocks_for_function (fn)++;
  return bb;
}


/* Read the CFG for function FN from input block IB.  */

static void
input_cfg (struct lto_input_block *ib, struct function *fn,
	   int count_materialization_scale)
{
  unsigned int bb_count;
  basic_block p_bb;
  unsigned int i;
  int index;

  init_empty_tree_cfg_for_function (fn);
  init_ssa_operands (fn);

  profile_status_for_function (fn) = streamer_read_enum (ib, profile_status_d,
							 PROFILE_LAST);

  bb_count = streamer_read_uhwi (ib);

  last_basic_block_for_function (fn) = bb_count;
  if (bb_count > basic_block_info_for_function (fn)->length ())
    vec_safe_grow_cleared (basic_block_info_for_function (fn), bb_count);

  if (bb_count > label_to_block_map_for_function (fn)->length ())
    vec_safe_grow_cleared (label_to_block_map_for_function (fn), bb_count);

  index = streamer_read_hwi (ib);
  while (index != -1)
    {
      basic_block bb = BASIC_BLOCK_FOR_FUNCTION (fn, index);
      unsigned int edge_count;

      if (bb == NULL)
	bb = make_new_block (fn, index);

      edge_count = streamer_read_uhwi (ib);

      /* Connect up the CFG.  */
      for (i = 0; i < edge_count; i++)
	{
	  unsigned int dest_index;
	  unsigned int edge_flags;
	  basic_block dest;
	  int probability;
	  gcov_type count;
	  edge e;

	  dest_index = streamer_read_uhwi (ib);
	  probability = (int) streamer_read_hwi (ib);
	  count = apply_scale ((gcov_type) streamer_read_gcov_count (ib),
                               count_materialization_scale);
	  edge_flags = streamer_read_uhwi (ib);

	  dest = BASIC_BLOCK_FOR_FUNCTION (fn, dest_index);

	  if (dest == NULL)
	    dest = make_new_block (fn, dest_index);

	  e = make_edge (bb, dest, edge_flags);
	  e->probability = probability;
	  e->count = count;
	}

      index = streamer_read_hwi (ib);
    }

  p_bb = ENTRY_BLOCK_PTR_FOR_FUNCTION(fn);
  index = streamer_read_hwi (ib);
  while (index != -1)
    {
      basic_block bb = BASIC_BLOCK_FOR_FUNCTION (fn, index);
      bb->prev_bb = p_bb;
      p_bb->next_bb = bb;
      p_bb = bb;
      index = streamer_read_hwi (ib);
    }

  /* ???  The cfgloop interface is tied to cfun.  */
  gcc_assert (cfun == fn);

  /* Input the loop tree.  */
  unsigned n_loops = streamer_read_uhwi (ib);
  if (n_loops == 0)
    return;

  struct loops *loops = ggc_alloc_cleared_loops ();
  init_loops_structure (fn, loops, n_loops);
  set_loops_for_fn (fn, loops);

  /* Input each loop and associate it with its loop header so
     flow_loops_find can rebuild the loop tree.  */
  for (unsigned i = 1; i < n_loops; ++i)
    {
      int header_index = streamer_read_hwi (ib);
      if (header_index == -1)
	{
	  loops->larray->quick_push (NULL);
	  continue;
	}

      struct loop *loop = alloc_loop ();
      loop->header = BASIC_BLOCK_FOR_FUNCTION (fn, header_index);
      loop->header->loop_father = loop;

      /* Read everything copy_loop_info copies.  */
      loop->estimate_state = streamer_read_enum (ib, loop_estimation, EST_LAST);
      loop->any_upper_bound = streamer_read_hwi (ib);
      if (loop->any_upper_bound)
	{
	  loop->nb_iterations_upper_bound.low = streamer_read_uhwi (ib);
	  loop->nb_iterations_upper_bound.high = streamer_read_hwi (ib);
	}
      loop->any_estimate = streamer_read_hwi (ib);
      if (loop->any_estimate)
	{
	  loop->nb_iterations_estimate.low = streamer_read_uhwi (ib);
	  loop->nb_iterations_estimate.high = streamer_read_hwi (ib);
	}

      place_new_loop (fn, loop);

      /* flow_loops_find doesn't like loops not in the tree, hook them
         all as siblings of the tree root temporarily.  */
      flow_loop_tree_node_add (loops->tree_root, loop);
    }

  /* Rebuild the loop tree.  */
  flow_loops_find (loops);
}


/* Read the SSA names array for function FN from DATA_IN using input
   block IB.  */

static void
input_ssa_names (struct lto_input_block *ib, struct data_in *data_in,
		 struct function *fn)
{
  unsigned int i, size;

  size = streamer_read_uhwi (ib);
  init_ssanames (fn, size);

  i = streamer_read_uhwi (ib);
  while (i)
    {
      tree ssa_name, name;
      bool is_default_def;

      /* Skip over the elements that had been freed.  */
      while (SSANAMES (fn)->length () < i)
	SSANAMES (fn)->quick_push (NULL_TREE);

      is_default_def = (streamer_read_uchar (ib) != 0);
      name = stream_read_tree (ib, data_in);
      ssa_name = make_ssa_name_fn (fn, name, gimple_build_nop ());

      if (is_default_def)
	set_ssa_default_def (cfun, SSA_NAME_VAR (ssa_name), ssa_name);

      i = streamer_read_uhwi (ib);
    }
}


/* Go through all NODE edges and fixup call_stmt pointers
   so they point to STMTS.  */

static void
fixup_call_stmt_edges_1 (struct cgraph_node *node, gimple *stmts,
			 struct function *fn)
{
  struct cgraph_edge *cedge;
  struct ipa_ref *ref;
  unsigned int i;

  for (cedge = node->callees; cedge; cedge = cedge->next_callee)
    {
      if (gimple_stmt_max_uid (fn) < cedge->lto_stmt_uid)
        fatal_error ("Cgraph edge statement index out of range");
      cedge->call_stmt = stmts[cedge->lto_stmt_uid - 1];
      if (!cedge->call_stmt)
        fatal_error ("Cgraph edge statement index not found");
    }
  for (cedge = node->indirect_calls; cedge; cedge = cedge->next_callee)
    {
      if (gimple_stmt_max_uid (fn) < cedge->lto_stmt_uid)
        fatal_error ("Cgraph edge statement index out of range");
      cedge->call_stmt = stmts[cedge->lto_stmt_uid - 1];
      if (!cedge->call_stmt)
        fatal_error ("Cgraph edge statement index not found");
    }
  for (i = 0;
       ipa_ref_list_reference_iterate (&node->symbol.ref_list, i, ref);
       i++)
    if (ref->lto_stmt_uid)
      {
	if (gimple_stmt_max_uid (fn) < ref->lto_stmt_uid)
	  fatal_error ("Reference statement index out of range");
	ref->stmt = stmts[ref->lto_stmt_uid - 1];
	if (!ref->stmt)
	  fatal_error ("Reference statement index not found");
      }
}


/* Fixup call_stmt pointers in NODE and all clones.  */

static void
fixup_call_stmt_edges (struct cgraph_node *orig, gimple *stmts)
{
  struct cgraph_node *node;
  struct function *fn;

  while (orig->clone_of)
    orig = orig->clone_of;
  fn = DECL_STRUCT_FUNCTION (orig->symbol.decl);

  fixup_call_stmt_edges_1 (orig, stmts, fn);
  if (orig->clones)
    for (node = orig->clones; node != orig;)
      {
	fixup_call_stmt_edges_1 (node, stmts, fn);
	if (node->clones)
	  node = node->clones;
	else if (node->next_sibling_clone)
	  node = node->next_sibling_clone;
	else
	  {
	    while (node != orig && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != orig)
	      node = node->next_sibling_clone;
	  }
      }
}


/* Input the base body of struct function FN from DATA_IN
   using input block IB.  */

static void
input_struct_function_base (struct function *fn, struct data_in *data_in,
                            struct lto_input_block *ib)
{
  struct bitpack_d bp;
  int len;

  /* Read the static chain and non-local goto save area.  */
  fn->static_chain_decl = stream_read_tree (ib, data_in);
  fn->nonlocal_goto_save_area = stream_read_tree (ib, data_in);

  /* Read all the local symbols.  */
  len = streamer_read_hwi (ib);
  if (len > 0)
    {
      int i;
      vec_safe_grow_cleared (fn->local_decls, len);
      for (i = 0; i < len; i++)
	{
	  tree t = stream_read_tree (ib, data_in);
	  (*fn->local_decls)[i] = t;
	}
    }

  /* Input the current IL state of the function.  */
  fn->curr_properties = streamer_read_uhwi (ib);

  /* Read all the attributes for FN.  */
  bp = streamer_read_bitpack (ib);
  fn->is_thunk = bp_unpack_value (&bp, 1);
  fn->has_local_explicit_reg_vars = bp_unpack_value (&bp, 1);
  fn->returns_pcc_struct = bp_unpack_value (&bp, 1);
  fn->returns_struct = bp_unpack_value (&bp, 1);
  fn->can_throw_non_call_exceptions = bp_unpack_value (&bp, 1);
  fn->can_delete_dead_exceptions = bp_unpack_value (&bp, 1);
  fn->always_inline_functions_inlined = bp_unpack_value (&bp, 1);
  fn->after_inlining = bp_unpack_value (&bp, 1);
  fn->stdarg = bp_unpack_value (&bp, 1);
  fn->has_nonlocal_label = bp_unpack_value (&bp, 1);
  fn->calls_alloca = bp_unpack_value (&bp, 1);
  fn->calls_setjmp = bp_unpack_value (&bp, 1);
  fn->va_list_fpr_size = bp_unpack_value (&bp, 8);
  fn->va_list_gpr_size = bp_unpack_value (&bp, 8);

  /* Input the function start and end loci.  */
  fn->function_start_locus = stream_input_location (&bp, data_in);
  fn->function_end_locus = stream_input_location (&bp, data_in);
}


/* Read the body of function FN_DECL from DATA_IN using input block IB.  */

static void
input_function (tree fn_decl, struct data_in *data_in,
		struct lto_input_block *ib, struct lto_input_block *ib_cfg)
{
  struct function *fn;
  enum LTO_tags tag;
  gimple *stmts;
  basic_block bb;
  struct cgraph_node *node;

  tag = streamer_read_record_start (ib);
  lto_tag_check (tag, LTO_function);

  /* Read decls for parameters and args.  */
  DECL_RESULT (fn_decl) = stream_read_tree (ib, data_in);
  DECL_ARGUMENTS (fn_decl) = streamer_read_chain (ib, data_in);

  /* Read the tree of lexical scopes for the function.  */
  DECL_INITIAL (fn_decl) = stream_read_tree (ib, data_in);

  if (!streamer_read_uhwi (ib))
    return;

  push_struct_function (fn_decl);
  fn = DECL_STRUCT_FUNCTION (fn_decl);
  init_tree_ssa (fn);
  /* We input IL in SSA form.  */
  cfun->gimple_df->in_ssa_p = true;

  gimple_register_cfg_hooks ();

  node = cgraph_get_create_node (fn_decl);
  input_struct_function_base (fn, data_in, ib);
  input_cfg (ib_cfg, fn, node->count_materialization_scale);

  /* Read all the SSA names.  */
  input_ssa_names (ib, data_in, fn);

  /* Read the exception handling regions in the function.  */
  input_eh_regions (ib, data_in, fn);

  gcc_assert (DECL_INITIAL (fn_decl));
  DECL_SAVED_TREE (fn_decl) = NULL_TREE;

  /* Read all the basic blocks.  */
  tag = streamer_read_record_start (ib);
  while (tag)
    {
      input_bb (ib, tag, data_in, fn,
		node->count_materialization_scale);
      tag = streamer_read_record_start (ib);
    }

  /* Fix up the call statements that are mentioned in the callgraph
     edges.  */
  set_gimple_stmt_max_uid (cfun, 0);
  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
    }
  stmts = (gimple *) xcalloc (gimple_stmt_max_uid (fn), sizeof (gimple));
  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator bsi = gsi_start_phis (bb);
      while (!gsi_end_p (bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  gsi_next (&bsi);
	  stmts[gimple_uid (stmt)] = stmt;
	}
      bsi = gsi_start_bb (bb);
      while (!gsi_end_p (bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  /* If we're recompiling LTO objects with debug stmts but
	     we're not supposed to have debug stmts, remove them now.
	     We can't remove them earlier because this would cause uid
	     mismatches in fixups, but we can do it at this point, as
	     long as debug stmts don't require fixups.  */
	  if (!MAY_HAVE_DEBUG_STMTS && is_gimple_debug (stmt))
	    {
	      gimple_stmt_iterator gsi = bsi;
	      gsi_next (&bsi);
	      gsi_remove (&gsi, true);
	    }
	  else
	    {
	      gsi_next (&bsi);
	      stmts[gimple_uid (stmt)] = stmt;
	    }
	}
    }

  /* Set the gimple body to the statement sequence in the entry
     basic block.  FIXME lto, this is fairly hacky.  The existence
     of a gimple body is used by the cgraph routines, but we should
     really use the presence of the CFG.  */
  {
    edge_iterator ei = ei_start (ENTRY_BLOCK_PTR->succs);
    gimple_set_body (fn_decl, bb_seq (ei_edge (ei)->dest));
  }

  fixup_call_stmt_edges (node, stmts);
  execute_all_ipa_stmt_fixups (node, stmts);

  update_ssa (TODO_update_ssa_only_virtuals);
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  free (stmts);
}


/* Read the body from DATA for function FN_DECL and fill it in.
   FILE_DATA are the global decls and types.  SECTION_TYPE is either
   LTO_section_function_body or LTO_section_static_initializer.  If
   section type is LTO_section_function_body, FN must be the decl for
   that function.  */

static void
lto_read_body (struct lto_file_decl_data *file_data, tree fn_decl,
	       const char *data, enum lto_section_type section_type)
{
  const struct lto_function_header *header;
  struct data_in *data_in;
  int cfg_offset;
  int main_offset;
  int string_offset;
  struct lto_input_block ib_cfg;
  struct lto_input_block ib_main;

  header = (const struct lto_function_header *) data;
  cfg_offset = sizeof (struct lto_function_header);
  main_offset = cfg_offset + header->cfg_size;
  string_offset = main_offset + header->main_size;

  LTO_INIT_INPUT_BLOCK (ib_cfg,
		        data + cfg_offset,
			0,
			header->cfg_size);

  LTO_INIT_INPUT_BLOCK (ib_main,
			data + main_offset,
			0,
			header->main_size);

  data_in = lto_data_in_create (file_data, data + string_offset,
			      header->string_size, vNULL);

  /* Make sure the file was generated by the exact same compiler.  */
  lto_check_version (header->lto_header.major_version,
		     header->lto_header.minor_version);

  if (section_type == LTO_section_function_body)
    {
      struct lto_in_decl_state *decl_state;
      struct cgraph_node *node = cgraph_get_node (fn_decl);
      unsigned from;

      gcc_checking_assert (node);

      /* Use the function's decl state. */
      decl_state = lto_get_function_in_decl_state (file_data, fn_decl);
      gcc_assert (decl_state);
      file_data->current_decl_state = decl_state;


      /* Set up the struct function.  */
      from = data_in->reader_cache->nodes.length ();
      input_function (fn_decl, data_in, &ib_main, &ib_cfg);
      /* And fixup types we streamed locally.  */
	{
	  struct streamer_tree_cache_d *cache = data_in->reader_cache;
	  unsigned len = cache->nodes.length ();
	  unsigned i;
	  for (i = len; i-- > from;)
	    {
	      tree t = streamer_tree_cache_get_tree (cache, i);
	      if (t == NULL_TREE)
		continue;

	      if (TYPE_P (t))
		{
		  gcc_assert (TYPE_CANONICAL (t) == NULL_TREE);
		  TYPE_CANONICAL (t) = TYPE_MAIN_VARIANT (t);
		  if (TYPE_MAIN_VARIANT (t) != t)
		    {
		      gcc_assert (TYPE_NEXT_VARIANT (t) == NULL_TREE);
		      TYPE_NEXT_VARIANT (t)
			= TYPE_NEXT_VARIANT (TYPE_MAIN_VARIANT (t));
		      TYPE_NEXT_VARIANT (TYPE_MAIN_VARIANT (t)) = t;
		    }
		}
	    }
	}

      /* Restore decl state */
      file_data->current_decl_state = file_data->global_decl_state;

      pop_cfun ();
    }

  lto_data_in_delete (data_in);
}


/* Read the body of FN_DECL using DATA.  FILE_DATA holds the global
   decls and types.  */

void
lto_input_function_body (struct lto_file_decl_data *file_data,
			 tree fn_decl, const char *data)
{
  lto_read_body (file_data, fn_decl, data, LTO_section_function_body);
}


/* Read the physical representation of a tree node EXPR from
   input block IB using the per-file context in DATA_IN.  */

static void
lto_read_tree_1 (struct lto_input_block *ib, struct data_in *data_in, tree expr)
{
  /* Read all the bitfield values in EXPR.  Note that for LTO, we
     only write language-independent bitfields, so no more unpacking is
     needed.  */
  streamer_read_tree_bitfields (ib, data_in, expr);

  /* Read all the pointer fields in EXPR.  */
  streamer_read_tree_body (ib, data_in, expr);

  /* Read any LTO-specific data not read by the tree streamer.  */
  if (DECL_P (expr)
      && TREE_CODE (expr) != FUNCTION_DECL
      && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
    DECL_INITIAL (expr) = stream_read_tree (ib, data_in);

  /* We should never try to instantiate an MD or NORMAL builtin here.  */
  if (TREE_CODE (expr) == FUNCTION_DECL)
    gcc_assert (!streamer_handle_as_builtin_p (expr));

#ifdef LTO_STREAMER_DEBUG
  /* Remove the mapping to RESULT's original address set by
     streamer_alloc_tree.  */
  lto_orig_address_remove (expr);
#endif
}

/* Read the physical representation of a tree node with tag TAG from
   input block IB using the per-file context in DATA_IN.  */

static tree
lto_read_tree (struct lto_input_block *ib, struct data_in *data_in,
	       enum LTO_tags tag, hashval_t hash)
{
  /* Instantiate a new tree node.  */
  tree result = streamer_alloc_tree (ib, data_in, tag);

  /* Enter RESULT in the reader cache.  This will make RESULT
     available so that circular references in the rest of the tree
     structure can be resolved in subsequent calls to stream_read_tree.  */
  streamer_tree_cache_append (data_in->reader_cache, result, hash);

  lto_read_tree_1 (ib, data_in, result);

  /* end_marker = */ streamer_read_uchar (ib);

  return result;
}


/* Populate the reader cache with trees materialized from the SCC
   following in the IB, DATA_IN stream.  */

hashval_t
lto_input_scc (struct lto_input_block *ib, struct data_in *data_in,
	       unsigned *len, unsigned *entry_len)
{
  /* A blob of unnamed tree nodes, fill the cache from it and
     recurse.  */
  unsigned size = streamer_read_uhwi (ib);
  hashval_t scc_hash = streamer_read_uhwi (ib);
  unsigned scc_entry_len = 1;

  if (size == 1)
    {
      enum LTO_tags tag = streamer_read_record_start (ib);
      lto_input_tree_1 (ib, data_in, tag, scc_hash);
    }
  else
    {
      unsigned int first = data_in->reader_cache->nodes.length ();
      tree result;

      scc_entry_len = streamer_read_uhwi (ib);

      /* Materialize size trees by reading their headers.  */
      for (unsigned i = 0; i < size; ++i)
	{
	  enum LTO_tags tag = streamer_read_record_start (ib);
	  if (tag == LTO_null
	      || (tag >= LTO_field_decl_ref && tag <= LTO_global_decl_ref)
	      || tag == LTO_tree_pickle_reference
	      || tag == LTO_builtin_decl
	      || tag == LTO_integer_cst
	      || tag == LTO_tree_scc)
	    gcc_unreachable ();

	  result = streamer_alloc_tree (ib, data_in, tag);
	  streamer_tree_cache_append (data_in->reader_cache, result, 0);
	}

      /* Read the tree bitpacks and references.  */
      for (unsigned i = 0; i < size; ++i)
	{
	  result = streamer_tree_cache_get_tree (data_in->reader_cache,
						 first + i);
	  lto_read_tree_1 (ib, data_in, result);
	  /* end_marker = */ streamer_read_uchar (ib);
	}
    }

  *len = size;
  *entry_len = scc_entry_len;
  return scc_hash;
}


/* Read a tree from input block IB using the per-file context in
   DATA_IN.  This context is used, for example, to resolve references
   to previously read nodes.  */

tree
lto_input_tree_1 (struct lto_input_block *ib, struct data_in *data_in,
		  enum LTO_tags tag, hashval_t hash)
{
  tree result;

  gcc_assert ((unsigned) tag < (unsigned) LTO_NUM_TAGS);

  if (tag == LTO_null)
    result = NULL_TREE;
  else if (tag >= LTO_field_decl_ref && tag <= LTO_global_decl_ref)
    {
      /* If TAG is a reference to an indexable tree, the next value
	 in IB is the index into the table where we expect to find
	 that tree.  */
      result = lto_input_tree_ref (ib, data_in, cfun, tag);
    }
  else if (tag == LTO_tree_pickle_reference)
    {
      /* If TAG is a reference to a previously read tree, look it up in
	 the reader cache.  */
      result = streamer_get_pickled_tree (ib, data_in);
    }
  else if (tag == LTO_builtin_decl)
    {
      /* If we are going to read a built-in function, all we need is
	 the code and class.  */
      result = streamer_get_builtin_tree (ib, data_in);
    }
  else if (tag == LTO_integer_cst)
    {
      /* For shared integer constants in singletons we can use the existing
         tree integer constant merging code.  */
      tree type = stream_read_tree (ib, data_in);
      unsigned HOST_WIDE_INT low = streamer_read_uhwi (ib);
      HOST_WIDE_INT high = streamer_read_hwi (ib);
      result = build_int_cst_wide (type, low, high);
      streamer_tree_cache_append (data_in->reader_cache, result, hash);
    }
  else if (tag == LTO_tree_scc)
    {
      unsigned len, entry_len;

      /* Input and skip the SCC.  */
      lto_input_scc (ib, data_in, &len, &entry_len);

      /* Recurse.  */
      return lto_input_tree (ib, data_in);
    }
  else
    {
      /* Otherwise, materialize a new node from IB.  */
      result = lto_read_tree (ib, data_in, tag, hash);
    }

  return result;
}

tree
lto_input_tree (struct lto_input_block *ib, struct data_in *data_in)
{
  return lto_input_tree_1 (ib, data_in, streamer_read_record_start (ib), 0);
}


/* Input toplevel asms.  */

void
lto_input_toplevel_asms (struct lto_file_decl_data *file_data, int order_base)
{
  size_t len;
  const char *data = lto_get_section_data (file_data, LTO_section_asm,
					   NULL, &len);
  const struct lto_asm_header *header = (const struct lto_asm_header *) data;
  int string_offset;
  struct data_in *data_in;
  struct lto_input_block ib;
  tree str;

  if (! data)
    return;

  string_offset = sizeof (*header) + header->main_size;

  LTO_INIT_INPUT_BLOCK (ib,
			data + sizeof (*header),
			0,
			header->main_size);

  data_in = lto_data_in_create (file_data, data + string_offset,
			      header->string_size, vNULL);

  /* Make sure the file was generated by the exact same compiler.  */
  lto_check_version (header->lto_header.major_version,
		     header->lto_header.minor_version);

  while ((str = streamer_read_string_cst (data_in, &ib)))
    {
      struct asm_node *node = add_asm_node (str);
      node->order = streamer_read_hwi (&ib) + order_base;
      if (node->order >= symtab_order)
	symtab_order = node->order + 1;
    }

  lto_data_in_delete (data_in);

  lto_free_section_data (file_data, LTO_section_asm, NULL, data, len);
}


/* Initialization for the LTO reader.  */

void
lto_reader_init (void)
{
  lto_streamer_init ();
  file_name_hash_table.create (37);
}


/* Create a new data_in object for FILE_DATA. STRINGS is the string
   table to use with LEN strings.  RESOLUTIONS is the vector of linker
   resolutions (NULL if not using a linker plugin).  */

struct data_in *
lto_data_in_create (struct lto_file_decl_data *file_data, const char *strings,
		    unsigned len,
		    vec<ld_plugin_symbol_resolution_t> resolutions)
{
  struct data_in *data_in = XCNEW (struct data_in);
  data_in->file_data = file_data;
  data_in->strings = strings;
  data_in->strings_len = len;
  data_in->globals_resolution = resolutions;
  data_in->reader_cache = streamer_tree_cache_create (false, false);

  return data_in;
}


/* Remove DATA_IN.  */

void
lto_data_in_delete (struct data_in *data_in)
{
  data_in->globals_resolution.release ();
  streamer_tree_cache_delete (data_in->reader_cache);
  free (data_in->labels);
  free (data_in);
}
