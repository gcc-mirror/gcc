/* Read the GIMPLE representation from a file stream.

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
#include "toplev.h"
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "varray.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "libfuncs.h"
#include "except.h"
#include "debug.h"
#include "vec.h"
#include "timevar.h"
#include "output.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "tree-pass.h"

/* Data structure used to hash file names in the source_location field.  */
struct string_slot
{
  const char *s;
  unsigned int slot_num;
};

/* The table to hold the file names.  */
static htab_t file_name_hash_table;


/* Check that tag ACTUAL has one of the given values.  NUM_TAGS is the
   number of valid tag values to check.  */

static void
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


/* Check that tag ACTUAL is in the range [TAG1, TAG2].  */

static void
lto_tag_check_range (enum LTO_tags actual, enum LTO_tags tag1,
		     enum LTO_tags tag2)
{
  if (actual < tag1 || actual > tag2)
    internal_error ("bytecode stream: tag %s is not in the expected range "
		    "[%s, %s]",
		    lto_tag_name (actual),
		    lto_tag_name (tag1),
		    lto_tag_name (tag2));
}


/* Check that tag ACTUAL == EXPECTED.  */

static void
lto_tag_check (enum LTO_tags actual, enum LTO_tags expected)
{
  if (actual != expected)
    internal_error ("bytecode stream: expected tag %s instead of %s",
		    lto_tag_name (expected), lto_tag_name (actual));
}


/* Return a hash code for P.  */

static hashval_t
hash_string_slot_node (const void *p)
{
  const struct string_slot *ds = (const struct string_slot *) p;
  return (hashval_t) htab_hash_string (ds->s);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_string_slot_node (const void *p1, const void *p2)
{
  const struct string_slot *ds1 = (const struct string_slot *) p1;
  const struct string_slot *ds2 = (const struct string_slot *) p2;
  return strcmp (ds1->s, ds2->s) == 0;
}


/* Read a string from the string table in DATA_IN using input block
   IB.  Write the length to RLEN.  */

static const char *
input_string_internal (struct data_in *data_in, struct lto_input_block *ib,
		       unsigned int *rlen)
{
  struct lto_input_block str_tab;
  unsigned int len;
  unsigned int loc;
  const char *result;

  loc = lto_input_uleb128 (ib);
  LTO_INIT_INPUT_BLOCK (str_tab, data_in->strings, loc, data_in->strings_len);
  len = lto_input_uleb128 (&str_tab);
  *rlen = len;

  if (str_tab.p + len > data_in->strings_len)
    internal_error ("bytecode stream: string too long for the string table");

  result = (const char *)(data_in->strings + str_tab.p);

  return result;
}


/* Read a STRING_CST from the string table in DATA_IN using input
   block IB.  */

static tree
input_string_cst (struct data_in *data_in, struct lto_input_block *ib)
{
  unsigned int len;
  const char * ptr;
  unsigned int is_null;

  is_null = lto_input_uleb128 (ib);
  if (is_null)
    return NULL;

  ptr = input_string_internal (data_in, ib, &len);
  return build_string (len, ptr);
}


/* Read an IDENTIFIER from the string table in DATA_IN using input
   block IB.  */

static tree
input_identifier (struct data_in *data_in, struct lto_input_block *ib)
{
  unsigned int len;
  const char *ptr;
  unsigned int is_null;

  is_null = lto_input_uleb128 (ib);
  if (is_null)
    return NULL;

  ptr = input_string_internal (data_in, ib, &len);
  return get_identifier_with_length (ptr, len);
}

/* Read a NULL terminated string from the string table in DATA_IN.  */

static const char *
input_string (struct data_in *data_in, struct lto_input_block *ib)
{
  unsigned int len;
  const char *ptr;
  unsigned int is_null;

  is_null = lto_input_uleb128 (ib);
  if (is_null)
    return NULL;

  ptr = input_string_internal (data_in, ib, &len);
  if (ptr[len - 1] != '\0')
    internal_error ("bytecode stream: found non-null terminated string");

  return ptr;
}


/* Return the next tag in the input block IB.  */

static enum LTO_tags
input_record_start (struct lto_input_block *ib)
{
  enum LTO_tags tag = (enum LTO_tags) lto_input_uleb128 (ib);
  return tag;
}


/* Lookup STRING in file_name_hash_table.  If found, return the existing
   string, otherwise insert STRING as the canonical version.  */

static const char *
canon_file_name (const char *string)
{
  void **slot;
  struct string_slot s_slot;
  s_slot.s = string;

  slot = htab_find_slot (file_name_hash_table, &s_slot, INSERT);
  if (*slot == NULL)
    {
      size_t len;
      char *saved_string;
      struct string_slot *new_slot;

      len = strlen (string);
      saved_string = (char *) xmalloc (len + 1);
      new_slot = XCNEW (struct string_slot);
      strcpy (saved_string, string);
      new_slot->s = saved_string;
      *slot = new_slot;
      return saved_string;
    }
  else
    {
      struct string_slot *old_slot = (struct string_slot *) *slot;
      return old_slot->s;
    }
}


/* Clear the line info stored in DATA_IN.  */

static void
clear_line_info (struct data_in *data_in)
{
  if (data_in->current_file)
    linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  data_in->current_file = NULL;
  data_in->current_line = 0;
  data_in->current_col = 0;
}


/* Read a location from input block IB.  */

static location_t
lto_input_location (struct lto_input_block *ib, struct data_in *data_in)
{
  expanded_location xloc;

  xloc.file = input_string (data_in, ib);
  if (xloc.file == NULL)
    return UNKNOWN_LOCATION;

  xloc.file = canon_file_name (xloc.file);
  xloc.line = lto_input_sleb128 (ib);
  xloc.column = lto_input_sleb128 (ib);
  xloc.sysp = lto_input_sleb128 (ib);

  if (data_in->current_file != xloc.file)
    {
      if (data_in->current_file)
	linemap_add (line_table, LC_LEAVE, false, NULL, 0);

      linemap_add (line_table, LC_ENTER, xloc.sysp, xloc.file, xloc.line);
    }
  else if (data_in->current_line != xloc.line)
    linemap_line_start (line_table, xloc.line, xloc.column);

  data_in->current_file = xloc.file;
  data_in->current_line = xloc.line;
  data_in->current_col = xloc.column;

  return linemap_position_for_column (line_table, xloc.column);
}


/* Read a reference to a tree node from DATA_IN using input block IB.
   TAG is the expected node that should be found in IB, if TAG belongs
   to one of the indexable trees, expect to read a reference index to
   be looked up in one of the symbol tables, otherwise read the pysical
   representation of the tree using lto_input_tree.  FN is the
   function scope for the read tree.  */

static tree
lto_input_tree_ref (struct lto_input_block *ib, struct data_in *data_in,
		    struct function *fn, enum LTO_tags tag)
{
  unsigned HOST_WIDE_INT ix_u;
  tree result = NULL_TREE;

  lto_tag_check_range (tag, LTO_field_decl_ref, LTO_global_decl_ref);

  switch (tag)
    {
    case LTO_type_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_type (data_in->file_data, ix_u);
      break;

    case LTO_ssa_name_ref:
      ix_u = lto_input_uleb128 (ib);
      result = VEC_index (tree, SSANAMES (fn), ix_u);
      break;

    case LTO_field_decl_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_field_decl (data_in->file_data, ix_u);
      break;

    case LTO_function_decl_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_fn_decl (data_in->file_data, ix_u);
      break;

    case LTO_type_decl_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_type_decl (data_in->file_data, ix_u);
      break;

    case LTO_namespace_decl_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_namespace_decl (data_in->file_data, ix_u);
      break;

    case LTO_global_decl_ref:
    case LTO_result_decl_ref:
    case LTO_const_decl_ref:
    case LTO_imported_decl_ref:
    case LTO_label_decl_ref:
      ix_u = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_var_decl (data_in->file_data, ix_u);
      if (TREE_CODE (result) == VAR_DECL)
	varpool_mark_needed_node (varpool_node (result));
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
  tag = input_record_start (ib);
  while (tag)
    {
      tree list;
      eh_catch n;

      lto_tag_check_range (tag, LTO_eh_catch, LTO_eh_catch);

      /* Read the catch node.  */
      n = GGC_CNEW (struct eh_catch_d);
      n->type_list = lto_input_tree (ib, data_in);
      n->filter_list = lto_input_tree (ib, data_in);
      n->label = lto_input_tree (ib, data_in);

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

      tag = input_record_start (ib);
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
  tag = input_record_start (ib);
  if (tag == LTO_null)
    return NULL;

  r = GGC_CNEW (struct eh_region_d);
  r->index = lto_input_sleb128 (ib);

  gcc_assert (r->index == ix);

  /* Read all the region pointers as region numbers.  We'll fix up
     the pointers once the whole array has been read.  */
  r->outer = (eh_region) (intptr_t) lto_input_sleb128 (ib);
  r->inner = (eh_region) (intptr_t) lto_input_sleb128 (ib);
  r->next_peer = (eh_region) (intptr_t) lto_input_sleb128 (ib);

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
	  r->u.allowed.type_list = lto_input_tree (ib, data_in);
	  r->u.allowed.label = lto_input_tree (ib, data_in);
	  r->u.allowed.filter = lto_input_uleb128 (ib);

	  for (l = r->u.allowed.type_list; l ; l = TREE_CHAIN (l))
	    add_type_for_runtime (TREE_VALUE (l));
	}
	break;

      case LTO_ert_must_not_throw:
	r->type = ERT_MUST_NOT_THROW;
	r->u.must_not_throw.failure_decl = lto_input_tree (ib, data_in);
	r->u.must_not_throw.failure_loc = lto_input_location (ib, data_in);
	break;

      default:
	gcc_unreachable ();
    }

  r->landing_pads = (eh_landing_pad) (intptr_t) lto_input_sleb128 (ib);

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
  tag = input_record_start (ib);
  if (tag == LTO_null)
    return NULL;

  lto_tag_check_range (tag, LTO_eh_landing_pad, LTO_eh_landing_pad);

  lp = GGC_CNEW (struct eh_landing_pad_d);
  lp->index = lto_input_sleb128 (ib);
  gcc_assert (lp->index == ix);
  lp->next_lp = (eh_landing_pad) (intptr_t) lto_input_sleb128 (ib);
  lp->region = (eh_region) (intptr_t) lto_input_sleb128 (ib);
  lp->post_landing_pad = lto_input_tree (ib, data_in);

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
  VEC(eh_region,gc) *eh_array = fn->eh->region_array;
  VEC(eh_landing_pad,gc) *lp_array = fn->eh->lp_array;
  eh_region r;
  eh_landing_pad lp;

  gcc_assert (eh_array && lp_array);

  gcc_assert (root_region >= 0);
  fn->eh->region_tree = VEC_index (eh_region, eh_array, root_region);

#define FIXUP_EH_REGION(r) (r) = VEC_index (eh_region, eh_array, \
					    (HOST_WIDE_INT) (intptr_t) (r))
#define FIXUP_EH_LP(p) (p) = VEC_index (eh_landing_pad, lp_array, \
					(HOST_WIDE_INT) (intptr_t) (p))

  /* Convert all the index numbers stored in pointer fields into
     pointers to the corresponding slots in the EH region array.  */
  for (i = 0; VEC_iterate (eh_region, eh_array, i, r); i++)
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
  for (i = 0; VEC_iterate (eh_landing_pad, lp_array, i, lp); i++)
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

static void
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

  /* Initialize dwarf2 tables.  Since dwarf2out_do_frame() returns
     true only when exceptions are enabled, this initialization is
     never done during lang_dependent_init.  */
#if defined DWARF2_DEBUGGING_INFO || defined DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_init ();
#endif

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

  tag = input_record_start (ib);
  if (tag == LTO_null)
    return;

  lto_tag_check_range (tag, LTO_eh_table, LTO_eh_table);

  /* If the file contains EH regions, then it was compiled with
     -fexceptions.  In that case, initialize the backend EH
     machinery.  */
  lto_init_eh ();

  gcc_assert (fn->eh);

  root_region = lto_input_sleb128 (ib);
  gcc_assert (root_region == (int) root_region);

  /* Read the EH region array.  */
  len = lto_input_sleb128 (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      VEC_safe_grow (eh_region, gc, fn->eh->region_array, len);
      for (i = 0; i < len; i++)
	{
	  eh_region r = input_eh_region (ib, data_in, i);
	  VEC_replace (eh_region, fn->eh->region_array, i, r);
	}
    }

  /* Read the landing pads.  */
  len = lto_input_sleb128 (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      VEC_safe_grow (eh_landing_pad, gc, fn->eh->lp_array, len);
      for (i = 0; i < len; i++)
	{
	  eh_landing_pad lp = input_eh_lp (ib, data_in, i);
	  VEC_replace (eh_landing_pad, fn->eh->lp_array, i, lp);
	}
    }

  /* Read the runtime type data.  */
  len = lto_input_sleb128 (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      VEC_safe_grow (tree, gc, fn->eh->ttype_data, len);
      for (i = 0; i < len; i++)
	{
	  tree ttype = lto_input_tree (ib, data_in);
	  VEC_replace (tree, fn->eh->ttype_data, i, ttype);
	}
    }

  /* Read the table of action chains.  */
  len = lto_input_sleb128 (ib);
  gcc_assert (len == (int) len);
  if (len > 0)
    {
      if (targetm.arm_eabi_unwinder)
	{
	  VEC_safe_grow (tree, gc, fn->eh->ehspec_data.arm_eabi, len);
	  for (i = 0; i < len; i++)
	    {
	      tree t = lto_input_tree (ib, data_in);
	      VEC_replace (tree, fn->eh->ehspec_data.arm_eabi, i, t);
	    }
	}
      else
	{
	  VEC_safe_grow (uchar, gc, fn->eh->ehspec_data.other, len);
	  for (i = 0; i < len; i++)
	    {
	      uchar c = lto_input_1_unsigned (ib);
	      VEC_replace (uchar, fn->eh->ehspec_data.other, i, c);
	    }
	}
    }

  /* Reconstruct the EH region tree by fixing up the peer/children
     pointers.  */
  fixup_eh_region_pointers (fn, root_region);

  tag = input_record_start (ib);
  lto_tag_check_range (tag, LTO_null, LTO_null);
}


/* Make a new basic block with index INDEX in function FN.  */

static basic_block
make_new_block (struct function *fn, unsigned int index)
{
  basic_block bb = alloc_block ();
  bb->index = index;
  SET_BASIC_BLOCK_FOR_FUNCTION (fn, index, bb);
  bb->il.gimple = GGC_CNEW (struct gimple_bb_info);
  n_basic_blocks_for_function (fn)++;
  bb->flags = 0;
  set_bb_seq (bb, gimple_seq_alloc ());
  return bb;
}


/* Read the CFG for function FN from input block IB.  */

static void
input_cfg (struct lto_input_block *ib, struct function *fn)
{
  unsigned int bb_count;
  basic_block p_bb;
  unsigned int i;
  int index;

  init_empty_tree_cfg_for_function (fn);
  init_ssa_operands ();

  profile_status_for_function (fn) =
    (enum profile_status_d) lto_input_uleb128 (ib);

  bb_count = lto_input_uleb128 (ib);

  last_basic_block_for_function (fn) = bb_count;
  if (bb_count > VEC_length (basic_block, basic_block_info_for_function (fn)))
    VEC_safe_grow_cleared (basic_block, gc,
			   basic_block_info_for_function (fn), bb_count);

  if (bb_count > VEC_length (basic_block, label_to_block_map_for_function (fn)))
    VEC_safe_grow_cleared (basic_block, gc,
			   label_to_block_map_for_function (fn), bb_count);

  index = lto_input_sleb128 (ib);
  while (index != -1)
    {
      basic_block bb = BASIC_BLOCK_FOR_FUNCTION (fn, index);
      unsigned int edge_count;

      if (bb == NULL)
	bb = make_new_block (fn, index);

      edge_count = lto_input_uleb128 (ib);

      /* Connect up the CFG.  */
      for (i = 0; i < edge_count; i++)
	{
	  unsigned int dest_index;
	  unsigned int edge_flags;
	  basic_block dest;
	  int probability;
	  gcov_type count;
	  edge e;

	  dest_index = lto_input_uleb128 (ib);
	  probability = (int) lto_input_sleb128 (ib);
	  count = (gcov_type) lto_input_sleb128 (ib);
	  edge_flags = lto_input_uleb128 (ib);

	  dest = BASIC_BLOCK_FOR_FUNCTION (fn, dest_index);

	  if (dest == NULL)
	    dest = make_new_block (fn, dest_index);

	  e = make_edge (bb, dest, edge_flags);
	  e->probability = probability;
	  e->count = count;
	}

      index = lto_input_sleb128 (ib);
    }

  p_bb = ENTRY_BLOCK_PTR_FOR_FUNCTION(fn);
  index = lto_input_sleb128 (ib);
  while (index != -1)
    {
      basic_block bb = BASIC_BLOCK_FOR_FUNCTION (fn, index);
      bb->prev_bb = p_bb;
      p_bb->next_bb = bb;
      p_bb = bb;
      index = lto_input_sleb128 (ib);
    }
}


/* Read a PHI function for basic block BB in function FN.  DATA_IN is
   the file being read.  IB is the input block to use for reading.  */

static gimple
input_phi (struct lto_input_block *ib, basic_block bb, struct data_in *data_in,
	   struct function *fn)
{
  unsigned HOST_WIDE_INT ix;
  tree phi_result;
  int i, len;
  gimple result;

  ix = lto_input_uleb128 (ib);
  phi_result = VEC_index (tree, SSANAMES (fn), ix);
  len = EDGE_COUNT (bb->preds);
  result = create_phi_node (phi_result, bb);
  SSA_NAME_DEF_STMT (phi_result) = result;

  /* We have to go through a lookup process here because the preds in the
     reconstructed graph are generally in a different order than they
     were in the original program.  */
  for (i = 0; i < len; i++)
    {
      tree def = lto_input_tree (ib, data_in);
      int src_index = lto_input_uleb128 (ib);
      location_t arg_loc = lto_input_location (ib, data_in);
      basic_block sbb = BASIC_BLOCK_FOR_FUNCTION (fn, src_index);

      edge e = NULL;
      int j;

      for (j = 0; j < len; j++)
	if (EDGE_PRED (bb, j)->src == sbb)
	  {
	    e = EDGE_PRED (bb, j);
	    break;
	  }

      add_phi_arg (result, def, e, arg_loc);
    }

  return result;
}


/* Read the SSA names array for function FN from DATA_IN using input
   block IB.  */

static void
input_ssa_names (struct lto_input_block *ib, struct data_in *data_in,
		 struct function *fn)
{
  unsigned int i, size;

  size = lto_input_uleb128 (ib);
  init_ssanames (fn, size);

  i = lto_input_uleb128 (ib);
  while (i)
    {
      tree ssa_name, name;
      bool is_default_def;

      /* Skip over the elements that had been freed.  */
      while (VEC_length (tree, SSANAMES (fn)) < i)
	VEC_quick_push (tree, SSANAMES (fn), NULL_TREE);

      is_default_def = (lto_input_1_unsigned (ib) != 0);
      name = lto_input_tree (ib, data_in);
      ssa_name = make_ssa_name_fn (fn, name, gimple_build_nop ());

      if (is_default_def)
	set_default_def (SSA_NAME_VAR (ssa_name), ssa_name);

      i = lto_input_uleb128 (ib);
    }
}


/* Fixup the reference tree OP for replaced VAR_DECLs with mismatched
   types.  */

static void
maybe_fixup_handled_component (tree op)
{
  tree decl_type;
  tree wanted_type;

  while (handled_component_p (TREE_OPERAND (op, 0)))
    op = TREE_OPERAND (op, 0);
  if (TREE_CODE (TREE_OPERAND (op, 0)) != VAR_DECL)
    return;

  decl_type = TREE_TYPE (TREE_OPERAND (op, 0));

  switch (TREE_CODE (op))
    {
    case COMPONENT_REF:
      /* The DECL_CONTEXT of the field-decl is the record type we look for.  */
      wanted_type = DECL_CONTEXT (TREE_OPERAND (op, 1));
      break;

    case ARRAY_REF:
      if (TREE_CODE (decl_type) == ARRAY_TYPE
	  && (TREE_TYPE (decl_type) == TREE_TYPE (op)
	      || useless_type_conversion_p (TREE_TYPE (op),
					    TREE_TYPE (decl_type))))
	return;
      /* An unknown size array type should be ok.  But we do not
         lower the lower bound in all cases - ugh.  */
      wanted_type = build_array_type (TREE_TYPE (op), NULL_TREE);
      break;

    case ARRAY_RANGE_REF:
      if (TREE_CODE (decl_type) == ARRAY_TYPE
	  && (TREE_TYPE (decl_type) == TREE_TYPE (TREE_TYPE (op))
	      || useless_type_conversion_p (TREE_TYPE (TREE_TYPE (op)),
					    TREE_TYPE (decl_type))))
	return;
      /* An unknown size array type should be ok.  But we do not
         lower the lower bound in all cases - ugh.  */
      wanted_type = build_array_type (TREE_TYPE (TREE_TYPE (op)), NULL_TREE);
      break;

    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
      /* Very nice - nothing to do.  */
      return;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (TREE_CODE (decl_type) == COMPLEX_TYPE
	  && (TREE_TYPE (decl_type) == TREE_TYPE (op)
	      || useless_type_conversion_p (TREE_TYPE (op),
					    TREE_TYPE (decl_type))))
	return;
      wanted_type = build_complex_type (TREE_TYPE (op));
      break;

    default:
      gcc_unreachable ();
    }

  if (!useless_type_conversion_p (wanted_type, decl_type))
    TREE_OPERAND (op, 0) = build1 (VIEW_CONVERT_EXPR, wanted_type,
				   TREE_OPERAND (op, 0));
}

/* Fixup reference tree operands for substituted prevailing decls
   with mismatched types in STMT.  This handles plain DECLs where
   we need the stmt for context to lookup the required type.  */

static void
maybe_fixup_decls (gimple stmt)
{
  /* We have to fixup replaced decls here in case there were
     inter-TU type mismatches.  Catch the most common cases
     for now - this way we'll get testcases for the rest as
     the type verifier will complain.  */
  if (gimple_assign_single_p (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);

      /* First catch loads and aggregate copies by adjusting the rhs.  */
      if (TREE_CODE (rhs) == VAR_DECL)
	{
	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    gimple_assign_set_rhs1 (stmt, build1 (VIEW_CONVERT_EXPR,
						  TREE_TYPE (lhs), rhs));
	}
      /* Then catch scalar stores.  */
      else if (TREE_CODE (lhs) == VAR_DECL)
	{
	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    gimple_assign_set_lhs (stmt, build1 (VIEW_CONVERT_EXPR,
						 TREE_TYPE (rhs), lhs));
	}
    }
  else if (is_gimple_call (stmt))
    {
      tree lhs = gimple_call_lhs (stmt);

      if (lhs && TREE_CODE (lhs) == VAR_DECL)
	{
	  if (!useless_type_conversion_p (TREE_TYPE (lhs),
					  gimple_call_return_type (stmt)))
	    gimple_call_set_lhs (stmt, build1 (VIEW_CONVERT_EXPR,
					       gimple_call_return_type (stmt),
					       lhs));
	}

      /* Arguments, especially for varargs functions will be funny...  */
    }
}

/* Read a statement with tag TAG in function FN from block IB using
   descriptors in DATA_IN.  */

static gimple
input_gimple_stmt (struct lto_input_block *ib, struct data_in *data_in,
		   struct function *fn, enum LTO_tags tag)
{
  gimple stmt;
  enum gimple_code code;
  unsigned HOST_WIDE_INT num_ops;
  size_t i;
  struct bitpack_d *bp;

  code = lto_tag_to_gimple_code (tag);

  /* Read the tuple header.  */
  bp = lto_input_bitpack (ib);
  num_ops = bp_unpack_value (bp, sizeof (unsigned) * 8);
  stmt = gimple_alloc (code, num_ops);
  stmt->gsbase.no_warning = bp_unpack_value (bp, 1);
  if (is_gimple_assign (stmt))
    stmt->gsbase.nontemporal_move = bp_unpack_value (bp, 1);
  stmt->gsbase.has_volatile_ops = bp_unpack_value (bp, 1);
  stmt->gsbase.subcode = bp_unpack_value (bp, 16);
  bitpack_delete (bp);

  /* Read location information.  */
  gimple_set_location (stmt, lto_input_location (ib, data_in));

  /* Read lexical block reference.  */
  gimple_set_block (stmt, lto_input_tree (ib, data_in));

  /* Read in all the operands.  */
  switch (code)
    {
    case GIMPLE_RESX:
      gimple_resx_set_region (stmt, lto_input_sleb128 (ib));
      break;

    case GIMPLE_EH_MUST_NOT_THROW:
      gimple_eh_must_not_throw_set_fndecl (stmt, lto_input_tree (ib, data_in));
      break;

    case GIMPLE_EH_DISPATCH:
      gimple_eh_dispatch_set_region (stmt, lto_input_sleb128 (ib));
      break;

    case GIMPLE_ASM:
      {
	/* FIXME lto.  Move most of this into a new gimple_asm_set_string().  */
	tree str;
	stmt->gimple_asm.ni = lto_input_uleb128 (ib);
	stmt->gimple_asm.no = lto_input_uleb128 (ib);
	stmt->gimple_asm.nc = lto_input_uleb128 (ib);
	stmt->gimple_asm.nl = lto_input_uleb128 (ib);
	str = input_string_cst (data_in, ib);
	stmt->gimple_asm.string = TREE_STRING_POINTER (str);
      }
      /* Fallthru  */

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
    case GIMPLE_RETURN:
    case GIMPLE_SWITCH:
    case GIMPLE_LABEL:
    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_DEBUG:
      for (i = 0; i < num_ops; i++)
	{
	  tree op = lto_input_tree (ib, data_in);
	  gimple_set_op (stmt, i, op);
	  if (!op)
	    continue;

	  /* Fixup reference tree operands for substituted prevailing decls
	     with mismatched types.  For plain VAR_DECLs we need to look
	     at context to determine the wanted type - we do that below
	     after the stmt is completed.  */
	  if (TREE_CODE (op) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (op, 0)) == VAR_DECL
	      && !useless_type_conversion_p (TREE_TYPE (TREE_TYPE (op)),
					     TREE_TYPE (TREE_OPERAND (op, 0))))
	    {
	      TREE_OPERAND (op, 0)
		= build1 (VIEW_CONVERT_EXPR, TREE_TYPE (TREE_TYPE (op)),
			  TREE_OPERAND (op, 0));
	      continue;
	    }

	  /* Fixup FIELD_DECLs in COMPONENT_REFs, they are not handled
	     by decl merging.  */
	  if (TREE_CODE (op) == ADDR_EXPR)
	    op = TREE_OPERAND (op, 0);
	  while (handled_component_p (op))
	    {
	      if (TREE_CODE (op) == COMPONENT_REF)
		{
		  tree field, type, tem;
		  field = TREE_OPERAND (op, 1);
		  type = DECL_CONTEXT (field);
		  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
		    {
		      if (tem == field
			  || (TREE_TYPE (tem) == TREE_TYPE (field)
			      && compare_field_offset (tem, field)))
			break;
		    }
		  /* In case of type mismatches across units we can fail
		     to unify some types and thus not find a proper
		     field-decl here.  So only assert here if checking
		     is enabled.  */
#ifdef ENABLE_CHECKING
		  gcc_assert (tem != NULL_TREE);
#endif
		  if (tem != NULL_TREE)
		    TREE_OPERAND (op, 1) = tem;
		}

	      /* Preserve the last handled component for the fixup of
	         its operand below.  */
	      if (!handled_component_p (TREE_OPERAND (op, 0)))
		break;
	      op = TREE_OPERAND (op, 0);
	    }

	  /* Fixup reference tree operands for substituted prevailing decls
	     with mismatched types.  */
	  if (handled_component_p (op))
	    maybe_fixup_handled_component (op);
	}
      break;

    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
      break;

    default:
      internal_error ("bytecode stream: unknown GIMPLE statement tag %s",
		      lto_tag_name (tag));
    }

  /* Update the properties of symbols, SSA names and labels associated
     with STMT.  */
  if (code == GIMPLE_ASSIGN || code == GIMPLE_CALL)
    {
      tree lhs = gimple_get_lhs (stmt);
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	SSA_NAME_DEF_STMT (lhs) = stmt;
    }
  else if (code == GIMPLE_LABEL)
    gcc_assert (emit_label_in_global_context_p (gimple_label_label (stmt))
	        || DECL_CONTEXT (gimple_label_label (stmt)) == fn->decl);
  else if (code == GIMPLE_ASM)
    {
      unsigned i;

      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
	{
	  tree op = TREE_VALUE (gimple_asm_output_op (stmt, i));
	  if (TREE_CODE (op) == SSA_NAME)
	    SSA_NAME_DEF_STMT (op) = stmt;
	}
    }

  /* Fixup reference tree operands for substituted prevailing decls
     with mismatched types.  */
  maybe_fixup_decls (stmt);

  /* Mark the statement modified so its operand vectors can be filled in.  */
  gimple_set_modified (stmt, true);

  return stmt;
}


/* Read a basic block with tag TAG from DATA_IN using input block IB.
   FN is the function being processed.  */

static void
input_bb (struct lto_input_block *ib, enum LTO_tags tag,
	  struct data_in *data_in, struct function *fn)
{
  unsigned int index;
  basic_block bb;
  gimple_stmt_iterator bsi;

  /* This routine assumes that CFUN is set to FN, as it needs to call
     basic GIMPLE routines that use CFUN.  */
  gcc_assert (cfun == fn);

  index = lto_input_uleb128 (ib);
  bb = BASIC_BLOCK_FOR_FUNCTION (fn, index);

  bb->count = lto_input_sleb128 (ib);
  bb->loop_depth = lto_input_sleb128 (ib);
  bb->frequency = lto_input_sleb128 (ib);
  bb->flags = lto_input_sleb128 (ib);

  /* LTO_bb1 has statements.  LTO_bb0 does not.  */
  if (tag == LTO_bb0)
    return;

  bsi = gsi_start_bb (bb);
  tag = input_record_start (ib);
  while (tag)
    {
      gimple stmt = input_gimple_stmt (ib, data_in, fn, tag);

      /* Change debug stmts to nops on-the-fly if we do not have VTA enabled.
	 This allows us to build for example static libs with debugging
	 enabled and do the final link without.  */
      if (!MAY_HAVE_DEBUG_STMTS
	  && is_gimple_debug (stmt))
	stmt = gimple_build_nop ();

      find_referenced_vars_in (stmt);
      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);

      /* After the statement, expect a 0 delimiter or the EH region
	 that the previous statement belongs to.  */
      tag = input_record_start (ib);
      lto_tag_check_set (tag, 2, LTO_eh_region, LTO_null);

      if (tag == LTO_eh_region)
	{
	  HOST_WIDE_INT region = lto_input_sleb128 (ib);
	  gcc_assert (region == (int) region);
	  add_stmt_to_eh_lp (stmt, region);
	}

      tag = input_record_start (ib);
    }

  tag = input_record_start (ib);
  while (tag)
    {
      gimple phi = input_phi (ib, bb, data_in, fn);
      find_referenced_vars_in (phi);
      tag = input_record_start (ib);
    }
}

/* Go through all NODE edges and fixup call_stmt pointers
   so they point to STMTS.  */

static void
fixup_call_stmt_edges_1 (struct cgraph_node *node, gimple *stmts)
{
  struct cgraph_edge *cedge;
  for (cedge = node->callees; cedge; cedge = cedge->next_callee)
    cedge->call_stmt = stmts[cedge->lto_stmt_uid];
}

/* Fixup call_stmt pointers in NODE and all clones.  */

static void
fixup_call_stmt_edges (struct cgraph_node *orig, gimple *stmts)
{
  struct cgraph_node *node;

  while (orig->clone_of)
    orig = orig->clone_of;

  fixup_call_stmt_edges_1 (orig, stmts);
  if (orig->clones)
    for (node = orig->clones; node != orig;)
      {
	fixup_call_stmt_edges_1 (node, stmts);
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

/* Read the body of function FN_DECL from DATA_IN using input block IB.  */

static void
input_function (tree fn_decl, struct data_in *data_in,
		struct lto_input_block *ib)
{
  struct function *fn;
  enum LTO_tags tag;
  gimple *stmts;
  basic_block bb;
  struct bitpack_d *bp;
  struct cgraph_node *node;
  tree args, narg, oarg;

  fn = DECL_STRUCT_FUNCTION (fn_decl);
  tag = input_record_start (ib);
  clear_line_info (data_in);

  gimple_register_cfg_hooks ();
  lto_tag_check (tag, LTO_function);

  /* Read all the attributes for FN.  */
  bp = lto_input_bitpack (ib);
  fn->is_thunk = bp_unpack_value (bp, 1);
  fn->has_local_explicit_reg_vars = bp_unpack_value (bp, 1);
  fn->after_tree_profile = bp_unpack_value (bp, 1);
  fn->returns_pcc_struct = bp_unpack_value (bp, 1);
  fn->returns_struct = bp_unpack_value (bp, 1);
  fn->always_inline_functions_inlined = bp_unpack_value (bp, 1);
  fn->after_inlining = bp_unpack_value (bp, 1);
  fn->dont_save_pending_sizes_p = bp_unpack_value (bp, 1);
  fn->stdarg = bp_unpack_value (bp, 1);
  fn->has_nonlocal_label = bp_unpack_value (bp, 1);
  fn->calls_alloca = bp_unpack_value (bp, 1);
  fn->calls_setjmp = bp_unpack_value (bp, 1);
  fn->function_frequency = (enum function_frequency) bp_unpack_value (bp, 2);
  fn->va_list_fpr_size = bp_unpack_value (bp, 8);
  fn->va_list_gpr_size = bp_unpack_value (bp, 8);
  bitpack_delete (bp);

  /* Input the current IL state of the function.  */
  fn->curr_properties = lto_input_uleb128 (ib);

  /* Read the static chain and non-local goto save area.  */
  fn->static_chain_decl = lto_input_tree (ib, data_in);
  fn->nonlocal_goto_save_area = lto_input_tree (ib, data_in);

  /* Read all the local symbols.  */
  fn->local_decls = lto_input_tree (ib, data_in);

  /* Read all function arguments.  We need to re-map them here to the
     arguments of the merged function declaration.  */
  args = lto_input_tree (ib, data_in);
  for (oarg = args, narg = DECL_ARGUMENTS (fn_decl);
       oarg && narg;
       oarg = TREE_CHAIN (oarg), narg = TREE_CHAIN (narg))
    {
      int ix;
      bool res;
      res = lto_streamer_cache_lookup (data_in->reader_cache, oarg, &ix);
      gcc_assert (res);
      /* Replace the argument in the streamer cache.  */
      lto_streamer_cache_insert_at (data_in->reader_cache, narg, ix);
    }
  gcc_assert (!oarg && !narg);

  /* Read all the SSA names.  */
  input_ssa_names (ib, data_in, fn);

  /* Read the exception handling regions in the function.  */
  input_eh_regions (ib, data_in, fn);

  /* Read the tree of lexical scopes for the function.  */
  DECL_INITIAL (fn_decl) = lto_input_tree (ib, data_in);
  gcc_assert (DECL_INITIAL (fn_decl));
  DECL_SAVED_TREE (fn_decl) = NULL_TREE;

  /* Read all the basic blocks.  */
  tag = input_record_start (ib);
  while (tag)
    {
      input_bb (ib, tag, data_in, fn);
      tag = input_record_start (ib);
    }

  /* Fix up the call statements that are mentioned in the callgraph
     edges.  */
  renumber_gimple_stmt_uids ();
  stmts = (gimple *) xcalloc (gimple_stmt_max_uid (fn), sizeof (gimple));
  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator bsi;
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  stmts[gimple_uid (stmt)] = stmt;
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

  node = cgraph_node (fn_decl);
  fixup_call_stmt_edges (node, stmts);
  execute_all_ipa_stmt_fixups (node, stmts);

  update_ssa (TODO_update_ssa_only_virtuals);
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  free (stmts);
}


/* Read initializer expressions for public statics.  DATA_IN is the
   file being read.  IB is the input block used for reading.  */

static void
input_alias_pairs (struct lto_input_block *ib, struct data_in *data_in)
{
  tree var;

  clear_line_info (data_in);

  /* Skip over all the unreferenced globals.  */
  do
    var = lto_input_tree (ib, data_in);
  while (var);

  var = lto_input_tree (ib, data_in);
  while (var)
    {
      const char *orig_name, *new_name;
      alias_pair *p;

      p = VEC_safe_push (alias_pair, gc, alias_pairs, NULL);
      p->decl = var;
      p->target = lto_input_tree (ib, data_in);

      /* If the target is a static object, we may have registered a
	 new name for it to avoid clashes between statics coming from
	 different files.  In that case, use the new name.  */
      orig_name = IDENTIFIER_POINTER (p->target);
      new_name = lto_get_decl_name_mapping (data_in->file_data, orig_name);
      if (strcmp (orig_name, new_name) != 0)
	p->target = get_identifier (new_name);

      var = lto_input_tree (ib, data_in);
    }
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
  int32_t cfg_offset;
  int32_t main_offset;
  int32_t string_offset;
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
				header->string_size, NULL);

  /* Make sure the file was generated by the exact same compiler.  */
  lto_check_version (header->lto_header.major_version,
		     header->lto_header.minor_version);

  if (section_type == LTO_section_function_body)
    {
      struct function *fn = DECL_STRUCT_FUNCTION (fn_decl);
      struct lto_in_decl_state *decl_state;

      push_cfun (fn);
      init_tree_ssa (fn);

      /* Use the function's decl state. */
      decl_state = lto_get_function_in_decl_state (file_data, fn_decl);
      gcc_assert (decl_state);
      file_data->current_decl_state = decl_state;

      input_cfg (&ib_cfg, fn);

      /* Set up the struct function.  */
      input_function (fn_decl, data_in, &ib_main);

      /* We should now be in SSA.  */
      cfun->gimple_df->in_ssa_p = true;

      /* Restore decl state */
      file_data->current_decl_state = file_data->global_decl_state;

      pop_cfun ();
    }
  else
    {
      input_alias_pairs (&ib_main, data_in);
    }

  clear_line_info (data_in);
  lto_data_in_delete (data_in);
}


/* Read the body of FN_DECL using DATA.  FILE_DATA holds the global
   decls and types.  */

void
lto_input_function_body (struct lto_file_decl_data *file_data,
			 tree fn_decl, const char *data)
{
  current_function_decl = fn_decl;
  lto_read_body (file_data, fn_decl, data, LTO_section_function_body);
}


/* Read in VAR_DECL using DATA.  FILE_DATA holds the global decls and
   types.  */

void
lto_input_constructors_and_inits (struct lto_file_decl_data *file_data,
				  const char *data)
{
  lto_read_body (file_data, NULL, data, LTO_section_static_initializer);
}


/* Return the resolution for the decl with index INDEX from DATA_IN. */

static enum ld_plugin_symbol_resolution
get_resolution (struct data_in *data_in, unsigned index)
{
  if (data_in->globals_resolution)
    {
      ld_plugin_symbol_resolution_t ret;
      /* We can have references to not emitted functions in
	 DECL_FUNCTION_PERSONALITY at least.  So we can and have
	 to indeed return LDPR_UNKNOWN in some cases.   */
      if (VEC_length (ld_plugin_symbol_resolution_t,
		      data_in->globals_resolution) <= index)
	return LDPR_UNKNOWN;
      ret = VEC_index (ld_plugin_symbol_resolution_t,
		       data_in->globals_resolution,
		       index);
      return ret;
    }
  else
    /* Delay resolution finding until decl merging.  */
    return LDPR_UNKNOWN;
}


/* Unpack all the non-pointer fields of the TS_BASE structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_base_value_fields (struct bitpack_d *bp, tree expr)
{
  /* Note that the code for EXPR has already been unpacked to create EXPR in
     lto_materialize_tree.  */
  if (!TYPE_P (expr))
    {
      TREE_SIDE_EFFECTS (expr) = (unsigned) bp_unpack_value (bp, 1);
      TREE_CONSTANT (expr) = (unsigned) bp_unpack_value (bp, 1);
      TREE_READONLY (expr) = (unsigned) bp_unpack_value (bp, 1);

      /* TREE_PUBLIC is used on types to indicate that the type
	 has a TYPE_CACHED_VALUES vector.  This is not streamed out,
	 so we skip it here.  */
      TREE_PUBLIC (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
  TREE_ADDRESSABLE (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_THIS_VOLATILE (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (DECL_P (expr))
    DECL_UNSIGNED (expr) = (unsigned) bp_unpack_value (bp, 1);
  else if (TYPE_P (expr))
    TYPE_UNSIGNED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_ASM_WRITTEN (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_NO_WARNING (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_USED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_NOTHROW (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_STATIC (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_PRIVATE (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_PROTECTED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_DEPRECATED (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (TYPE_P (expr))
    TYPE_SATURATING (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (TREE_CODE (expr) == SSA_NAME)
    SSA_NAME_IS_DEFAULT_DEF (expr) = (unsigned) bp_unpack_value (bp, 1);
}


/* Unpack all the non-pointer fields of the TS_REAL_CST structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_real_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  unsigned i;
  REAL_VALUE_TYPE r;
  REAL_VALUE_TYPE *rp;

  r.cl = (unsigned) bp_unpack_value (bp, 2);
  r.decimal = (unsigned) bp_unpack_value (bp, 1);
  r.sign = (unsigned) bp_unpack_value (bp, 1);
  r.signalling = (unsigned) bp_unpack_value (bp, 1);
  r.canonical = (unsigned) bp_unpack_value (bp, 1);
  r.uexp = (unsigned) bp_unpack_value (bp, EXP_BITS);
  for (i = 0; i < SIGSZ; i++)
    r.sig[i] = (unsigned long) bp_unpack_value (bp, HOST_BITS_PER_LONG);

  rp = GGC_NEW (REAL_VALUE_TYPE);
  memcpy (rp, &r, sizeof (REAL_VALUE_TYPE));
  TREE_REAL_CST_PTR (expr) = rp;
}


/* Unpack all the non-pointer fields of the TS_FIXED_CST structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_fixed_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  struct fixed_value fv;

  fv.data.low = (HOST_WIDE_INT) bp_unpack_value (bp, HOST_BITS_PER_WIDE_INT);
  fv.data.high = (HOST_WIDE_INT) bp_unpack_value (bp, HOST_BITS_PER_WIDE_INT);
  fv.mode = (enum machine_mode) bp_unpack_value (bp, HOST_BITS_PER_INT);
  TREE_FIXED_CST (expr) = fv;
}


/* Unpack all the non-pointer fields of the TS_DECL_COMMON structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_common_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_MODE (expr) = (enum machine_mode) bp_unpack_value (bp, 8);
  DECL_NONLOCAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_VIRTUAL_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IGNORED_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_ABSTRACT (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_ARTIFICIAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_USER_ALIGN (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_PRESERVE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DEBUG_EXPR_IS_FROM (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_EXTERNAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_GIMPLE_REG_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_ALIGN (expr) = (unsigned) bp_unpack_value (bp, HOST_BITS_PER_INT);

  if (TREE_CODE (expr) == LABEL_DECL)
    {
      DECL_ERROR_ISSUED (expr) = (unsigned) bp_unpack_value (bp, 1);
      EH_LANDING_PAD_NR (expr) = (int) bp_unpack_value (bp, HOST_BITS_PER_INT);

      /* Always assume an initial value of -1 for LABEL_DECL_UID to
	 force gimple_set_bb to recreate label_to_block_map.  */
      LABEL_DECL_UID (expr) = -1;
    }

  if (TREE_CODE (expr) == FIELD_DECL)
    {
      unsigned HOST_WIDE_INT off_align;
      DECL_PACKED (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_NONADDRESSABLE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      off_align = (unsigned HOST_WIDE_INT) bp_unpack_value (bp, 8);
      SET_DECL_OFFSET_ALIGN (expr, off_align);
    }

  if (TREE_CODE (expr) == RESULT_DECL
      || TREE_CODE (expr) == PARM_DECL
      || TREE_CODE (expr) == VAR_DECL)
    {
      DECL_BY_REFERENCE (expr) = (unsigned) bp_unpack_value (bp, 1);
      if (TREE_CODE (expr) == VAR_DECL
	  || TREE_CODE (expr) == PARM_DECL)
	DECL_HAS_VALUE_EXPR_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_RESTRICTED_P (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
}


/* Unpack all the non-pointer fields of the TS_DECL_WRTL structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_wrtl_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_REGISTER (expr) = (unsigned) bp_unpack_value (bp, 1);
}


/* Unpack all the non-pointer fields of the TS_DECL_WITH_VIS structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_with_vis_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_DEFER_OUTPUT (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_COMMON (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DLLIMPORT_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_WEAK (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_SEEN_IN_BIND_EXPR_P (expr) = (unsigned) bp_unpack_value (bp,  1);
  DECL_COMDAT (expr) = (unsigned) bp_unpack_value (bp,  1);
  DECL_VISIBILITY (expr) = (enum symbol_visibility) bp_unpack_value (bp,  2);
  DECL_VISIBILITY_SPECIFIED (expr) = (unsigned) bp_unpack_value (bp,  1);

  if (TREE_CODE (expr) == VAR_DECL)
    {
      DECL_HARD_REGISTER (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_IN_TEXT_SECTION (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_TLS_MODEL (expr) = (enum tls_model) bp_unpack_value (bp,  3);
    }

  if (VAR_OR_FUNCTION_DECL_P (expr))
    {
      priority_type p;
      p = (priority_type) bp_unpack_value (bp, HOST_BITS_PER_SHORT);
      SET_DECL_INIT_PRIORITY (expr, p);
    }
}


/* Unpack all the non-pointer fields of the TS_FUNCTION_DECL structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_function_decl_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_FUNCTION_CODE (expr) = (enum built_in_function) bp_unpack_value (bp, 11);
  DECL_BUILT_IN_CLASS (expr) = (enum built_in_class) bp_unpack_value (bp, 2);
  DECL_STATIC_CONSTRUCTOR (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_STATIC_DESTRUCTOR (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_UNINLINABLE (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_POSSIBLY_INLINED (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_NOVOPS (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_RETURNS_TWICE (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_MALLOC (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_OPERATOR_NEW (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DECLARED_INLINE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_STATIC_CHAIN (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_INLINE_WARNING_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (expr)
    			= (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_LIMIT_STACK (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DISREGARD_INLINE_LIMITS (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_PURE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_LOOPING_CONST_OR_PURE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
}


/* Unpack all the non-pointer fields of the TS_TYPE structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_type_value_fields (struct bitpack_d *bp, tree expr)
{
  enum machine_mode mode;

  TYPE_PRECISION (expr) = (unsigned) bp_unpack_value (bp, 10);
  mode = (enum machine_mode) bp_unpack_value (bp, 8);
  SET_TYPE_MODE (expr, mode);
  TYPE_STRING_FLAG (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_NO_FORCE_BLK (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_NEEDS_CONSTRUCTING (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (RECORD_OR_UNION_TYPE_P (expr))
    TYPE_TRANSPARENT_AGGR (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_PACKED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_RESTRICT (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (expr)
    	= (unsigned) bp_unpack_value (bp, 2);
  TYPE_USER_ALIGN (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_READONLY (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_ALIGN (expr) = (unsigned) bp_unpack_value (bp, HOST_BITS_PER_INT);
  TYPE_ALIAS_SET (expr) = bp_unpack_value (bp, HOST_BITS_PER_INT);
}


/* Unpack all the non-pointer fields of the TS_BLOCK structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_block_value_fields (struct bitpack_d *bp, tree expr)
{
  BLOCK_ABSTRACT (expr) = (unsigned) bp_unpack_value (bp, 1);
  BLOCK_NUMBER (expr) = (unsigned) bp_unpack_value (bp, 31);
}


/* Unpack all the non-pointer fields in EXPR into a bit pack.  */

static void
unpack_value_fields (struct bitpack_d *bp, tree expr)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  /* Note that all these functions are highly sensitive to changes in
     the types and sizes of each of the fields being packed.  */
  unpack_ts_base_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    unpack_ts_real_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    unpack_ts_fixed_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    unpack_ts_decl_common_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    unpack_ts_decl_wrtl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    unpack_ts_decl_with_vis_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    unpack_ts_function_decl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE))
    unpack_ts_type_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    unpack_ts_block_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    {
      /* We only stream the version number of SSA names.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      /* This is only used by GENERIC.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    {
      /* This is only used by High GIMPLE.  */
      gcc_unreachable ();
    }
}


/* Read a bitpack from input block IB.  */

struct bitpack_d *
lto_input_bitpack (struct lto_input_block *ib)
{
  unsigned i, num_words;
  struct bitpack_d *bp;

  bp = bitpack_create ();

  /* If we are about to read more than a handful of words, something
     is wrong.  This check is overly strict, but it acts as an early
     warning.  No streamed object has hundreds of bits in its fields.  */
  num_words = lto_input_uleb128 (ib);
  gcc_assert (num_words < 20);

  for (i = 0; i < num_words; i++)
    {
      bitpack_word_t w = lto_input_uleb128 (ib);
      VEC_safe_push (bitpack_word_t, heap, bp->values, w);
    }

  return bp;
}


/* Materialize a new tree from input block IB using descriptors in
   DATA_IN.  The code for the new tree should match TAG.  Store in
   *IX_P the index into the reader cache where the new tree is stored.  */

static tree
lto_materialize_tree (struct lto_input_block *ib, struct data_in *data_in,
		      enum LTO_tags tag, int *ix_p)
{
  struct bitpack_d *bp;
  enum tree_code code;
  tree result;
#ifdef LTO_STREAMER_DEBUG
  HOST_WIDEST_INT orig_address_in_writer;
#endif
  HOST_WIDE_INT ix;

  result = NULL_TREE;

  /* Read the header of the node we are about to create.  */
  ix = lto_input_sleb128 (ib);
  gcc_assert ((int) ix == ix);
  *ix_p = (int) ix;

#ifdef LTO_STREAMER_DEBUG
  /* Read the word representing the memory address for the tree
     as it was written by the writer.  This is useful when
     debugging differences between the writer and reader.  */
  orig_address_in_writer = lto_input_sleb128 (ib);
  gcc_assert ((intptr_t) orig_address_in_writer == orig_address_in_writer);
#endif

  code = lto_tag_to_tree_code (tag);

  /* We should never see an SSA_NAME tree.  Only the version numbers of
     SSA names are ever written out.  See input_ssa_names.  */
  gcc_assert (code != SSA_NAME);

  /* Instantiate a new tree using the header data.  */
  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    result = input_string_cst (data_in, ib);
  else if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    result = input_identifier (data_in, ib);
  else if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    {
      HOST_WIDE_INT len = lto_input_sleb128 (ib);
      result = make_tree_vec (len);
    }
  else if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned HOST_WIDE_INT len = lto_input_uleb128 (ib);
      result = make_tree_binfo (len);
    }
  else
    {
      /* All other nodes can be materialized with a raw make_node
	 call.  */
      result = make_node (code);
    }

#ifdef LTO_STREAMER_DEBUG
  /* Store the original address of the tree as seen by the writer
     in RESULT's aux field.  This is useful when debugging streaming
     problems.  This way, a debugging session can be started on
     both writer and reader with a breakpoint using this address
     value in both.  */
  lto_orig_address_map (result, (intptr_t) orig_address_in_writer);
#endif

  /* Read the bitpack of non-pointer values from IB.  */
  bp = lto_input_bitpack (ib);

  /* The first word in BP contains the code of the tree that we
     are about to read.  */
  code = (enum tree_code) bp_unpack_value (bp, 16);
  lto_tag_check (lto_tree_code_to_tag (code), tag);

  /* Unpack all the value fields from BP.  */
  unpack_value_fields (bp, result);
  bitpack_delete (bp);

  /* Enter RESULT in the reader cache.  This will make RESULT
     available so that circular references in the rest of the tree
     structure can be resolved in subsequent calls to lto_input_tree.  */
  lto_streamer_cache_insert_at (data_in->reader_cache, result, ix);

  return result;
}


/* Read a chain of tree nodes from input block IB. DATA_IN contains
   tables and descriptors for the file being read.  */

static tree
lto_input_chain (struct lto_input_block *ib, struct data_in *data_in)
{
  int i, count;
  tree first, prev, curr;

  first = prev = NULL_TREE;
  count = lto_input_sleb128 (ib);
  for (i = 0; i < count; i++)
    {
      curr = lto_input_tree (ib, data_in);
      if (prev)
	TREE_CHAIN (prev) = curr;
      else
	first = curr;

      TREE_CHAIN (curr) = NULL_TREE;
      prev = curr;
    }

  return first;
}


/* Read all pointer fields in the TS_COMMON structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */


static void
lto_input_ts_common_tree_pointers (struct lto_input_block *ib,
				   struct data_in *data_in, tree expr)
{
  TREE_TYPE (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_VECTOR structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_vector_tree_pointers (struct lto_input_block *ib,
				   struct data_in *data_in, tree expr)
{
  TREE_VECTOR_CST_ELTS (expr) = lto_input_chain (ib, data_in);
}


/* Read all pointer fields in the TS_COMPLEX structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_complex_tree_pointers (struct lto_input_block *ib,
				    struct data_in *data_in, tree expr)
{
  TREE_REALPART (expr) = lto_input_tree (ib, data_in);
  TREE_IMAGPART (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_MINIMAL structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_minimal_tree_pointers (struct lto_input_block *ib,
					 struct data_in *data_in, tree expr)
{
  DECL_NAME (expr) = lto_input_tree (ib, data_in);
  DECL_CONTEXT (expr) = lto_input_tree (ib, data_in);
  DECL_SOURCE_LOCATION (expr) = lto_input_location (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_COMMON structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_common_tree_pointers (struct lto_input_block *ib,
					struct data_in *data_in, tree expr)
{
  DECL_SIZE (expr) = lto_input_tree (ib, data_in);
  DECL_SIZE_UNIT (expr) = lto_input_tree (ib, data_in);

  if (TREE_CODE (expr) != FUNCTION_DECL)
    DECL_INITIAL (expr) = lto_input_tree (ib, data_in);

  DECL_ATTRIBUTES (expr) = lto_input_tree (ib, data_in);
  DECL_ABSTRACT_ORIGIN (expr) = lto_input_tree (ib, data_in);

  if (TREE_CODE (expr) == PARM_DECL)
    TREE_CHAIN (expr) = lto_input_chain (ib, data_in);

  if ((TREE_CODE (expr) == VAR_DECL
       || TREE_CODE (expr) == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (expr))
    SET_DECL_VALUE_EXPR (expr, lto_input_tree (ib, data_in));
}


/* Read all pointer fields in the TS_DECL_NON_COMMON structure of
   EXPR from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_non_common_tree_pointers (struct lto_input_block *ib,
					    struct data_in *data_in, tree expr)
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    {
      DECL_ARGUMENTS (expr) = lto_input_tree (ib, data_in);
      DECL_RESULT (expr) = lto_input_tree (ib, data_in);
    }
  DECL_VINDEX (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_WITH_VIS structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_with_vis_tree_pointers (struct lto_input_block *ib,
				          struct data_in *data_in, tree expr)
{
  tree id;

  id = lto_input_tree (ib, data_in);
  if (id)
    {
      gcc_assert (TREE_CODE (id) == IDENTIFIER_NODE);
      SET_DECL_ASSEMBLER_NAME (expr, id);
    }

  DECL_SECTION_NAME (expr) = lto_input_tree (ib, data_in);
  DECL_COMDAT_GROUP (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_FIELD_DECL structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_field_decl_tree_pointers (struct lto_input_block *ib,
				       struct data_in *data_in, tree expr)
{
  DECL_FIELD_OFFSET (expr) = lto_input_tree (ib, data_in);
  DECL_BIT_FIELD_TYPE (expr) = lto_input_tree (ib, data_in);
  DECL_QUALIFIER (expr) = lto_input_tree (ib, data_in);
  DECL_FIELD_BIT_OFFSET (expr) = lto_input_tree (ib, data_in);
  DECL_FCONTEXT (expr) = lto_input_tree (ib, data_in);
  TREE_CHAIN (expr) = lto_input_chain (ib, data_in);
}


/* Read all pointer fields in the TS_FUNCTION_DECL structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_function_decl_tree_pointers (struct lto_input_block *ib,
					  struct data_in *data_in, tree expr)
{
  /* DECL_STRUCT_FUNCTION is handled by lto_input_function.  FIXME lto,
     maybe it should be handled here?  */
  DECL_FUNCTION_PERSONALITY (expr) = lto_input_tree (ib, data_in);
  DECL_FUNCTION_SPECIFIC_TARGET (expr) = lto_input_tree (ib, data_in);
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr) = lto_input_tree (ib, data_in);

  /* If the file contains a function with an EH personality set,
     then it was compiled with -fexceptions.  In that case, initialize
     the backend EH machinery.  */
  if (DECL_FUNCTION_PERSONALITY (expr))
    lto_init_eh ();
}


/* Read all pointer fields in the TS_TYPE structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_type_tree_pointers (struct lto_input_block *ib,
				 struct data_in *data_in, tree expr)
{
  if (TREE_CODE (expr) == ENUMERAL_TYPE)
    TYPE_VALUES (expr) = lto_input_tree (ib, data_in);
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    TYPE_DOMAIN (expr) = lto_input_tree (ib, data_in);
  else if (RECORD_OR_UNION_TYPE_P (expr))
    TYPE_FIELDS (expr) = lto_input_tree (ib, data_in);
  else if (TREE_CODE (expr) == FUNCTION_TYPE
	   || TREE_CODE (expr) == METHOD_TYPE)
    TYPE_ARG_TYPES (expr) = lto_input_tree (ib, data_in);
  else if (TREE_CODE (expr) == VECTOR_TYPE)
    TYPE_DEBUG_REPRESENTATION_TYPE (expr) = lto_input_tree (ib, data_in);

  TYPE_SIZE (expr) = lto_input_tree (ib, data_in);
  TYPE_SIZE_UNIT (expr) = lto_input_tree (ib, data_in);
  TYPE_ATTRIBUTES (expr) = lto_input_tree (ib, data_in);
  TYPE_NAME (expr) = lto_input_tree (ib, data_in);
  /* Do not stream TYPE_POINTER_TO or TYPE_REFERENCE_TO nor
     TYPE_NEXT_PTR_TO or TYPE_NEXT_REF_TO.  */
  if (!POINTER_TYPE_P (expr))
    TYPE_MINVAL (expr) = lto_input_tree (ib, data_in);
  TYPE_MAXVAL (expr) = lto_input_tree (ib, data_in);
  TYPE_MAIN_VARIANT (expr) = lto_input_tree (ib, data_in);
  /* Do not stream TYPE_NEXT_VARIANT, we reconstruct the variant lists
     during fixup.  */
  if (RECORD_OR_UNION_TYPE_P (expr))
    TYPE_BINFO (expr) = lto_input_tree (ib, data_in);
  TYPE_CONTEXT (expr) = lto_input_tree (ib, data_in);
  TYPE_CANONICAL (expr) = lto_input_tree (ib, data_in);
  TYPE_STUB_DECL (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_LIST structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_list_tree_pointers (struct lto_input_block *ib,
				 struct data_in *data_in, tree expr)
{
  TREE_PURPOSE (expr) = lto_input_tree (ib, data_in);
  TREE_VALUE (expr) = lto_input_tree (ib, data_in);
  TREE_CHAIN (expr) = lto_input_chain (ib, data_in);
}


/* Read all pointer fields in the TS_VEC structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_vec_tree_pointers (struct lto_input_block *ib,
				struct data_in *data_in, tree expr)
{
  int i;

  /* Note that TREE_VEC_LENGTH was read by lto_materialize_tree to
     instantiate EXPR.  */
  for (i = 0; i < TREE_VEC_LENGTH (expr); i++)
    TREE_VEC_ELT (expr, i) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_EXP structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */


static void
lto_input_ts_exp_tree_pointers (struct lto_input_block *ib,
			        struct data_in *data_in, tree expr)
{
  int i, length;
  location_t loc;

  length = lto_input_sleb128 (ib);
  gcc_assert (length == TREE_OPERAND_LENGTH (expr));

  for (i = 0; i < length; i++)
    TREE_OPERAND (expr, i) = lto_input_tree (ib, data_in);

  loc = lto_input_location (ib, data_in);
  SET_EXPR_LOCATION (expr, loc);
  TREE_BLOCK (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_BLOCK structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_block_tree_pointers (struct lto_input_block *ib,
				  struct data_in *data_in, tree expr)
{
  unsigned i, len;

  BLOCK_SOURCE_LOCATION (expr) = lto_input_location (ib, data_in);
  BLOCK_VARS (expr) = lto_input_chain (ib, data_in);

  len = lto_input_uleb128 (ib);
  for (i = 0; i < len; i++)
    {
      tree t = lto_input_tree (ib, data_in);
      VEC_safe_push (tree, gc, BLOCK_NONLOCALIZED_VARS (expr), t);
    }

  BLOCK_SUPERCONTEXT (expr) = lto_input_tree (ib, data_in);
  BLOCK_ABSTRACT_ORIGIN (expr) = lto_input_tree (ib, data_in);
  BLOCK_FRAGMENT_ORIGIN (expr) = lto_input_tree (ib, data_in);
  BLOCK_FRAGMENT_CHAIN (expr) = lto_input_tree (ib, data_in);
  BLOCK_SUBBLOCKS (expr) = lto_input_chain (ib, data_in);
}


/* Read all pointer fields in the TS_BINFO structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_binfo_tree_pointers (struct lto_input_block *ib,
				  struct data_in *data_in, tree expr)
{
  unsigned i, len;
  tree t;

  /* Note that the number of slots in EXPR was read in
     lto_materialize_tree when instantiating EXPR.  However, the
     vector is empty so we cannot rely on VEC_length to know how many
     elements to read.  So, this list is emitted as a 0-terminated
     list on the writer side.  */
  do
    {
      t = lto_input_tree (ib, data_in);
      if (t)
	VEC_quick_push (tree, BINFO_BASE_BINFOS (expr), t);
    }
  while (t);

  BINFO_OFFSET (expr) = lto_input_tree (ib, data_in);
  BINFO_VTABLE (expr) = lto_input_tree (ib, data_in);
  BINFO_VIRTUALS (expr) = lto_input_tree (ib, data_in);
  BINFO_VPTR_FIELD (expr) = lto_input_tree (ib, data_in);

  len = lto_input_uleb128 (ib);
  for (i = 0; i < len; i++)
    {
      tree a = lto_input_tree (ib, data_in);
      VEC_safe_push (tree, gc, BINFO_BASE_ACCESSES (expr), a);
    }

  BINFO_INHERITANCE_CHAIN (expr) = lto_input_tree (ib, data_in);
  BINFO_SUBVTT_INDEX (expr) = lto_input_tree (ib, data_in);
  BINFO_VPTR_INDEX (expr) = lto_input_tree (ib, data_in);
}


/* Read all pointer fields in the TS_CONSTRUCTOR structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_constructor_tree_pointers (struct lto_input_block *ib,
				        struct data_in *data_in, tree expr)
{
  unsigned i, len;

  len = lto_input_uleb128 (ib);
  for (i = 0; i < len; i++)
    {
      tree index, value;

      index = lto_input_tree (ib, data_in);
      value = lto_input_tree (ib, data_in);
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (expr), index, value);
    }
}


/* Helper for lto_input_tree.  Read all pointer fields in EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_tree_pointers (struct lto_input_block *ib, struct data_in *data_in,
			 tree expr)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    lto_input_ts_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    lto_input_ts_vector_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    lto_input_ts_complex_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    lto_input_ts_decl_minimal_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    lto_input_ts_decl_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    lto_input_ts_decl_non_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    lto_input_ts_decl_with_vis_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    lto_input_ts_field_decl_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    lto_input_ts_function_decl_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE))
    lto_input_ts_type_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    lto_input_ts_list_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    lto_input_ts_vec_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    lto_input_ts_exp_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    {
      /* We only stream the version number of SSA names.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    lto_input_ts_block_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    lto_input_ts_binfo_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      /* This should only appear in GENERIC.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    lto_input_ts_constructor_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    {
      /* This should only appear in High GIMPLE.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    {
      sorry ("optimization options not supported yet");
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    {
      sorry ("target optimization options not supported yet");
    }
}


/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  */

static void
lto_register_var_decl_in_symtab (struct data_in *data_in, tree decl)
{
  /* Register symbols with file or global scope to mark what input
     file has their definition.  */
  if (decl_function_context (decl) == NULL_TREE)
    {
      /* Variable has file scope, not local. Need to ensure static variables
	 between different files don't clash unexpectedly.  */
      if (!TREE_PUBLIC (decl))
        {
	  /* ??? We normally pre-mangle names before we serialize them
	     out.  Here, in lto1, we do not know the language, and
	     thus cannot do the mangling again. Instead, we just
	     append a suffix to the mangled name.  The resulting name,
	     however, is not a properly-formed mangled name, and will
	     confuse any attempt to unmangle it.  */
	  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  char *label;

	  ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
	  SET_DECL_ASSEMBLER_NAME (decl, get_identifier (label));
          rest_of_decl_compilation (decl, 1, 0);
        }
    }

  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl))
    {
      int ix;
      if (!lto_streamer_cache_lookup (data_in->reader_cache, decl, &ix))
	gcc_unreachable ();
      lto_symtab_register_decl (decl, get_resolution (data_in, ix),
				data_in->file_data);
    }
}



/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  DATA_IN contains descriptors and tables for the
   file being read.  */

static void
lto_register_function_decl_in_symtab (struct data_in *data_in, tree decl)
{
  /* Need to ensure static entities between different files
     don't clash unexpectedly.  */
  if (!TREE_PUBLIC (decl))
    {
      /* We must not use the DECL_ASSEMBLER_NAME macro here, as it
	 may set the assembler name where it was previously empty.  */
      tree old_assembler_name = decl->decl_with_vis.assembler_name;

      /* FIXME lto: We normally pre-mangle names before we serialize
	 them out.  Here, in lto1, we do not know the language, and
	 thus cannot do the mangling again. Instead, we just append a
	 suffix to the mangled name.  The resulting name, however, is
	 not a properly-formed mangled name, and will confuse any
	 attempt to unmangle it.  */
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (label));

      /* We may arrive here with the old assembler name not set
	 if the function body is not needed, e.g., it has been
	 inlined away and does not appear in the cgraph.  */
      if (old_assembler_name)
	{
	  tree new_assembler_name = DECL_ASSEMBLER_NAME (decl);

	  /* Make the original assembler name available for later use.
	     We may have used it to indicate the section within its
	     object file where the function body may be found.
	     FIXME lto: Find a better way to maintain the function decl
	     to body section mapping so we don't need this hack.  */
	  lto_record_renamed_decl (data_in->file_data,
				   IDENTIFIER_POINTER (old_assembler_name),
				   IDENTIFIER_POINTER (new_assembler_name));

	  /* Also register the reverse mapping so that we can find the
	     new name given to an existing assembler name (used when
	     restoring alias pairs in input_constructors_or_inits.  */
	  lto_record_renamed_decl (data_in->file_data,
				   IDENTIFIER_POINTER (new_assembler_name),
				   IDENTIFIER_POINTER (old_assembler_name));
	}
    }

  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl) && !DECL_ABSTRACT (decl))
    {
      int ix;
      if (!lto_streamer_cache_lookup (data_in->reader_cache, decl, &ix))
	gcc_unreachable ();
      lto_symtab_register_decl (decl, get_resolution (data_in, ix),
				data_in->file_data);
    }
}


/* Read an index IX from input block IB and return the tree node at
   DATA_IN->FILE_DATA->GLOBALS_INDEX[IX].  */

static tree
lto_get_pickled_tree (struct lto_input_block *ib, struct data_in *data_in)
{
  HOST_WIDE_INT ix;
  tree result;
  enum LTO_tags expected_tag;
  unsigned HOST_WIDE_INT orig_offset;

  ix = lto_input_sleb128 (ib);
  expected_tag = (enum LTO_tags) lto_input_uleb128 (ib);

  orig_offset = lto_input_uleb128 (ib);
  gcc_assert (orig_offset == (unsigned) orig_offset);

  result = lto_streamer_cache_get (data_in->reader_cache, ix);
  if (result == NULL_TREE)
    {
      /* We have not yet read the cache slot IX.  Go to the offset
	 in the stream where the physical tree node is, and materialize
	 it from there.  */
      struct lto_input_block fwd_ib;

      /* If we are trying to go back in the stream, something is wrong.
	 We should've read the node at the earlier position already.  */
      if (ib->p >= orig_offset)
	internal_error ("bytecode stream: tried to jump backwards in the "
		        "stream");

      LTO_INIT_INPUT_BLOCK (fwd_ib, ib->data, orig_offset, ib->len);
      result = lto_input_tree (&fwd_ib, data_in);
    }

  gcc_assert (result
              && TREE_CODE (result) == lto_tag_to_tree_code (expected_tag));

  return result;
}


/* Read a code and class from input block IB and return the
   corresponding builtin.  DATA_IN is as in lto_input_tree.  */

static tree
lto_get_builtin_tree (struct lto_input_block *ib, struct data_in *data_in)
{
  enum built_in_class fclass;
  enum built_in_function fcode;
  const char *asmname;
  tree result;
  int ix;

  fclass = (enum built_in_class) lto_input_uleb128 (ib);
  gcc_assert (fclass == BUILT_IN_NORMAL || fclass == BUILT_IN_MD);

  fcode = (enum built_in_function) lto_input_uleb128 (ib);

  ix = lto_input_sleb128 (ib);
  gcc_assert (ix == (int) ix);

  if (fclass == BUILT_IN_NORMAL)
    {
      gcc_assert (fcode < END_BUILTINS);
      result = built_in_decls[fcode];
      gcc_assert (result);
    }
  else if (fclass == BUILT_IN_MD)
    {
      result = targetm.builtin_decl (fcode, true);
      if (!result || result == error_mark_node)
	fatal_error ("target specific builtin not available");
    }
  else
    gcc_unreachable ();

  asmname = input_string (data_in, ib);
  if (asmname)
    set_builtin_user_assembler_name (result, asmname);

  lto_streamer_cache_insert_at (data_in->reader_cache, result, ix);

  return result;
}


/* Read the physical representation of a tree node with tag TAG from
   input block IB using the per-file context in DATA_IN.  */

static tree
lto_read_tree (struct lto_input_block *ib, struct data_in *data_in,
	       enum LTO_tags tag)
{
  tree result;
  int ix;

  result = lto_materialize_tree (ib, data_in, tag, &ix);

  /* Read all the pointer fields in RESULT.  */
  lto_input_tree_pointers (ib, data_in, result);

  /* We should never try to instantiate an MD or NORMAL builtin here.  */
  if (TREE_CODE (result) == FUNCTION_DECL)
    gcc_assert (!lto_stream_as_builtin_p (result));

  if (TREE_CODE (result) == VAR_DECL)
    lto_register_var_decl_in_symtab (data_in, result);
  else if (TREE_CODE (result) == FUNCTION_DECL && !DECL_BUILT_IN (result))
    lto_register_function_decl_in_symtab (data_in, result);

  /* end_marker = */ lto_input_1_unsigned (ib);

#ifdef LTO_STREAMER_DEBUG
  /* Remove the mapping to RESULT's original address set by
     lto_materialize_tree.  */
  lto_orig_address_remove (result);
#endif

  return result;
}


/* Read and INTEGER_CST node from input block IB using the per-file
   context in DATA_IN.  */

static tree
lto_input_integer_cst (struct lto_input_block *ib, struct data_in *data_in)
{
  tree result, type;
  HOST_WIDE_INT low, high;
  bool overflow_p;

  type = lto_input_tree (ib, data_in);
  overflow_p = (lto_input_1_unsigned (ib) != 0);
  low = lto_input_uleb128 (ib);
  high = lto_input_uleb128 (ib);
  result = build_int_cst_wide (type, low, high);

  /* If the original constant had overflown, build a replica of RESULT to
     avoid modifying the shared constant returned by build_int_cst_wide.  */
  if (overflow_p)
    {
      result = copy_node (result);
      TREE_OVERFLOW (result) = 1;
    }

  return result;
}


/* Read a tree from input block IB using the per-file context in
   DATA_IN.  This context is used, for example, to resolve references
   to previously read nodes.  */

tree
lto_input_tree (struct lto_input_block *ib, struct data_in *data_in)
{
  enum LTO_tags tag;
  tree result;

  tag = input_record_start (ib);
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
      result = lto_get_pickled_tree (ib, data_in);
    }
  else if (tag == LTO_builtin_decl)
    {
      /* If we are going to read a built-in function, all we need is
	 the code and class.  */
      result = lto_get_builtin_tree (ib, data_in);
    }
  else if (tag == LTO_var_decl_alias)
    {
      /* An extra_name alias for a variable.  */
      unsigned HOST_WIDE_INT ix;
      tree target;
      ix = lto_input_uleb128 (ib);
      result = lto_file_decl_data_get_var_decl (data_in->file_data, ix);
      ix = lto_input_uleb128 (ib);
      target = lto_file_decl_data_get_var_decl (data_in->file_data, ix);
      varpool_extra_name_alias (result, target);
    }
  else if (tag == lto_tree_code_to_tag (INTEGER_CST))
    {
      /* For integer constants we only need the type and its hi/low
	 words.  */
      result = lto_input_integer_cst (ib, data_in);
    }
  else
    {
      /* Otherwise, materialize a new node from IB.  */
      result = lto_read_tree (ib, data_in, tag);
    }

  return result;
}


/* Initialization for the LTO reader.  */

void
lto_init_reader (void)
{
  lto_streamer_init ();

  memset (&lto_stats, 0, sizeof (lto_stats));
  bitmap_obstack_initialize (NULL);

  file_name_hash_table = htab_create (37, hash_string_slot_node,
				      eq_string_slot_node, free);

  gimple_register_cfg_hooks ();
}


/* Create a new data_in object for FILE_DATA. STRINGS is the string
   table to use with LEN strings.  RESOLUTIONS is the vector of linker
   resolutions (NULL if not using a linker plugin).  */

struct data_in *
lto_data_in_create (struct lto_file_decl_data *file_data, const char *strings,
		    unsigned len,
		    VEC(ld_plugin_symbol_resolution_t,heap) *resolutions)
{
  struct data_in *data_in = XCNEW (struct data_in);
  data_in->file_data = file_data;
  data_in->strings = strings;
  data_in->strings_len = len;
  data_in->globals_resolution = resolutions;
  data_in->reader_cache = lto_streamer_cache_create ();

  return data_in;
}


/* Remove DATA_IN.  */

void
lto_data_in_delete (struct data_in *data_in)
{
  VEC_free (ld_plugin_symbol_resolution_t, heap, data_in->globals_resolution);
  lto_streamer_cache_delete (data_in->reader_cache);
  free (data_in->labels);
  free (data_in);
}
