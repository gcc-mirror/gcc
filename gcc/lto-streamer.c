/* Miscellaneous utilities for GIMPLE streaming.  Things that are used
   in both input and output are here.

   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Doug Kwan <dougkwan@google.com>

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
#include "flags.h"
#include "tree.h"
#include "gimple.h"
#include "tree-flow.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "vec.h"
#include "lto-streamer.h"

/* Statistics gathered during LTO, WPA and LTRANS.  */
struct lto_stats_d lto_stats;

/* LTO uses bitmaps with different life-times.  So use a seperate
   obstack for all LTO bitmaps.  */
static bitmap_obstack lto_obstack;
static bool lto_obstack_initialized;


/* Return a string representing LTO tag TAG.  */

const char *
lto_tag_name (enum LTO_tags tag)
{
  if (lto_tag_is_tree_code_p (tag))
    {
      /* For tags representing tree nodes, return the name of the
	 associated tree code.  */
      return tree_code_name[lto_tag_to_tree_code (tag)];
    }

  if (lto_tag_is_gimple_code_p (tag))
    {
      /* For tags representing gimple statements, return the name of
	 the associated gimple code.  */
      return gimple_code_name[lto_tag_to_gimple_code (tag)];
    }

  switch (tag)
    {
    case LTO_null:
      return "LTO_null";
    case LTO_bb0:
      return "LTO_bb0";
    case LTO_bb1:
      return "LTO_bb1";
    case LTO_eh_region:
      return "LTO_eh_region";
    case LTO_function:
      return "LTO_function";
    case LTO_eh_table:
      return "LTO_eh_table";
    case LTO_ert_cleanup:
      return "LTO_ert_cleanup";
    case LTO_ert_try:
      return "LTO_ert_try";
    case LTO_ert_allowed_exceptions:
      return "LTO_ert_allowed_exceptions";
    case LTO_ert_must_not_throw:
      return "LTO_ert_must_not_throw";
    case LTO_tree_pickle_reference:
      return "LTO_tree_pickle_reference";
    case LTO_field_decl_ref:
      return "LTO_field_decl_ref";
    case LTO_function_decl_ref:
      return "LTO_function_decl_ref";
    case LTO_label_decl_ref:
      return "LTO_label_decl_ref";
    case LTO_namespace_decl_ref:
      return "LTO_namespace_decl_ref";
    case LTO_result_decl_ref:
      return "LTO_result_decl_ref";
    case LTO_ssa_name_ref:
      return "LTO_ssa_name_ref";
    case LTO_type_decl_ref:
      return "LTO_type_decl_ref";
    case LTO_type_ref:
      return "LTO_type_ref";
    case LTO_global_decl_ref:
      return "LTO_global_decl_ref";
    default:
      return "LTO_UNKNOWN";
    }
}


/* Allocate a bitmap from heap.  Initializes the LTO obstack if necessary.  */

bitmap
lto_bitmap_alloc (void)
{
  if (!lto_obstack_initialized)
    {
      bitmap_obstack_initialize (&lto_obstack);
      lto_obstack_initialized = true;
    }
  return BITMAP_ALLOC (&lto_obstack);
}

/* Free bitmap B.  */

void
lto_bitmap_free (bitmap b)
{
  BITMAP_FREE (b);
}


/* Get a section name for a particular type or name.  The NAME field
   is only used if SECTION_TYPE is LTO_section_function_body or
   LTO_static_initializer.  For all others it is ignored.  The callee
   of this function is responcible to free the returned name.  */

char *
lto_get_section_name (int section_type, const char *name)
{
  switch (section_type)
    {
    case LTO_section_function_body:
      gcc_assert (name != NULL);
      if (name[0] == '*')
	name++;
      return concat (LTO_SECTION_NAME_PREFIX, name, NULL);

    case LTO_section_static_initializer:
      return concat (LTO_SECTION_NAME_PREFIX, ".statics", NULL);

    case LTO_section_symtab:
      return concat (LTO_SECTION_NAME_PREFIX, ".symtab", NULL);

    case LTO_section_decls:
      return concat (LTO_SECTION_NAME_PREFIX, ".decls", NULL);

    case LTO_section_cgraph:
      return concat (LTO_SECTION_NAME_PREFIX, ".cgraph", NULL);

    case LTO_section_jump_functions:
      return concat (LTO_SECTION_NAME_PREFIX, ".jmpfuncs", NULL);

    case LTO_section_ipa_pure_const:
      return concat (LTO_SECTION_NAME_PREFIX, ".pureconst", NULL);

    case LTO_section_ipa_reference:
      return concat (LTO_SECTION_NAME_PREFIX, ".reference", NULL);

    case LTO_section_wpa_fixup:
      return concat (LTO_SECTION_NAME_PREFIX, ".wpa_fixup", NULL);

    case LTO_section_opts:
      return concat (LTO_SECTION_NAME_PREFIX, ".opts", NULL);

    default:
      internal_error ("bytecode stream: unexpected LTO section %s", name);
    }
}


/* Show various memory usage statistics related to LTO.  */

void
print_lto_report (void)
{
  const char *s = (flag_lto) ? "LTO" : (flag_wpa) ? "WPA" : "LTRANS";
  unsigned i;

  fprintf (stderr, "%s statistics\n", s);
  fprintf (stderr, "[%s] # of input files: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s, lto_stats.num_input_files);

  fprintf (stderr, "[%s] # of input cgraph nodes: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	   lto_stats.num_input_cgraph_nodes);

  fprintf (stderr, "[%s] # of function bodies: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	   lto_stats.num_function_bodies);

  fprintf (stderr, "[%s] ", s);
  print_gimple_types_stats ();

  for (i = 0; i < NUM_TREE_CODES; i++)
    if (lto_stats.num_trees[i])
      fprintf (stderr, "[%s] # of '%s' objects read: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       tree_code_name[i], lto_stats.num_trees[i]);

  if (flag_lto)
    {
      fprintf (stderr, "[%s] Compression: "
	       HOST_WIDE_INT_PRINT_UNSIGNED " output bytes, "
	       HOST_WIDE_INT_PRINT_UNSIGNED " compressed bytes", s,
	       lto_stats.num_output_il_bytes,
	       lto_stats.num_compressed_il_bytes);
      if (lto_stats.num_output_il_bytes > 0)
	{
	  const float dividend = (float) lto_stats.num_compressed_il_bytes;
	  const float divisor = (float) lto_stats.num_output_il_bytes;
	  fprintf (stderr, " (ratio: %f)", dividend / divisor);
	}
      fprintf (stderr, "\n");
    }

  if (flag_wpa)
    {
      fprintf (stderr, "[%s] # of output files: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_output_files);

      fprintf (stderr, "[%s] # of output cgraph nodes: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_output_cgraph_nodes);

      fprintf (stderr, "[%s] # callgraph partitions: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_cgraph_partitions);

      fprintf (stderr, "[%s] Compression: "
	       HOST_WIDE_INT_PRINT_UNSIGNED " input bytes, "
	       HOST_WIDE_INT_PRINT_UNSIGNED " uncompressed bytes", s,
	       lto_stats.num_input_il_bytes,
	       lto_stats.num_uncompressed_il_bytes);
      if (lto_stats.num_input_il_bytes > 0)
	{
	  const float dividend = (float) lto_stats.num_uncompressed_il_bytes;
	  const float divisor = (float) lto_stats.num_input_il_bytes;
	  fprintf (stderr, " (ratio: %f)", dividend / divisor);
	}
      fprintf (stderr, "\n");
    }

  for (i = 0; i < LTO_N_SECTION_TYPES; i++)
    fprintf (stderr, "[%s] Size of mmap'd section %s: "
	     HOST_WIDE_INT_PRINT_UNSIGNED " bytes\n", s,
	     lto_section_name[i], lto_stats.section_size[i]);
}


/* Create a new bitpack.  */

struct bitpack_d *
bitpack_create (void)
{
  return XCNEW (struct bitpack_d);
}


/* Free the memory used by bitpack BP.  */

void
bitpack_delete (struct bitpack_d *bp)
{
  VEC_free (bitpack_word_t, heap, bp->values);
  free (bp);
}


/* Return an index to the word in bitpack BP that contains the
   next NBITS.  */

static inline unsigned
bp_get_next_word (struct bitpack_d *bp, unsigned nbits)
{
  unsigned last, ix;

  /* In principle, the next word to use is determined by the
     number of bits already processed in BP.  */
  ix = bp->num_bits / BITS_PER_BITPACK_WORD;

  /* All the encoded bit patterns in BP are contiguous, therefore if
     the next NBITS would straddle over two different words, move the
     index to the next word and update the number of encoded bits
     by adding up the hole of unused bits created by this move.  */
  bp->first_unused_bit %= BITS_PER_BITPACK_WORD;
  last = bp->first_unused_bit + nbits - 1;
  if (last >= BITS_PER_BITPACK_WORD)
    {
      ix++;
      bp->num_bits += (BITS_PER_BITPACK_WORD - bp->first_unused_bit);
      bp->first_unused_bit = 0;
    }

  return ix;
}


/* Pack NBITS of value VAL into bitpack BP.  */

void
bp_pack_value (struct bitpack_d *bp, bitpack_word_t val, unsigned nbits)
{
  unsigned ix;
  bitpack_word_t word;

  /* We cannot encode more bits than BITS_PER_BITPACK_WORD.  */
  gcc_assert (nbits > 0 && nbits <= BITS_PER_BITPACK_WORD);

  /* Compute which word will contain the next NBITS.  */
  ix = bp_get_next_word (bp, nbits);
  if (ix >= VEC_length (bitpack_word_t, bp->values))
    {
      /* If there is no room left in the last word of the values
	 array, add a new word.  Additionally, we should only
	 need to add a single word, since every pack operation cannot
	 use more bits than fit in a single word.  */
      gcc_assert (ix < VEC_length (bitpack_word_t, bp->values) + 1);
      VEC_safe_push (bitpack_word_t, heap, bp->values, 0);
    }

  /* Grab the last word to pack VAL into.  */
  word = VEC_index (bitpack_word_t, bp->values, ix);

  /* To fit VAL in WORD, we need to shift VAL to the left to
     skip the bottom BP->FIRST_UNUSED_BIT bits.  */
  gcc_assert (BITS_PER_BITPACK_WORD >= bp->first_unused_bit + nbits);
  val <<= bp->first_unused_bit;

  /* Update WORD with VAL.  */
  word |= val;

  /* Update BP.  */
  VEC_replace (bitpack_word_t, bp->values, ix, word);
  bp->num_bits += nbits;
  bp->first_unused_bit += nbits;
}


/* Unpack the next NBITS from bitpack BP.  */

bitpack_word_t
bp_unpack_value (struct bitpack_d *bp, unsigned nbits)
{
  bitpack_word_t val, word, mask;
  unsigned ix;

  /* We cannot decode more bits than BITS_PER_BITPACK_WORD.  */
  gcc_assert (nbits > 0 && nbits <= BITS_PER_BITPACK_WORD);

  /* Compute which word contains the next NBITS.  */
  ix = bp_get_next_word (bp, nbits);
  word = VEC_index (bitpack_word_t, bp->values, ix);

  /* Compute the mask to get NBITS from WORD.  */
  mask = (nbits == BITS_PER_BITPACK_WORD)
	 ? (bitpack_word_t) -1
	 : ((bitpack_word_t) 1 << nbits) - 1;

  /* Shift WORD to the right to skip over the bits already decoded
     in word.  */
  word >>= bp->first_unused_bit;

  /* Apply the mask to obtain the requested value.  */
  val = word & mask;

  /* Update BP->NUM_BITS for the next unpack operation.  */
  bp->num_bits += nbits;
  bp->first_unused_bit += nbits;

  return val;
}


/* Check that all the TS_* structures handled by the lto_output_* and
   lto_input_* routines are exactly ALL the structures defined in
   treestruct.def.  */

static void
check_handled_ts_structures (void)
{
  bool handled_p[LAST_TS_ENUM];
  unsigned i;

  memset (&handled_p, 0, sizeof (handled_p));

  /* These are the TS_* structures that are either handled or
     explicitly ignored by the streamer routines.  */
  handled_p[TS_BASE] = true;
  handled_p[TS_COMMON] = true;
  handled_p[TS_INT_CST] = true;
  handled_p[TS_REAL_CST] = true;
  handled_p[TS_FIXED_CST] = true;
  handled_p[TS_VECTOR] = true;
  handled_p[TS_STRING] = true;
  handled_p[TS_COMPLEX] = true;
  handled_p[TS_IDENTIFIER] = true;
  handled_p[TS_DECL_MINIMAL] = true;
  handled_p[TS_DECL_COMMON] = true;
  handled_p[TS_DECL_WRTL] = true;
  handled_p[TS_DECL_NON_COMMON] = true;
  handled_p[TS_DECL_WITH_VIS] = true;
  handled_p[TS_FIELD_DECL] = true;
  handled_p[TS_VAR_DECL] = true;
  handled_p[TS_PARM_DECL] = true;
  handled_p[TS_LABEL_DECL] = true;
  handled_p[TS_RESULT_DECL] = true;
  handled_p[TS_CONST_DECL] = true;
  handled_p[TS_TYPE_DECL] = true;
  handled_p[TS_FUNCTION_DECL] = true;
  handled_p[TS_TYPE] = true;
  handled_p[TS_LIST] = true;
  handled_p[TS_VEC] = true;
  handled_p[TS_EXP] = true;
  handled_p[TS_SSA_NAME] = true;
  handled_p[TS_BLOCK] = true;
  handled_p[TS_BINFO] = true;
  handled_p[TS_STATEMENT_LIST] = true;
  handled_p[TS_CONSTRUCTOR] = true;
  handled_p[TS_OMP_CLAUSE] = true;
  handled_p[TS_OPTIMIZATION] = true;
  handled_p[TS_TARGET_OPTION] = true;

  /* Anything not marked above will trigger the following assertion.
     If this assertion triggers, it means that there is a new TS_*
     structure that should be handled by the streamer.  */
  for (i = 0; i < LAST_TS_ENUM; i++)
    gcc_assert (handled_p[i]);
}


/* Helper for lto_streamer_cache_insert_1.  Add T to CACHE->NODES at
   slot IX.  Add OFFSET to CACHE->OFFSETS at slot IX.  */

static void
lto_streamer_cache_add_to_node_array (struct lto_streamer_cache_d *cache,
				      int ix, tree t, unsigned offset)
{
  gcc_assert (ix >= 0);

  /* Grow the array of nodes and offsets to accomodate T at IX.  */
  if (ix >= (int) VEC_length (tree, cache->nodes))
    {
      size_t sz = ix + (20 + ix) / 4;
      VEC_safe_grow_cleared (tree, gc, cache->nodes, sz);
      VEC_safe_grow_cleared (unsigned, heap, cache->offsets, sz);
    }

  VEC_replace (tree, cache->nodes, ix, t);
  VEC_replace (unsigned, cache->offsets, ix, offset);
}


/* Helper for lto_streamer_cache_insert and lto_streamer_cache_insert_at.
   CACHE, T, IX_P and OFFSET_P are as in lto_streamer_cache_insert.

   If INSERT_AT_NEXT_SLOT_P is true, T is inserted at the next available
   slot in the cache.  Otherwise, T is inserted at the position indicated
   in *IX_P.

   If T already existed in CACHE, return true.  Otherwise,
   return false.  */

static bool
lto_streamer_cache_insert_1 (struct lto_streamer_cache_d *cache,
			     tree t, int *ix_p, unsigned *offset_p,
			     bool insert_at_next_slot_p)
{
  void **slot;
  struct tree_int_map d_entry, *entry;
  int ix;
  unsigned offset;
  bool existed_p;

  gcc_assert (t);

  d_entry.base.from = t;
  slot = htab_find_slot (cache->node_map, &d_entry, INSERT);
  if (*slot == NULL)
    {
      /* Determine the next slot to use in the cache.  */
      if (insert_at_next_slot_p)
	ix = cache->next_slot++;
      else
	ix = *ix_p;

      entry = XCNEW (struct tree_int_map);
      entry->base.from = t;
      entry->to = (unsigned) ix;
      *slot = entry;

      /* If no offset was given, store the invalid offset -1.  */
      offset = (offset_p) ? *offset_p : (unsigned) -1;

      lto_streamer_cache_add_to_node_array (cache, ix, t, offset);

      /* Indicate that the item was not present in the cache.  */
      existed_p = false;
    }
  else
    {
      entry = (struct tree_int_map *) *slot;
      ix = (int) entry->to;
      offset = VEC_index (unsigned, cache->offsets, ix);

      if (!insert_at_next_slot_p && ix != *ix_p)
	{
	  /* If the caller wants to insert T at a specific slot
	     location, and ENTRY->TO does not match *IX_P, add T to
	     the requested location slot.  This situation arises when
	     streaming builtin functions.

	     For instance, on the writer side we could have two
	     FUNCTION_DECLS T1 and T2 that are represented by the same
	     builtin function.  The reader will only instantiate the
	     canonical builtin, but since T1 and T2 had been
	     originally stored in different cache slots (S1 and S2),
	     the reader must be able to find the canonical builtin
	     function at slots S1 and S2.  */
	  gcc_assert (lto_stream_as_builtin_p (t));
	  ix = *ix_p;

	  /* Since we are storing a builtin, the offset into the
	     stream is not necessary as we will not need to read
	     forward in the stream.  */
	  lto_streamer_cache_add_to_node_array (cache, ix, t, -1);
	}

      /* Indicate that T was already in the cache.  */
      existed_p = true;
    }

  if (ix_p)
    *ix_p = ix;

  if (offset_p)
    *offset_p = offset;

  return existed_p;
}


/* Insert tree node T in CACHE.  If T already existed in the cache
   return true.  Otherwise, return false.

   If IX_P is non-null, update it with the index into the cache where
   T has been stored.

   *OFFSET_P represents the offset in the stream where T is physically
   written out.  The first time T is added to the cache, *OFFSET_P is
   recorded in the cache together with T.  But if T already existed
   in the cache, *OFFSET_P is updated with the value that was recorded
   the first time T was added to the cache.

   If OFFSET_P is NULL, it is ignored.  */

bool
lto_streamer_cache_insert (struct lto_streamer_cache_d *cache, tree t,
			   int *ix_p, unsigned *offset_p)
{
  return lto_streamer_cache_insert_1 (cache, t, ix_p, offset_p, true);
}


/* Insert tree node T in CACHE at slot IX.  If T already
   existed in the cache return true.  Otherwise, return false.  */

bool
lto_streamer_cache_insert_at (struct lto_streamer_cache_d *cache,
			      tree t, int ix)
{
  return lto_streamer_cache_insert_1 (cache, t, &ix, NULL, false);
}


/* Return true if tree node T exists in CACHE.  If IX_P is
   not NULL, write to *IX_P the index into the cache where T is stored
   (-1 if T is not found).  */

bool
lto_streamer_cache_lookup (struct lto_streamer_cache_d *cache, tree t,
			   int *ix_p)
{
  void **slot;
  struct tree_int_map d_slot;
  bool retval;
  int ix;

  gcc_assert (t);

  d_slot.base.from = t;
  slot = htab_find_slot (cache->node_map, &d_slot, NO_INSERT);
  if (slot == NULL)
    {
      retval = false;
      ix = -1;
    }
  else
    {
      retval = true;
      ix = (int) ((struct tree_int_map *) *slot)->to;
    }

  if (ix_p)
    *ix_p = ix;

  return retval;
}


/* Return the tree node at slot IX in CACHE.  */

tree
lto_streamer_cache_get (struct lto_streamer_cache_d *cache, int ix)
{
  gcc_assert (cache);

  /* If the reader is requesting an index beyond the length of the
     cache, it will need to read ahead.  Return NULL_TREE to indicate
     that.  */
  if ((unsigned) ix >= VEC_length (tree, cache->nodes))
    return NULL_TREE;

  return VEC_index (tree, cache->nodes, (unsigned) ix);
}


/* Record NODE in COMMON_NODES if it is not NULL and is not already in
   SEEN_NODES.  */

static void
lto_record_common_node (tree *nodep, VEC(tree, heap) **common_nodes,
			struct pointer_set_t *seen_nodes)
{
  tree node = *nodep;

  if (node == NULL_TREE)
    return;

  if (TYPE_P (node))
    *nodep = node = gimple_register_type (node);

  /* Return if node is already seen.  */
  if (pointer_set_insert (seen_nodes, node))
    return;

  VEC_safe_push (tree, heap, *common_nodes, node);

  if (tree_node_can_be_shared (node))
    {
      if (POINTER_TYPE_P (node)
	  || TREE_CODE (node) == COMPLEX_TYPE
	  || TREE_CODE (node) == ARRAY_TYPE)
	lto_record_common_node (&TREE_TYPE (node), common_nodes, seen_nodes);
    }
}


/* Generate a vector of common nodes and make sure they are merged
   properly according to the the gimple type table.  */

static VEC(tree,heap) *
lto_get_common_nodes (void)
{
  unsigned i;
  VEC(tree,heap) *common_nodes = NULL;
  struct pointer_set_t *seen_nodes;

  /* The MAIN_IDENTIFIER_NODE is normally set up by the front-end, but the
     LTO back-end must agree. Currently, the only languages that set this
     use the name "main".  */
  if (main_identifier_node)
    {
      const char *main_name = IDENTIFIER_POINTER (main_identifier_node);
      gcc_assert (strcmp (main_name, "main") == 0);
    }
  else
    main_identifier_node = get_identifier ("main");

  gcc_assert (ptrdiff_type_node == integer_type_node);

  /* FIXME lto.  In the C++ front-end, fileptr_type_node is defined as a
     variant copy of of ptr_type_node, rather than ptr_node itself.  The
     distinction should only be relevant to the front-end, so we always
     use the C definition here in lto1.

     These should be assured in pass_ipa_free_lang_data.  */
  gcc_assert (fileptr_type_node == ptr_type_node);
  gcc_assert (TYPE_MAIN_VARIANT (fileptr_type_node) == ptr_type_node);

  seen_nodes = pointer_set_create ();

  /* Skip itk_char.  char_type_node is shared with the appropriately
     signed variant.  */
  for (i = itk_signed_char; i < itk_none; i++)
    lto_record_common_node (&integer_types[i], &common_nodes, seen_nodes);

  for (i = 0; i < TYPE_KIND_LAST; i++)
    lto_record_common_node (&sizetype_tab[i], &common_nodes, seen_nodes);

  for (i = 0; i < TI_MAX; i++)
    lto_record_common_node (&global_trees[i], &common_nodes, seen_nodes);

  pointer_set_destroy (seen_nodes);

  return common_nodes;
}


/* Assign an index to tree node T and enter it in the streamer cache
   CACHE.  */

static void
preload_common_node (struct lto_streamer_cache_d *cache, tree t)
{
  gcc_assert (t);

  lto_streamer_cache_insert (cache, t, NULL, NULL);

 /* The FIELD_DECLs of structures should be shared, so that every
    COMPONENT_REF uses the same tree node when referencing a field.
    Pointer equality between FIELD_DECLs is used by the alias
    machinery to compute overlapping memory references (See
    nonoverlapping_component_refs_p).  */
 if (TREE_CODE (t) == RECORD_TYPE)
   {
     tree f;

     for (f = TYPE_FIELDS (t); f; f = TREE_CHAIN (f))
       preload_common_node (cache, f);
   }
}


/* Create a cache of pickled nodes.  */

struct lto_streamer_cache_d *
lto_streamer_cache_create (void)
{
  struct lto_streamer_cache_d *cache;
  VEC(tree, heap) *common_nodes;
  unsigned i;
  tree node;

  cache = XCNEW (struct lto_streamer_cache_d);

  cache->node_map = htab_create (101, tree_int_map_hash, tree_int_map_eq, NULL);

  /* Load all the well-known tree nodes that are always created by
     the compiler on startup.  This prevents writing them out
     unnecessarily.  */
  common_nodes = lto_get_common_nodes ();

  for (i = 0; VEC_iterate (tree, common_nodes, i, node); i++)
    preload_common_node (cache, node);

  VEC_free(tree, heap, common_nodes);

  return cache;
}


/* Delete the streamer cache C.  */

void
lto_streamer_cache_delete (struct lto_streamer_cache_d *c)
{
  if (c == NULL)
    return;

  htab_delete (c->node_map);
  VEC_free (tree, gc, c->nodes);
  VEC_free (unsigned, heap, c->offsets);
  free (c);
}


/* Initialization common to the LTO reader and writer.  */

void
lto_streamer_init (void)
{
  /* Check that all the TS_* handled by the reader and writer routines
     match exactly the structures defined in treestruct.def.  When a
     new TS_* astructure is added, the streamer should be updated to
     handle it.  */
  check_handled_ts_structures ();
}


/* Gate function for all LTO streaming passes.  */

bool
gate_lto_out (void)
{
  return ((flag_generate_lto || in_lto_p)
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}


#ifdef LTO_STREAMER_DEBUG
/* Add a mapping between T and ORIG_T, which is the numeric value of
   the original address of T as it was seen by the LTO writer.  This
   mapping is useful when debugging streaming problems.  A debugging
   session can be started on both reader and writer using ORIG_T
   as a breakpoint value in both sessions.

   Note that this mapping is transient and only valid while T is
   being reconstructed.  Once T is fully built, the mapping is
   removed.  */

void
lto_orig_address_map (tree t, intptr_t orig_t)
{
  /* FIXME lto.  Using the annotation field is quite hacky as it relies
     on the GC not running while T is being rematerialized.  It would
     be cleaner to use a hash table here.  */
  t->base.ann = (union tree_ann_d *) orig_t;
}


/* Get the original address of T as it was seen by the writer.  This
   is only valid while T is being reconstructed.  */

intptr_t
lto_orig_address_get (tree t)
{
  return (intptr_t) t->base.ann;
}


/* Clear the mapping of T to its original address.  */

void
lto_orig_address_remove (tree t)
{
  t->base.ann = NULL;
}
#endif


/* Check that the version MAJOR.MINOR is the correct version number.  */

void
lto_check_version (int major, int minor)
{
  if (major != LTO_major_version || minor != LTO_minor_version)
    fatal_error ("bytecode stream generated with LTO version %d.%d instead "
	         "of the expected %d.%d",
		 major, minor,
		 LTO_major_version, LTO_minor_version);
}
