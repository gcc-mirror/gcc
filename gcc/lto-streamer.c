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
#include "diagnostic-core.h"
#include "bitmap.h"
#include "vec.h"
#include "tree-streamer.h"
#include "lto-streamer.h"
#include "streamer-hooks.h"

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
   is only used if SECTION_TYPE is LTO_section_function_body. For all
   others it is ignored.  The callee of this function is responsible
   to free the returned name.  */

char *
lto_get_section_name (int section_type, const char *name, struct lto_file_decl_data *f)
{
  const char *add;
  char post[32];
  const char *sep;

  if (section_type == LTO_section_function_body)
    {
      gcc_assert (name != NULL);
      if (name[0] == '*')
	name++;
      add = name;
      sep = "";
    }
  else if (section_type < LTO_N_SECTION_TYPES)
    {
      add = lto_section_name[section_type];
      sep = ".";
    }
  else
    internal_error ("bytecode stream: unexpected LTO section %s", name);

  /* Make the section name unique so that ld -r combining sections
     doesn't confuse the reader with merged sections.

     For options don't add a ID, the option reader cannot deal with them
     and merging should be ok here.

     XXX: use crc64 to minimize collisions? */
  if (section_type == LTO_section_opts)
    strcpy (post, "");
  else
    sprintf (post, ".%x", f ? f->id : crc32_string(0, get_random_seed (false)));
  return concat (LTO_SECTION_NAME_PREFIX, sep, add, post, NULL);
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


/* Record NODE in CACHE.  */

static void
lto_record_common_node (struct lto_streamer_cache_d *cache, tree node)
{
  /* We have to make sure to fill exactly the same number of
     elements for all frontends.  That can include NULL trees.
     As our hash table can't deal with zero entries we'll simply stream
     a random other tree.  A NULL tree never will be looked up so it
     doesn't matter which tree we replace it with, just to be sure
     use error_mark_node.  */
  if (!node)
    node = error_mark_node;

  lto_streamer_cache_append (cache, node);

  if (POINTER_TYPE_P (node)
      || TREE_CODE (node) == COMPLEX_TYPE
      || TREE_CODE (node) == ARRAY_TYPE)
    lto_record_common_node (cache, TREE_TYPE (node));
  else if (TREE_CODE (node) == RECORD_TYPE)
    {
      /* The FIELD_DECLs of structures should be shared, so that every
	 COMPONENT_REF uses the same tree node when referencing a field.
	 Pointer equality between FIELD_DECLs is used by the alias
	 machinery to compute overlapping memory references (See
	 nonoverlapping_component_refs_p).  */
      tree f;
      for (f = TYPE_FIELDS (node); f; f = TREE_CHAIN (f))
	lto_record_common_node (cache, f);
    }
}

/* Preload common nodes into CACHE and make sure they are merged
   properly according to the gimple type table.  */

static void
lto_preload_common_nodes (struct lto_streamer_cache_d *cache)
{
  unsigned i;

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

  for (i = 0; i < itk_none; i++)
    /* Skip itk_char.  char_type_node is dependent on -f[un]signed-char.  */
    if (i != itk_char)
      lto_record_common_node (cache, integer_types[i]);

  for (i = 0; i < TYPE_KIND_LAST; i++)
    lto_record_common_node (cache, sizetype_tab[i]);

  for (i = 0; i < TI_MAX; i++)
    /* Skip boolean type and constants, they are frontend dependent.  */
    if (i != TI_BOOLEAN_TYPE
	&& i != TI_BOOLEAN_FALSE
	&& i != TI_BOOLEAN_TRUE)
      lto_record_common_node (cache, global_trees[i]);
}


#ifdef LTO_STREAMER_DEBUG
static htab_t tree_htab;

struct tree_hash_entry
{
  tree key;
  intptr_t value;
};

static hashval_t
hash_tree (const void *p)
{
  const struct tree_hash_entry *e = (const struct tree_hash_entry *) p;
  return htab_hash_pointer (e->key);
}

static int
eq_tree (const void *p1, const void *p2)
{
  const struct tree_hash_entry *e1 = (const struct tree_hash_entry *) p1;
  const struct tree_hash_entry *e2 = (const struct tree_hash_entry *) p2;
  return (e1->key == e2->key);
}
#endif

/* Initialization common to the LTO reader and writer.  */

void
lto_streamer_init (void)
{
  /* Check that all the TS_* handled by the reader and writer routines
     match exactly the structures defined in treestruct.def.  When a
     new TS_* astructure is added, the streamer should be updated to
     handle it.  */
  check_handled_ts_structures ();

#ifdef LTO_STREAMER_DEBUG
  tree_htab = htab_create (31, hash_tree, eq_tree, NULL);
#endif
}


/* Gate function for all LTO streaming passes.  */

bool
gate_lto_out (void)
{
  return ((flag_generate_lto || in_lto_p)
	  /* Don't bother doing anything if the program has errors.  */
	  && !seen_error ());
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
  struct tree_hash_entry ent;
  struct tree_hash_entry **slot;

  ent.key = t;
  ent.value = orig_t;
  slot
    = (struct tree_hash_entry **) htab_find_slot (tree_htab, &ent, INSERT);
  gcc_assert (!*slot);
  *slot = XNEW (struct tree_hash_entry);
  **slot = ent;
}


/* Get the original address of T as it was seen by the writer.  This
   is only valid while T is being reconstructed.  */

intptr_t
lto_orig_address_get (tree t)
{
  struct tree_hash_entry ent;
  struct tree_hash_entry **slot;

  ent.key = t;
  slot
    = (struct tree_hash_entry **) htab_find_slot (tree_htab, &ent, NO_INSERT);
  return (slot ? (*slot)->value : 0);
}


/* Clear the mapping of T to its original address.  */

void
lto_orig_address_remove (tree t)
{
  struct tree_hash_entry ent;
  struct tree_hash_entry **slot;

  ent.key = t;
  slot
    = (struct tree_hash_entry **) htab_find_slot (tree_htab, &ent, NO_INSERT);
  gcc_assert (slot);
  free (*slot);
  htab_clear_slot (tree_htab, (PTR *)slot);
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


/* Initialize all the streamer hooks used for streaming GIMPLE.  */

void
lto_streamer_hooks_init (void)
{
  streamer_hooks_init ();
  streamer_hooks.name = "gimple";
  streamer_hooks.preload_common_nodes = lto_preload_common_nodes;
  streamer_hooks.is_streamable = lto_is_streamable;
  streamer_hooks.write_tree = lto_streamer_write_tree;
  streamer_hooks.read_tree = lto_streamer_read_tree;
}
