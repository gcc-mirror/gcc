/* Miscellaneous utilities for GIMPLE streaming.  Things that are used
   in both input and output are here.

   Copyright (C) 2009-2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "toplev.h"
#include "lto-section-names.h"

/* Statistics gathered during LTO, WPA and LTRANS.  */
struct lto_stats_d lto_stats;

/* LTO uses bitmaps with different life-times.  So use a separate
   obstack for all LTO bitmaps.  */
static bitmap_obstack lto_obstack;
static bool lto_obstack_initialized;

const char *section_name_prefix = LTO_SECTION_NAME_PREFIX;
/* Set when streaming LTO for offloading compiler.  */
bool lto_stream_offload_p;

FILE *streamer_dump_file;

/* Return a string representing LTO tag TAG.  */

const char *
lto_tag_name (enum LTO_tags tag)
{
  if (lto_tag_is_tree_code_p (tag))
    {
      /* For tags representing tree nodes, return the name of the
	 associated tree code.  */
      return get_tree_code_name (lto_tag_to_tree_code (tag));
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
     and merging should be ok here. */
  if (section_type == LTO_section_opts)
    strcpy (post, "");
  else if (f != NULL) 
    sprintf (post, "." HOST_WIDE_INT_PRINT_HEX_PURE, f->id);
  else
    sprintf (post, "." HOST_WIDE_INT_PRINT_HEX_PURE, get_random_seed (false)); 
  return concat (section_name_prefix, sep, add, post, NULL);
}


/* Show various memory usage statistics related to LTO.  */

void
print_lto_report (const char *s)
{
  unsigned i;

  fprintf (stderr, "[%s] # of input files: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s, lto_stats.num_input_files);

  fprintf (stderr, "[%s] # of input cgraph nodes: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	   lto_stats.num_input_cgraph_nodes);

  fprintf (stderr, "[%s] # of function bodies: "
	   HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	   lto_stats.num_function_bodies);

  for (i = 0; i < NUM_TREE_CODES; i++)
    if (lto_stats.num_trees[i])
      fprintf (stderr, "[%s] # of '%s' objects read: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       get_tree_code_name ((enum tree_code) i), lto_stats.num_trees[i]);

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

      fprintf (stderr, "[%s] # of output symtab nodes: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_output_symtab_nodes);

      fprintf (stderr, "[%s] # of output tree pickle references: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_pickle_refs_output);
      fprintf (stderr, "[%s] # of output tree bodies: "
	       HOST_WIDE_INT_PRINT_UNSIGNED "\n", s,
	       lto_stats.num_tree_bodies_output);

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

/* Initialization common to the LTO reader and writer.  */

void
lto_streamer_init (void)
{
  /* Check that all the TS_* handled by the reader and writer routines
     match exactly the structures defined in treestruct.def.  When a
     new TS_* astructure is added, the streamer should be updated to
     handle it.  */
  if (flag_checking)
    streamer_check_handled_ts_structures ();
}


/* Gate function for all LTO streaming passes.  */

bool
gate_lto_out (void)
{
  return ((flag_generate_lto || flag_generate_offload || in_lto_p)
	  /* Don't bother doing anything if the program has errors.  */
	  && !seen_error ());
}

/* Check that the version MAJOR.MINOR is the correct version number.  */

void
lto_check_version (int major, int minor, const char *file_name)
{
  if (major != LTO_major_version || minor != LTO_minor_version)
    fatal_error (input_location,
		 "bytecode stream in file %qs generated with LTO version "
		 "%d.%d instead of the expected %d.%d",
		 file_name,
		 major, minor,
		 LTO_major_version, LTO_minor_version);
}


/* Initialize all the streamer hooks used for streaming GIMPLE.  */

void
lto_streamer_hooks_init (void)
{
  streamer_hooks_init ();
  streamer_hooks.write_tree = lto_output_tree;
  streamer_hooks.read_tree = lto_input_tree;
  streamer_hooks.input_location = lto_input_location;
  streamer_hooks.output_location = lto_output_location;
}
