/* Read and write coverage files, and associated functionality.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Further mangled by Nathan Sidwell, CodeSourcery

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


#define GCOV_LINKAGE

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "basic-block.h"
#include "toplev.h"
#include "tm_p.h"
#include "ggc.h"
#include "coverage.h"
#include "langhooks.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "filenames.h"

#include "gcov-io.h"
#include "gcov-io.c"

struct GTY((chain_next ("%h.next"))) function_list
{
  struct function_list *next;	 /* next function */
  unsigned ident;		 /* function ident */
  unsigned lineno_checksum;	 /* function lineno checksum */
  unsigned cfg_checksum;	 /* function cfg checksum */
  tree fn_decl;			 /* the function decl */
  tree ctr_vars[GCOV_COUNTERS];	 /* counter variables.  */
};

/* Counts information for a function.  */
typedef struct counts_entry
{
  /* We hash by  */
  unsigned ident;
  unsigned ctr;

  /* Store  */
  unsigned lineno_checksum;
  unsigned cfg_checksum;
  gcov_type *counts;
  struct gcov_ctr_summary summary;
} counts_entry_t;

static GTY(()) struct function_list *functions_head = 0;
static struct function_list **functions_tail = &functions_head;
static unsigned no_coverage = 0;

/* Cumulative counter information for whole program.  */
static unsigned prg_ctr_mask; /* Mask of counter types generated.  */

/* Counter information for current function.  */
static unsigned fn_ctr_mask; /* Mask of counters used.  */
static GTY(()) tree fn_v_ctrs[GCOV_COUNTERS];   /* counter variables.  */
static unsigned fn_n_ctrs[GCOV_COUNTERS]; /* Counters allocated.  */
static unsigned fn_b_ctrs[GCOV_COUNTERS]; /* Allocation base.  */

/* Name of the output file for coverage output file.  */
static char *bbg_file_name;
static unsigned bbg_file_opened;
static int bbg_function_announced;

/* Name of the count data file.  */
static char *da_file_name;

/* Hash table of count data.  */
static htab_t counts_hash = NULL;

/* The names of merge functions for counters.  */
static const char *const ctr_merge_functions[GCOV_COUNTERS] = GCOV_MERGE_FUNCTIONS;
static const char *const ctr_names[GCOV_COUNTERS] = GCOV_COUNTER_NAMES;

/* Forward declarations.  */
static hashval_t htab_counts_entry_hash (const void *);
static int htab_counts_entry_eq (const void *, const void *);
static void htab_counts_entry_del (void *);
static void read_counts_file (void);
static tree build_var (tree, tree, int);
static void build_fn_info_type (tree, unsigned, tree);
static tree build_fn_info (const struct function_list *, tree, tree);
static void build_info_type (tree, unsigned, tree);
static tree build_info (tree, tree, tree, unsigned);
static void create_coverage (void);

/* Return the type node for gcov_type.  */

tree
get_gcov_type (void)
{
  return lang_hooks.types.type_for_size (GCOV_TYPE_SIZE, false);
}

/* Return the type node for gcov_unsigned_t.  */

static tree
get_gcov_unsigned_t (void)
{
  return lang_hooks.types.type_for_size (32, true);
}

static hashval_t
htab_counts_entry_hash (const void *of)
{
  const counts_entry_t *const entry = (const counts_entry_t *) of;

  return entry->ident * GCOV_COUNTERS + entry->ctr;
}

static int
htab_counts_entry_eq (const void *of1, const void *of2)
{
  const counts_entry_t *const entry1 = (const counts_entry_t *) of1;
  const counts_entry_t *const entry2 = (const counts_entry_t *) of2;

  return entry1->ident == entry2->ident && entry1->ctr == entry2->ctr;
}

static void
htab_counts_entry_del (void *of)
{
  counts_entry_t *const entry = (counts_entry_t *) of;

  free (entry->counts);
  free (entry);
}

/* Read in the counts file, if available.  */

static void
read_counts_file (void)
{
  gcov_unsigned_t fn_ident = 0;
  struct gcov_summary summary;
  unsigned new_summary = 1;
  gcov_unsigned_t tag;
  int is_error = 0;
  unsigned lineno_checksum = 0;
  unsigned cfg_checksum = 0;

  if (!gcov_open (da_file_name, 1))
    return;

  if (!gcov_magic (gcov_read_unsigned (), GCOV_DATA_MAGIC))
    {
      warning (0, "%qs is not a gcov data file", da_file_name);
      gcov_close ();
      return;
    }
  else if ((tag = gcov_read_unsigned ()) != GCOV_VERSION)
    {
      char v[4], e[4];

      GCOV_UNSIGNED2STRING (v, tag);
      GCOV_UNSIGNED2STRING (e, GCOV_VERSION);

      warning (0, "%qs is version %q.*s, expected version %q.*s",
 	       da_file_name, 4, v, 4, e);
      gcov_close ();
      return;
    }

  /* Read and discard the stamp.  */
  gcov_read_unsigned ();

  counts_hash = htab_create (10,
			     htab_counts_entry_hash, htab_counts_entry_eq,
			     htab_counts_entry_del);
  while ((tag = gcov_read_unsigned ()))
    {
      gcov_unsigned_t length;
      gcov_position_t offset;

      length = gcov_read_unsigned ();
      offset = gcov_position ();
      if (tag == GCOV_TAG_FUNCTION)
	{
	  if (length)
	    {
	      fn_ident = gcov_read_unsigned ();
	      lineno_checksum = gcov_read_unsigned ();
	      cfg_checksum = gcov_read_unsigned ();
	    }
	  else
	    fn_ident = lineno_checksum = cfg_checksum = 0;
	  new_summary = 1;
	}
      else if (tag == GCOV_TAG_PROGRAM_SUMMARY)
	{
	  struct gcov_summary sum;
	  unsigned ix;

	  if (new_summary)
	    memset (&summary, 0, sizeof (summary));

	  gcov_read_summary (&sum);
	  for (ix = 0; ix != GCOV_COUNTERS_SUMMABLE; ix++)
	    {
	      summary.ctrs[ix].runs += sum.ctrs[ix].runs;
	      summary.ctrs[ix].sum_all += sum.ctrs[ix].sum_all;
	      if (summary.ctrs[ix].run_max < sum.ctrs[ix].run_max)
		summary.ctrs[ix].run_max = sum.ctrs[ix].run_max;
	      summary.ctrs[ix].sum_max += sum.ctrs[ix].sum_max;
	    }
	  new_summary = 0;
	}
      else if (GCOV_TAG_IS_COUNTER (tag) && fn_ident)
	{
	  counts_entry_t **slot, *entry, elt;
	  unsigned n_counts = GCOV_TAG_COUNTER_NUM (length);
	  unsigned ix;

	  elt.ident = fn_ident;
	  elt.ctr = GCOV_COUNTER_FOR_TAG (tag);

	  slot = (counts_entry_t **) htab_find_slot
	    (counts_hash, &elt, INSERT);
	  entry = *slot;
	  if (!entry)
	    {
	      *slot = entry = XCNEW (counts_entry_t);
	      entry->ident = fn_ident;
	      entry->ctr = elt.ctr;
	      entry->lineno_checksum = lineno_checksum;
	      entry->cfg_checksum = cfg_checksum;
	      entry->summary = summary.ctrs[elt.ctr];
	      entry->summary.num = n_counts;
	      entry->counts = XCNEWVEC (gcov_type, n_counts);
	    }
	  else if (entry->lineno_checksum != lineno_checksum
		   || entry->cfg_checksum != cfg_checksum)
	    {
	      error ("Profile data for function %u is corrupted", fn_ident);
	      error ("checksum is (%x,%x) instead of (%x,%x)",
		     entry->lineno_checksum, entry->cfg_checksum,
		     lineno_checksum, cfg_checksum);
	      htab_delete (counts_hash);
	      break;
	    }
	  else if (entry->summary.num != n_counts)
	    {
	      error ("Profile data for function %u is corrupted", fn_ident);
	      error ("number of counters is %d instead of %d", entry->summary.num, n_counts);
	      htab_delete (counts_hash);
	      break;
	    }
	  else if (elt.ctr >= GCOV_COUNTERS_SUMMABLE)
	    {
	      error ("cannot merge separate %s counters for function %u",
		     ctr_names[elt.ctr], fn_ident);
	      goto skip_merge;
	    }
	  else
	    {
	      entry->summary.runs += summary.ctrs[elt.ctr].runs;
	      entry->summary.sum_all += summary.ctrs[elt.ctr].sum_all;
	      if (entry->summary.run_max < summary.ctrs[elt.ctr].run_max)
		entry->summary.run_max = summary.ctrs[elt.ctr].run_max;
	      entry->summary.sum_max += summary.ctrs[elt.ctr].sum_max;
	    }
	  for (ix = 0; ix != n_counts; ix++)
	    entry->counts[ix] += gcov_read_counter ();
	skip_merge:;
	}
      gcov_sync (offset, length);
      if ((is_error = gcov_is_error ()))
	{
	  error (is_error < 0 ? "%qs has overflowed" : "%qs is corrupted",
		 da_file_name);
	  htab_delete (counts_hash);
	  break;
	}
    }

  gcov_close ();
}

/* Returns the counters for a particular tag.  */

gcov_type *
get_coverage_counts (unsigned counter, unsigned expected,
                     unsigned cfg_checksum, unsigned lineno_checksum,
		     const struct gcov_ctr_summary **summary)
{
  counts_entry_t *entry, elt;

  /* No hash table, no counts.  */
  if (!counts_hash)
    {
      static int warned = 0;

      if (!warned++)
	inform (input_location, (flag_guess_branch_prob
		 ? "file %s not found, execution counts estimated"
		 : "file %s not found, execution counts assumed to be zero"),
		da_file_name);
      return NULL;
    }

  elt.ident = current_function_funcdef_no + 1;
  elt.ctr = counter;
  entry = (counts_entry_t *) htab_find (counts_hash, &elt);
  if (!entry || !entry->summary.num)
    /* The function was not emitted, or is weak and not chosen in the
       final executable.  Silently fail, because there's nothing we
       can do about it.  */
    return NULL;
  
  if (entry->cfg_checksum != cfg_checksum
      || entry->summary.num != expected)
    {
      static int warned = 0;
      bool warning_printed = false;
      tree id = DECL_ASSEMBLER_NAME (current_function_decl);

      warning_printed =
	warning_at (input_location, OPT_Wcoverage_mismatch,
		    "the control flow of function %qE does not match "
		    "its profile data (counter %qs)", id, ctr_names[counter]);
      if (warning_printed)
	{
	 inform (input_location, "use -Wno-error=coverage-mismatch to tolerate "
	 	 "the mismatch but performance may drop if the function is hot");
	  
	  if (!seen_error ()
	      && !warned++)
	    {
	      inform (input_location, "coverage mismatch ignored");
	      inform (input_location, flag_guess_branch_prob
		      ? G_("execution counts estimated")
		      : G_("execution counts assumed to be zero"));
	      if (!flag_guess_branch_prob)
		inform (input_location,
			"this can result in poorly optimized code");
	    }
	}

      return NULL;
    }
  else if (entry->lineno_checksum != lineno_checksum)
    {
      warning (0, "source location for function %qE have changed,"
	       " the profile data may be out of date",
	       DECL_ASSEMBLER_NAME (current_function_decl));
    }

  if (summary)
    *summary = &entry->summary;

  return entry->counts;
}

/* Allocate NUM counters of type COUNTER. Returns nonzero if the
   allocation succeeded.  */

int
coverage_counter_alloc (unsigned counter, unsigned num)
{
  if (no_coverage)
    return 0;

  if (!num)
    return 1;

  if (!fn_v_ctrs[counter])
    {
      tree array_type = build_array_type (get_gcov_type (), NULL_TREE);

      fn_v_ctrs[counter]
	= build_var (current_function_decl, array_type, counter);
    }

  fn_b_ctrs[counter] = fn_n_ctrs[counter];
  fn_n_ctrs[counter] += num;
  
  fn_ctr_mask |= 1 << counter;
  return 1;
}

/* Generate a tree to access COUNTER NO.  */

tree
tree_coverage_counter_ref (unsigned counter, unsigned no)
{
  tree gcov_type_node = get_gcov_type ();

  gcc_assert (no < fn_n_ctrs[counter] - fn_b_ctrs[counter]);

  no += fn_b_ctrs[counter];
  
  /* "no" here is an array index, scaled to bytes later.  */
  return build4 (ARRAY_REF, gcov_type_node, fn_v_ctrs[counter],
		 build_int_cst (integer_type_node, no), NULL, NULL);
}

/* Generate a tree to access the address of COUNTER NO.  */

tree
tree_coverage_counter_addr (unsigned counter, unsigned no)
{
  tree gcov_type_node = get_gcov_type ();

  gcc_assert (no < fn_n_ctrs[counter] - fn_b_ctrs[counter]);
  no += fn_b_ctrs[counter];

  /* "no" here is an array index, scaled to bytes later.  */
  return build_fold_addr_expr (build4 (ARRAY_REF, gcov_type_node,
				       fn_v_ctrs[counter],
				       build_int_cst (integer_type_node, no),
				       NULL, NULL));
}


/* Generate a checksum for a string.  CHKSUM is the current
   checksum.  */

static unsigned
coverage_checksum_string (unsigned chksum, const char *string)
{
  int i;
  char *dup = NULL;

  /* Look for everything that looks if it were produced by
     get_file_function_name and zero out the second part
     that may result from flag_random_seed.  This is not critical
     as the checksums are used only for sanity checking.  */
  for (i = 0; string[i]; i++)
    {
      int offset = 0;
      if (!strncmp (string + i, "_GLOBAL__N_", 11))
      offset = 11;
      if (!strncmp (string + i, "_GLOBAL__", 9))
      offset = 9;

      /* C++ namespaces do have scheme:
         _GLOBAL__N_<filename>_<wrongmagicnumber>_<magicnumber>functionname
       since filename might contain extra underscores there seems
       to be no better chance then walk all possible offsets looking
       for magicnumber.  */
      if (offset)
	{
	  for (i = i + offset; string[i]; i++)
	    if (string[i]=='_')
	      {
		int y;

		for (y = 1; y < 9; y++)
		  if (!(string[i + y] >= '0' && string[i + y] <= '9')
		      && !(string[i + y] >= 'A' && string[i + y] <= 'F'))
		    break;
		if (y != 9 || string[i + 9] != '_')
		  continue;
		for (y = 10; y < 18; y++)
		  if (!(string[i + y] >= '0' && string[i + y] <= '9')
		      && !(string[i + y] >= 'A' && string[i + y] <= 'F'))
		    break;
		if (y != 18)
		  continue;
		if (!dup)
		  string = dup = xstrdup (string);
		for (y = 10; y < 18; y++)
		  dup[i + y] = '0';
	      }
	  break;
	}
    }

  chksum = crc32_string (chksum, string);
  free (dup);

  return chksum;
}

/* Compute checksum for the current function.  We generate a CRC32.  */

unsigned
coverage_compute_lineno_checksum (void)
{
  expanded_location xloc
    = expand_location (DECL_SOURCE_LOCATION (current_function_decl));
  unsigned chksum = xloc.line;

  chksum = coverage_checksum_string (chksum, xloc.file);
  chksum = coverage_checksum_string
    (chksum, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl)));

  return chksum;
}

/* Compute cfg checksum for the current function.
   The checksum is calculated carefully so that
   source code changes that doesn't affect the control flow graph
   won't change the checksum.
   This is to make the profile data useable across source code change.
   The downside of this is that the compiler may use potentially
   wrong profile data - that the source code change has non-trivial impact
   on the validity of profile data (e.g. the reversed condition)
   but the compiler won't detect the change and use the wrong profile data.  */

unsigned
coverage_compute_cfg_checksum (void)
{
  basic_block bb;
  unsigned chksum = n_basic_blocks;

  FOR_EACH_BB (bb)
    {
      edge e;
      edge_iterator ei;
      chksum = crc32_byte (chksum, bb->index);
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
          chksum = crc32_byte (chksum, e->dest->index);
        }
    }

  return chksum;
}

/* Begin output to the graph file for the current function.
   Opens the output file, if not already done. Writes the
   function header, if not already done. Returns nonzero if data
   should be output.  */

int
coverage_begin_output (unsigned lineno_checksum, unsigned cfg_checksum)
{
  /* We don't need to output .gcno file unless we're under -ftest-coverage
     (e.g. -fprofile-arcs/generate/use don't need .gcno to work). */
  if (no_coverage || !flag_test_coverage || flag_compare_debug)
    return 0;

  if (!bbg_function_announced)
    {
      expanded_location xloc
	= expand_location (DECL_SOURCE_LOCATION (current_function_decl));
      unsigned long offset;

      if (!bbg_file_opened)
	{
	  if (!gcov_open (bbg_file_name, -1))
	    error ("cannot open %s", bbg_file_name);
	  else
	    {
	      gcov_write_unsigned (GCOV_NOTE_MAGIC);
	      gcov_write_unsigned (GCOV_VERSION);
	      gcov_write_unsigned (local_tick);
	    }
	  bbg_file_opened = 1;
	}


      /* Announce function */
      offset = gcov_write_tag (GCOV_TAG_FUNCTION);
      gcov_write_unsigned (current_function_funcdef_no + 1);
      gcov_write_unsigned (lineno_checksum);
      gcov_write_unsigned (cfg_checksum);
      gcov_write_string (IDENTIFIER_POINTER
                         (DECL_ASSEMBLER_NAME (current_function_decl)));
      gcov_write_string (xloc.file);
      gcov_write_unsigned (xloc.line);
      gcov_write_length (offset);

      bbg_function_announced = 1;
    }
  return !gcov_is_error ();
}

/* Finish coverage data for the current function. Verify no output
   error has occurred.  Save function coverage counts.  */

void
coverage_end_function (unsigned lineno_checksum, unsigned cfg_checksum)
{
  unsigned i;

  if (bbg_file_opened > 1 && gcov_is_error ())
    {
      warning (0, "error writing %qs", bbg_file_name);
      bbg_file_opened = -1;
    }

  if (fn_ctr_mask)
    {
      struct function_list *item;

      item = ggc_alloc_function_list ();

      item->next = 0;
      item->ident = current_function_funcdef_no + 1;
      item->lineno_checksum = lineno_checksum;
      item->cfg_checksum = cfg_checksum;
      item->fn_decl = current_function_decl;
      for (i = 0; i != GCOV_COUNTERS; i++)
	{
	  tree var = fn_v_ctrs[i];
	  
	  item->ctr_vars[i] = var;
	  if (var)
	    {
	      tree array_type = build_index_type (size_int (fn_n_ctrs[i] - 1));
	      array_type = build_array_type (get_gcov_type (), array_type);
	      TREE_TYPE (var) = array_type;
	      DECL_SIZE (var) = TYPE_SIZE (array_type);
	      DECL_SIZE_UNIT (var) = TYPE_SIZE_UNIT (array_type);
	      varpool_finalize_decl (var);
	    }
	  fn_b_ctrs[i] = fn_n_ctrs[i] = 0;
	  fn_v_ctrs[i] = NULL_TREE;
	}
      prg_ctr_mask |= fn_ctr_mask;
      fn_ctr_mask = 0;
      /* If the function is extern (i.e. extern inline), then we won't
	 be outputting it, so don't chain it onto the function list.  */
      if (!DECL_EXTERNAL (item->fn_decl))
	{
	  *functions_tail = item;
	  functions_tail = &item->next;
	}
    }
  bbg_function_announced = 0;
}

/* Build a coverage variable of TYPE for function FN_DECL.  If COUNTER
   >= 0 it is a counter array, otherwise it is the function structure.  */

static tree
build_var (tree fn_decl, tree type, int counter)
{
  tree var = build_decl (BUILTINS_LOCATION, VAR_DECL, NULL_TREE, type);
  tree fn_name = DECL_ASSEMBLER_NAME (fn_decl);
  char *buf = (char *)alloca (IDENTIFIER_LENGTH (fn_name) + 10);

  if (counter < 0)
    sprintf (buf, "__gcov__%s", IDENTIFIER_POINTER (fn_name));
  else
    sprintf (buf, "__gcov%u_%s", counter, IDENTIFIER_POINTER (fn_name));
  DECL_NAME (var) = get_identifier (buf);
  TREE_STATIC (var) = 1;
  TREE_ADDRESSABLE (var) = 1;
  DECL_ALIGN (var) = TYPE_ALIGN (type);

  return var;
}

/* Creates the gcov_fn_info RECORD_TYPE.  */

static void
build_fn_info_type (tree type, unsigned counters, tree gcov_info_type)
{
  tree ctr_info = lang_hooks.types.make_type (RECORD_TYPE);
  tree field, fields;
  tree array_type;

  gcc_assert (counters);
  
  /* ctr_info::num */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  fields = field;
  
  /* ctr_info::values */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      build_pointer_type (get_gcov_type ()));
  DECL_CHAIN (field) = fields;
  fields = field;
  
  finish_builtin_struct (ctr_info, "__gcov_ctr_info", fields, NULL_TREE);

  /* key */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      build_pointer_type (build_qualified_type
					  (gcov_info_type, TYPE_QUAL_CONST)));
  fields = field;
  
  /* ident */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;
  
  /* lineno_checksum */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;

  /* cfg checksum */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;

  array_type = build_index_type (size_int (counters - 1));
  array_type = build_array_type (ctr_info, array_type);

  /* counters */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE, array_type);
  DECL_CHAIN (field) = fields;
  fields = field;

  finish_builtin_struct (type, "__gcov_fn_info", fields, NULL_TREE);
}

/* Creates a CONSTRUCTOR for a gcov_fn_info. FUNCTION is
   the function being processed and TYPE is the gcov_fn_info
   RECORD_TYPE.  KEY is the object file key. */

static tree
build_fn_info (const struct function_list *function, tree type, tree key)
{
  tree fields = TYPE_FIELDS (type);
  tree ctr_type;
  unsigned ix;
  VEC(constructor_elt,gc) *v1 = NULL;
  VEC(constructor_elt,gc) *v2 = NULL;

  /* key */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build1 (ADDR_EXPR, TREE_TYPE (fields), key));
  fields = DECL_CHAIN (fields);
  
  /* ident */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  function->ident));
  fields = DECL_CHAIN (fields);

  /* lineno_checksum */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  function->lineno_checksum));
  fields = DECL_CHAIN (fields);

  /* cfg_checksum */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  function->cfg_checksum));
  fields = DECL_CHAIN (fields);

  /* counters */
  ctr_type = TREE_TYPE (TREE_TYPE (fields));
  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if (prg_ctr_mask & (1 << ix))
      {
	VEC(constructor_elt,gc) *ctr = NULL;
	tree var = function->ctr_vars[ix];
	unsigned count = 0;

	if (var)
	  count
	    = tree_low_cst (TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (var))), 0)
	    + 1;

	CONSTRUCTOR_APPEND_ELT (ctr, TYPE_FIELDS (ctr_type),
				build_int_cstu (get_gcov_unsigned_t (),
						count));

	if (var)
	  CONSTRUCTOR_APPEND_ELT (ctr, DECL_CHAIN (TYPE_FIELDS (ctr_type)),
				  build_fold_addr_expr (var));
	
	CONSTRUCTOR_APPEND_ELT (v2, NULL, build_constructor (ctr_type, ctr));
      }
  
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_constructor (TREE_TYPE (fields), v2));

  return build_constructor (type, v1);
}

/* Creaste gcov_info_struct.  N_FUNCS is the number of functions in
   the trailing array.  */

static void
build_info_type (tree type, unsigned n_funcs, tree fn_info_type)
{
  tree field, fields = NULL_TREE;
  tree merge_fn_type, fn_info_array;

  gcc_assert (n_funcs);
  
  /* Version ident */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;

  /* next pointer */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      build_pointer_type (build_qualified_type
					  (type, TYPE_QUAL_CONST)));
  DECL_CHAIN (field) = fields;
  fields = field;

  /* stamp */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;

  /* Filename */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      build_pointer_type (build_qualified_type
					  (char_type_node, TYPE_QUAL_CONST)));
  DECL_CHAIN (field) = fields;
  fields = field;

  /* merge fn array */
  merge_fn_type
    = build_function_type_list (void_type_node,
				build_pointer_type (get_gcov_type ()),
				get_gcov_unsigned_t (), NULL_TREE);
  merge_fn_type
    = build_array_type (build_pointer_type (merge_fn_type),
			build_index_type (size_int (GCOV_COUNTERS - 1)));
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      merge_fn_type);
  DECL_CHAIN (field) = fields;
  fields = field;
  
  /* n_functions */
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      get_gcov_unsigned_t ());
  DECL_CHAIN (field) = fields;
  fields = field;
  
  /* function_info pointer array */
  fn_info_type = build_pointer_type
    (build_qualified_type (fn_info_type, TYPE_QUAL_CONST));
  fn_info_array = build_index_type (size_int (n_funcs));
  fn_info_array = build_array_type (fn_info_type, fn_info_array);
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      fn_info_array);
  DECL_CHAIN (field) = fields;
  fields = field;

  finish_builtin_struct (type, "__gcov_info", fields, NULL_TREE);
}

/* Creates the gcov_info initializer. Returns a CONSTRUCTOR.  */

static tree
build_info (tree info_type, tree fn_type, tree key_var, unsigned n_funcs)
{
  tree info_fields = TYPE_FIELDS (info_type);
  tree merge_fn_type, fn_info_ptr_type;
  unsigned ix;
  tree filename_string;
  int da_file_name_len;
  const struct function_list *fn;
  VEC(constructor_elt,gc) *v1 = NULL;
  VEC(constructor_elt,gc) *v2 = NULL;
  VEC(constructor_elt,gc) *v3 = NULL;

  /* Version ident */
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build_int_cstu (TREE_TYPE (info_fields),
					  GCOV_VERSION));
  info_fields = DECL_CHAIN (info_fields);

  /* next -- NULL */
  CONSTRUCTOR_APPEND_ELT (v1, info_fields, null_pointer_node);
  info_fields = DECL_CHAIN (info_fields);
  
  /* stamp */
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build_int_cstu (TREE_TYPE (info_fields),
					  local_tick));
  info_fields = DECL_CHAIN (info_fields);

  /* Filename */
  da_file_name_len = strlen (da_file_name);
  filename_string = build_string (da_file_name_len + 1, da_file_name);
  TREE_TYPE (filename_string) = build_array_type
    (char_type_node, build_index_type (size_int (da_file_name_len)));
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build1 (ADDR_EXPR, TREE_TYPE (info_fields),
				  filename_string));
  info_fields = DECL_CHAIN (info_fields);

  /* merge fn array -- NULL slots indicate unmeasured counters */
  merge_fn_type = TREE_TYPE (TREE_TYPE (info_fields));
  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    {
      tree ptr = null_pointer_node;

      if ((1u << ix) & prg_ctr_mask)
	{
	  tree merge_fn = build_decl (BUILTINS_LOCATION,
				      FUNCTION_DECL,
				      get_identifier (ctr_merge_functions[ix]),
				      TREE_TYPE (merge_fn_type));
	  DECL_EXTERNAL (merge_fn) = 1;
	  TREE_PUBLIC (merge_fn) = 1;
	  DECL_ARTIFICIAL (merge_fn) = 1;
	  TREE_NOTHROW (merge_fn) = 1;
	  /* Initialize assembler name so we can stream out. */
	  DECL_ASSEMBLER_NAME (merge_fn);
	  ptr = build1 (ADDR_EXPR, merge_fn_type, merge_fn);
	}
      CONSTRUCTOR_APPEND_ELT (v2, NULL, ptr);
    }
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build_constructor (TREE_TYPE (info_fields), v2));
  info_fields = DECL_CHAIN (info_fields);

  /* n_functions */
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build_int_cstu (TREE_TYPE (info_fields), n_funcs));
  info_fields = DECL_CHAIN (info_fields);
  
  /* Build the fn_info type and initializer.  */
  fn_info_ptr_type = TREE_TYPE (TREE_TYPE (info_fields));
  
  for (fn = functions_head; fn; fn = fn->next)
    {
      tree init = build_fn_info (fn, fn_type, key_var);
      tree var = build_var (fn->fn_decl, fn_type, -1);

      DECL_INITIAL (var) = init;
      varpool_finalize_decl (var);
      
      CONSTRUCTOR_APPEND_ELT (v3, NULL,
			      build1 (ADDR_EXPR, fn_info_ptr_type, var));
    }
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build_constructor (TREE_TYPE (info_fields), v3));
  return build_constructor (info_type, v1);
}

/* Write out the structure which libgcov uses to locate all the
   counters.  The structures used here must match those defined in
   gcov-io.h.  Write out the constructor to call __gcov_init.  */

static void
create_coverage (void)
{
  tree gcov_info, gcov_init, body, t;
  tree gcov_info_type, gcov_fn_type;
  unsigned n_counters = 0, n_functions  = 0;
  struct function_list *fn;
  struct function_list **fn_prev;
  unsigned ix;
  char name_buf[32];

  no_coverage = 1; /* Disable any further coverage.  */

  if (!prg_ctr_mask)
    return;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Using data file %s\n", da_file_name);

  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if ((1u << ix) & prg_ctr_mask)
      n_counters++;
  for (fn_prev = &functions_head; (fn = *fn_prev);)
    if (DECL_STRUCT_FUNCTION (fn->fn_decl))
      {
	n_functions++;
	fn_prev = &fn->next;
      }
    else
      /* The function is not being emitted, remove from list.  */
      *fn_prev = fn->next;
  
  /* Build the info and fn_info types.  These are mutually recursive.  */
  gcov_info_type = lang_hooks.types.make_type (RECORD_TYPE);
  gcov_fn_type = lang_hooks.types.make_type (RECORD_TYPE);
  build_fn_info_type (gcov_fn_type, n_counters, gcov_info_type);
  build_info_type (gcov_info_type, n_functions, gcov_fn_type);
  
  /* Build the gcov info var, this is referred to in its own
     initializer.  */
  gcov_info = build_decl (BUILTINS_LOCATION,
			  VAR_DECL, NULL_TREE, gcov_info_type);
  TREE_STATIC (gcov_info) = 1;
  ASM_GENERATE_INTERNAL_LABEL (name_buf, "LPBX", 0);
  DECL_NAME (gcov_info) = get_identifier (name_buf);
  DECL_INITIAL (gcov_info) = build_info (gcov_info_type, gcov_fn_type,
					 gcov_info, n_functions);

  /* Build structure.  */
  varpool_finalize_decl (gcov_info);

  /* Build a decl for __gcov_init.  */
  t = build_pointer_type (TREE_TYPE (gcov_info));
  t = build_function_type_list (void_type_node, t, NULL);
  t = build_decl (BUILTINS_LOCATION,
		  FUNCTION_DECL, get_identifier ("__gcov_init"), t);
  TREE_PUBLIC (t) = 1;
  DECL_EXTERNAL (t) = 1;
  DECL_ASSEMBLER_NAME (t);  /* Initialize assembler name so we can stream out. */
  gcov_init = t;

  /* Generate a call to __gcov_init(&gcov_info).  */
  body = NULL;
  t = build_fold_addr_expr (gcov_info);
  t = build_call_expr (gcov_init, 1, t);
  append_to_statement_list (t, &body);

  /* Generate a constructor to run it.  */
  cgraph_build_static_cdtor ('I', body, DEFAULT_INIT_PRIORITY);
}

/* Perform file-level initialization. Read in data file, generate name
   of graph file.  */

void
coverage_init (const char *filename)
{
  int len = strlen (filename);
  /* + 1 for extra '/', in case prefix doesn't end with /.  */
  int prefix_len;

  if (profile_data_prefix == 0 && !IS_ABSOLUTE_PATH(&filename[0]))
    profile_data_prefix = getpwd ();

  prefix_len = (profile_data_prefix) ? strlen (profile_data_prefix) + 1 : 0;

  /* Name of da file.  */
  da_file_name = XNEWVEC (char, len + strlen (GCOV_DATA_SUFFIX)
			  + prefix_len + 1);

  if (profile_data_prefix)
    {
      strcpy (da_file_name, profile_data_prefix);
      da_file_name[prefix_len - 1] = '/';
      da_file_name[prefix_len] = 0;
    }
  else
    da_file_name[0] = 0;
  strcat (da_file_name, filename);
  strcat (da_file_name, GCOV_DATA_SUFFIX);

  /* Name of bbg file.  */
  bbg_file_name = XNEWVEC (char, len + strlen (GCOV_NOTE_SUFFIX) + 1);
  strcpy (bbg_file_name, filename);
  strcat (bbg_file_name, GCOV_NOTE_SUFFIX);

  if (flag_branch_probabilities)
    read_counts_file ();
}

/* Performs file-level cleanup.  Close graph file, generate coverage
   variables and constructor.  */

void
coverage_finish (void)
{
  create_coverage ();
  if (bbg_file_opened)
    {
      int error = gcov_close ();

      if (error)
	unlink (bbg_file_name);
      if (!local_tick)
	/* Only remove the da file, if we cannot stamp it. If we can
	   stamp it, libgcov will DTRT.  */
	unlink (da_file_name);
    }
}

#include "gt-coverage.h"
