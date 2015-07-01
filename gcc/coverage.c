/* Read and write coverage files, and associated functionality.
   Copyright (C) 1990-2015 Free Software Foundation, Inc.
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
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "function.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "predict.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "toplev.h"
#include "tm_p.h"
#include "coverage.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "context.h"
#include "pass_manager.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "filenames.h"
#include "target.h"
#include "params.h"
#include "auto-profile.h"

#include "gcov-io.h"
#include "gcov-io.c"

struct GTY((chain_next ("%h.next"))) coverage_data
{
  struct coverage_data *next;	 /* next function */
  unsigned ident;		 /* function ident */
  unsigned lineno_checksum;	 /* function lineno checksum */
  unsigned cfg_checksum;	 /* function cfg checksum */
  tree fn_decl;			 /* the function decl */
  tree ctr_vars[GCOV_COUNTERS];	 /* counter variables.  */
};

/* Counts information for a function.  */
typedef struct counts_entry : pointer_hash <counts_entry>
{
  /* We hash by  */
  unsigned ident;
  unsigned ctr;

  /* Store  */
  unsigned lineno_checksum;
  unsigned cfg_checksum;
  gcov_type *counts;
  struct gcov_ctr_summary summary;

  /* hash_table support.  */
  static inline hashval_t hash (const counts_entry *);
  static int equal (const counts_entry *, const counts_entry *);
  static void remove (counts_entry *);
} counts_entry_t;

static GTY(()) struct coverage_data *functions_head = 0;
static struct coverage_data **functions_tail = &functions_head;
static unsigned no_coverage = 0;

/* Cumulative counter information for whole program.  */
static unsigned prg_ctr_mask; /* Mask of counter types generated.  */

/* Counter information for current function.  */
static unsigned fn_ctr_mask; /* Mask of counters used.  */
static GTY(()) tree fn_v_ctrs[GCOV_COUNTERS];   /* counter variables.  */
static unsigned fn_n_ctrs[GCOV_COUNTERS]; /* Counters allocated.  */
static unsigned fn_b_ctrs[GCOV_COUNTERS]; /* Allocation base.  */

/* Coverage info VAR_DECL and function info type nodes.  */
static GTY(()) tree gcov_info_var;
static GTY(()) tree gcov_fn_info_type;
static GTY(()) tree gcov_fn_info_ptr_type;

/* Name of the notes (gcno) output file.  The "bbg" prefix is for
   historical reasons, when the notes file contained only the
   basic block graph notes.
   If this is NULL we're not writing to the notes file.  */
static char *bbg_file_name;

/* File stamp for notes file.  */
static unsigned bbg_file_stamp;

/* Name of the count data (gcda) file.  */
static char *da_file_name;

/* The names of merge functions for counters.  */
#define STR(str) #str
#define DEF_GCOV_COUNTER(COUNTER, NAME, FN_TYPE) STR(__gcov_merge ## FN_TYPE),
static const char *const ctr_merge_functions[GCOV_COUNTERS] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER
#undef STR

#define DEF_GCOV_COUNTER(COUNTER, NAME, FN_TYPE) NAME,
static const char *const ctr_names[GCOV_COUNTERS] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER

/* Forward declarations.  */
static void read_counts_file (void);
static tree build_var (tree, tree, int);
static void build_fn_info_type (tree, unsigned, tree);
static void build_info_type (tree, tree);
static tree build_fn_info (const struct coverage_data *, tree, tree);
static tree build_info (tree, tree);
static bool coverage_obj_init (void);
static vec<constructor_elt, va_gc> *coverage_obj_fn
(vec<constructor_elt, va_gc> *, tree, struct coverage_data const *);
static void coverage_obj_finish (vec<constructor_elt, va_gc> *);

/* Return the type node for gcov_type.  */

tree
get_gcov_type (void)
{
  machine_mode mode = smallest_mode_for_size (GCOV_TYPE_SIZE, MODE_INT);
  return lang_hooks.types.type_for_mode (mode, false);
}

/* Return the type node for gcov_unsigned_t.  */

static tree
get_gcov_unsigned_t (void)
{
  machine_mode mode = smallest_mode_for_size (32, MODE_INT);
  return lang_hooks.types.type_for_mode (mode, true);
}

inline hashval_t
counts_entry::hash (const counts_entry *entry)
{
  return entry->ident * GCOV_COUNTERS + entry->ctr;
}

inline int
counts_entry::equal (const counts_entry *entry1, const counts_entry *entry2)
{
  return entry1->ident == entry2->ident && entry1->ctr == entry2->ctr;
}

inline void
counts_entry::remove (counts_entry *entry)
{
  free (entry->counts);
  free (entry);
}

/* Hash table of count data.  */
static hash_table<counts_entry> *counts_hash;

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

  /* Read the stamp, used for creating a generation count.  */
  tag = gcov_read_unsigned ();
  bbg_file_stamp = crc32_unsigned (bbg_file_stamp, tag);

  counts_hash = new hash_table<counts_entry> (10);
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
          if (new_summary)
            memcpy (summary.ctrs[GCOV_COUNTER_ARCS].histogram,
                    sum.ctrs[GCOV_COUNTER_ARCS].histogram,
                    sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);
          else
            gcov_histogram_merge (summary.ctrs[GCOV_COUNTER_ARCS].histogram,
                                  sum.ctrs[GCOV_COUNTER_ARCS].histogram);
	  new_summary = 0;
	}
      else if (GCOV_TAG_IS_COUNTER (tag) && fn_ident)
	{
	  counts_entry_t **slot, *entry, elt;
	  unsigned n_counts = GCOV_TAG_COUNTER_NUM (length);
	  unsigned ix;

	  elt.ident = fn_ident;
	  elt.ctr = GCOV_COUNTER_FOR_TAG (tag);

	  slot = counts_hash->find_slot (&elt, INSERT);
	  entry = *slot;
	  if (!entry)
	    {
	      *slot = entry = XCNEW (counts_entry_t);
	      entry->ident = fn_ident;
	      entry->ctr = elt.ctr;
	      entry->lineno_checksum = lineno_checksum;
	      entry->cfg_checksum = cfg_checksum;
              if (elt.ctr < GCOV_COUNTERS_SUMMABLE)
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
	      delete counts_hash;
	      counts_hash = NULL;
	      break;
	    }
	  else if (entry->summary.num != n_counts)
	    {
	      error ("Profile data for function %u is corrupted", fn_ident);
	      error ("number of counters is %d instead of %d", entry->summary.num, n_counts);
	      delete counts_hash;
	      counts_hash = NULL;
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
	  delete counts_hash;
	  counts_hash = NULL;
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

      if (!warned++ && dump_enabled_p ())
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, input_location,
                         (flag_guess_branch_prob
                          ? "file %s not found, execution counts estimated\n"
                          : "file %s not found, execution counts assumed to "
                            "be zero\n"),
                         da_file_name);
      return NULL;
    }
  if (PARAM_VALUE (PARAM_PROFILE_FUNC_INTERNAL_ID))
    elt.ident = current_function_funcdef_no + 1;
  else
    {
      gcc_assert (coverage_node_map_initialized_p ());
      elt.ident = cgraph_node::get (cfun->decl)->profile_id;
    }
  elt.ctr = counter;
  entry = counts_hash->find (&elt);
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
      if (warning_printed && dump_enabled_p ())
	{
          dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, input_location,
                           "use -Wno-error=coverage-mismatch to tolerate "
                           "the mismatch but performance may drop if the "
                           "function is hot\n");
	  
	  if (!seen_error ()
	      && !warned++)
	    {
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, input_location,
                               "coverage mismatch ignored\n");
	      dump_printf (MSG_OPTIMIZED_LOCATIONS,
                           flag_guess_branch_prob
                           ? G_("execution counts estimated\n")
                           : G_("execution counts assumed to be zero\n"));
	      if (!flag_guess_branch_prob)
		dump_printf (MSG_OPTIMIZED_LOCATIONS,
                             "this can result in poorly optimized code\n");
	    }
	}

      return NULL;
    }
  else if (entry->lineno_checksum != lineno_checksum)
    {
      warning (OPT_Wcoverage_mismatch,
               "source locations for function %qE have changed,"
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

/* Compute profile ID.  This is better to be unique in whole program.  */

unsigned
coverage_compute_profile_id (struct cgraph_node *n)
{
  unsigned chksum;

  /* Externally visible symbols have unique name.  */
  if (TREE_PUBLIC (n->decl) || DECL_EXTERNAL (n->decl) || n->unique_name)
    {
      chksum = coverage_checksum_string
	(0, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (n->decl)));
    }
  else
    {
      expanded_location xloc
	= expand_location (DECL_SOURCE_LOCATION (n->decl));
      bool use_name_only = (PARAM_VALUE (PARAM_PROFILE_FUNC_INTERNAL_ID) == 0);

      chksum = (use_name_only ? 0 : xloc.line);
      chksum = coverage_checksum_string (chksum, xloc.file);
      chksum = coverage_checksum_string
	(chksum, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (n->decl)));
      if (!use_name_only && first_global_object_name)
	chksum = coverage_checksum_string
	  (chksum, first_global_object_name);
      chksum = coverage_checksum_string
	(chksum, aux_base_name);
    }

  /* Non-negative integers are hopefully small enough to fit in all targets.
     Gcov file formats wants non-zero function IDs.  */
  chksum = chksum & 0x7fffffff;
  return chksum + (!chksum);
}

/* Compute cfg checksum for the function FN given as argument.
   The checksum is calculated carefully so that
   source code changes that doesn't affect the control flow graph
   won't change the checksum.
   This is to make the profile data useable across source code change.
   The downside of this is that the compiler may use potentially
   wrong profile data - that the source code change has non-trivial impact
   on the validity of profile data (e.g. the reversed condition)
   but the compiler won't detect the change and use the wrong profile data.  */

unsigned
coverage_compute_cfg_checksum (struct function *fn)
{
  basic_block bb;
  unsigned chksum = n_basic_blocks_for_fn (fn);

  FOR_EACH_BB_FN (bb, fn)
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

/* Begin output to the notes file for the current function.
   Writes the function header. Returns nonzero if data should be output.  */

int
coverage_begin_function (unsigned lineno_checksum, unsigned cfg_checksum)
{
  expanded_location xloc;
  unsigned long offset;

  /* We don't need to output .gcno file unless we're under -ftest-coverage
     (e.g. -fprofile-arcs/generate/use don't need .gcno to work). */
  if (no_coverage || !bbg_file_name)
    return 0;

  xloc = expand_location (DECL_SOURCE_LOCATION (current_function_decl));

  /* Announce function */
  offset = gcov_write_tag (GCOV_TAG_FUNCTION);
  if (PARAM_VALUE (PARAM_PROFILE_FUNC_INTERNAL_ID))
    gcov_write_unsigned (current_function_funcdef_no + 1);
  else
    {
      gcc_assert (coverage_node_map_initialized_p ());
      gcov_write_unsigned (
        cgraph_node::get (current_function_decl)->profile_id);
    }

  gcov_write_unsigned (lineno_checksum);
  gcov_write_unsigned (cfg_checksum);
  gcov_write_string (IDENTIFIER_POINTER
		     (DECL_ASSEMBLER_NAME (current_function_decl)));
  gcov_write_string (xloc.file);
  gcov_write_unsigned (xloc.line);
  gcov_write_length (offset);

  return !gcov_is_error ();
}

/* Finish coverage data for the current function. Verify no output
   error has occurred.  Save function coverage counts.  */

void
coverage_end_function (unsigned lineno_checksum, unsigned cfg_checksum)
{
  unsigned i;

  if (bbg_file_name && gcov_is_error ())
    {
      warning (0, "error writing %qs", bbg_file_name);
      unlink (bbg_file_name);
      bbg_file_name = NULL;
    }

  if (fn_ctr_mask)
    {
      struct coverage_data *item = 0;

      item = ggc_alloc<coverage_data> ();

      if (PARAM_VALUE (PARAM_PROFILE_FUNC_INTERNAL_ID))
	item->ident = current_function_funcdef_no + 1;
      else
	{
	  gcc_assert (coverage_node_map_initialized_p ());
	  item->ident = cgraph_node::get (cfun->decl)->profile_id;
	}

      item->lineno_checksum = lineno_checksum;
      item->cfg_checksum = cfg_checksum;

      item->fn_decl = current_function_decl;
      item->next = 0;
      *functions_tail = item;
      functions_tail = &item->next;

      for (i = 0; i != GCOV_COUNTERS; i++)
	{
	  tree var = fn_v_ctrs[i];

	  if (item)
	    item->ctr_vars[i] = var;
	  if (var)
	    {
	      tree array_type = build_index_type (size_int (fn_n_ctrs[i] - 1));
	      array_type = build_array_type (get_gcov_type (), array_type);
	      TREE_TYPE (var) = array_type;
	      DECL_SIZE (var) = TYPE_SIZE (array_type);
	      DECL_SIZE_UNIT (var) = TYPE_SIZE_UNIT (array_type);
	      varpool_node::finalize_decl (var);
	    }
	  
	  fn_b_ctrs[i] = fn_n_ctrs[i] = 0;
	  fn_v_ctrs[i] = NULL_TREE;
	}
      prg_ctr_mask |= fn_ctr_mask;
      fn_ctr_mask = 0;
    }
}

/* Build a coverage variable of TYPE for function FN_DECL.  If COUNTER
   >= 0 it is a counter array, otherwise it is the function structure.  */

static tree
build_var (tree fn_decl, tree type, int counter)
{
  tree var = build_decl (BUILTINS_LOCATION, VAR_DECL, NULL_TREE, type);
  const char *fn_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn_decl));
  char *buf;
  size_t fn_name_len, len;

  fn_name = targetm.strip_name_encoding (fn_name);
  fn_name_len = strlen (fn_name);
  buf = XALLOCAVEC (char, fn_name_len + 8 + sizeof (int) * 3);

  if (counter < 0)
    strcpy (buf, "__gcov__");
  else
    sprintf (buf, "__gcov%u_", counter);
  len = strlen (buf);
#ifndef NO_DOT_IN_LABEL
  buf[len - 1] = '.';
#elif !defined NO_DOLLAR_IN_LABEL
  buf[len - 1] = '$';
#endif
  memcpy (buf + len, fn_name, fn_name_len + 1);
  DECL_NAME (var) = get_identifier (buf);
  TREE_STATIC (var) = 1;
  TREE_ADDRESSABLE (var) = 1;
  DECL_NONALIASED (var) = 1;
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

/* Returns a CONSTRUCTOR for a gcov_fn_info.  DATA is
   the coverage data for the function and TYPE is the gcov_fn_info
   RECORD_TYPE.  KEY is the object file key.  */

static tree
build_fn_info (const struct coverage_data *data, tree type, tree key)
{
  tree fields = TYPE_FIELDS (type);
  tree ctr_type;
  unsigned ix;
  vec<constructor_elt, va_gc> *v1 = NULL;
  vec<constructor_elt, va_gc> *v2 = NULL;

  /* key */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build1 (ADDR_EXPR, TREE_TYPE (fields), key));
  fields = DECL_CHAIN (fields);
  
  /* ident */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  data->ident));
  fields = DECL_CHAIN (fields);

  /* lineno_checksum */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  data->lineno_checksum));
  fields = DECL_CHAIN (fields);

  /* cfg_checksum */
  CONSTRUCTOR_APPEND_ELT (v1, fields,
			  build_int_cstu (get_gcov_unsigned_t (),
					  data->cfg_checksum));
  fields = DECL_CHAIN (fields);

  /* counters */
  ctr_type = TREE_TYPE (TREE_TYPE (fields));
  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if (prg_ctr_mask & (1 << ix))
      {
	vec<constructor_elt, va_gc> *ctr = NULL;
	tree var = data->ctr_vars[ix];
	unsigned count = 0;

	if (var)
	  count
	    = tree_to_shwi (TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (var))))
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

/* Create gcov_info struct.  TYPE is the incomplete RECORD_TYPE to be
   completed, and FN_INFO_PTR_TYPE is a pointer to the function info type.  */

static void
build_info_type (tree type, tree fn_info_ptr_type)
{
  tree field, fields = NULL_TREE;
  tree merge_fn_type;

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
  
  /* function_info pointer pointer */
  fn_info_ptr_type = build_pointer_type
    (build_qualified_type (fn_info_ptr_type, TYPE_QUAL_CONST));
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
		      fn_info_ptr_type);
  DECL_CHAIN (field) = fields;
  fields = field;

  finish_builtin_struct (type, "__gcov_info", fields, NULL_TREE);
}

/* Returns a CONSTRUCTOR for the gcov_info object.  INFO_TYPE is the
   gcov_info structure type, FN_ARY is the array of pointers to
   function info objects.  */

static tree
build_info (tree info_type, tree fn_ary)
{
  tree info_fields = TYPE_FIELDS (info_type);
  tree merge_fn_type, n_funcs;
  unsigned ix;
  tree filename_string;
  int da_file_name_len;
  vec<constructor_elt, va_gc> *v1 = NULL;
  vec<constructor_elt, va_gc> *v2 = NULL;

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
					  bbg_file_stamp));
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
  n_funcs = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (fn_ary)));
  n_funcs = fold_build2 (PLUS_EXPR, TREE_TYPE (info_fields),
			 n_funcs, size_one_node);
  CONSTRUCTOR_APPEND_ELT (v1, info_fields, n_funcs);
  info_fields = DECL_CHAIN (info_fields);

  /* functions */
  CONSTRUCTOR_APPEND_ELT (v1, info_fields,
			  build1 (ADDR_EXPR, TREE_TYPE (info_fields), fn_ary));
  info_fields = DECL_CHAIN (info_fields);

  gcc_assert (!info_fields);
  return build_constructor (info_type, v1);
}

/* Generate the constructor function to call __gcov_init.  */

static void
build_init_ctor (tree gcov_info_type)
{
  tree ctor, stmt, init_fn;

  /* Build a decl for __gcov_init.  */
  init_fn = build_pointer_type (gcov_info_type);
  init_fn = build_function_type_list (void_type_node, init_fn, NULL);
  init_fn = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			get_identifier ("__gcov_init"), init_fn);
  TREE_PUBLIC (init_fn) = 1;
  DECL_EXTERNAL (init_fn) = 1;
  DECL_ASSEMBLER_NAME (init_fn);

  /* Generate a call to __gcov_init(&gcov_info).  */
  ctor = NULL;
  stmt = build_fold_addr_expr (gcov_info_var);
  stmt = build_call_expr (init_fn, 1, stmt);
  append_to_statement_list (stmt, &ctor);

  /* Generate a constructor to run it.  */
  cgraph_build_static_cdtor ('I', ctor, DEFAULT_INIT_PRIORITY);
}

/* Create the gcov_info types and object.  Generate the constructor
   function to call __gcov_init.  Does not generate the initializer
   for the object.  Returns TRUE if coverage data is being emitted.  */

static bool
coverage_obj_init (void)
{
  tree gcov_info_type;
  unsigned n_counters = 0;
  unsigned ix;
  struct coverage_data *fn;
  struct coverage_data **fn_prev;
  char name_buf[32];

  no_coverage = 1; /* Disable any further coverage.  */

  if (!prg_ctr_mask)
    return false;

  if (symtab->dump_file)
    fprintf (symtab->dump_file, "Using data file %s\n", da_file_name);

  /* Prune functions.  */
  for (fn_prev = &functions_head; (fn = *fn_prev);)
    if (DECL_STRUCT_FUNCTION (fn->fn_decl))
      fn_prev = &fn->next;
    else
      /* The function is not being emitted, remove from list.  */
      *fn_prev = fn->next;

  if (functions_head == NULL)
    return false;

  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if ((1u << ix) & prg_ctr_mask)
      n_counters++;
  
  /* Build the info and fn_info types.  These are mutually recursive.  */
  gcov_info_type = lang_hooks.types.make_type (RECORD_TYPE);
  gcov_fn_info_type = lang_hooks.types.make_type (RECORD_TYPE);
  build_fn_info_type (gcov_fn_info_type, n_counters, gcov_info_type);
  gcov_info_type = lang_hooks.types.make_type (RECORD_TYPE);
  gcov_fn_info_ptr_type = build_pointer_type
    (build_qualified_type (gcov_fn_info_type, TYPE_QUAL_CONST));
  build_info_type (gcov_info_type, gcov_fn_info_ptr_type);
  
  /* Build the gcov info var, this is referred to in its own
     initializer.  */
  gcov_info_var = build_decl (BUILTINS_LOCATION,
			      VAR_DECL, NULL_TREE, gcov_info_type);
  TREE_STATIC (gcov_info_var) = 1;
  ASM_GENERATE_INTERNAL_LABEL (name_buf, "LPBX", 0);
  DECL_NAME (gcov_info_var) = get_identifier (name_buf);

  build_init_ctor (gcov_info_type);

  return true;
}

/* Generate the coverage function info for FN and DATA.  Append a
   pointer to that object to CTOR and return the appended CTOR.  */

static vec<constructor_elt, va_gc> *
coverage_obj_fn (vec<constructor_elt, va_gc> *ctor, tree fn,
		 struct coverage_data const *data)
{
  tree init = build_fn_info (data, gcov_fn_info_type, gcov_info_var);
  tree var = build_var (fn, gcov_fn_info_type, -1);
  
  DECL_INITIAL (var) = init;
  varpool_node::finalize_decl (var);
      
  CONSTRUCTOR_APPEND_ELT (ctor, NULL,
			  build1 (ADDR_EXPR, gcov_fn_info_ptr_type, var));
  return ctor;
}

/* Finalize the coverage data.  Generates the array of pointers to
   function objects from CTOR.  Generate the gcov_info initializer.  */

static void
coverage_obj_finish (vec<constructor_elt, va_gc> *ctor)
{
  unsigned n_functions = vec_safe_length (ctor);
  tree fn_info_ary_type = build_array_type
    (build_qualified_type (gcov_fn_info_ptr_type, TYPE_QUAL_CONST),
     build_index_type (size_int (n_functions - 1)));
  tree fn_info_ary = build_decl (BUILTINS_LOCATION, VAR_DECL, NULL_TREE,
				 fn_info_ary_type);
  char name_buf[32];

  TREE_STATIC (fn_info_ary) = 1;
  ASM_GENERATE_INTERNAL_LABEL (name_buf, "LPBX", 1);
  DECL_NAME (fn_info_ary) = get_identifier (name_buf);
  DECL_INITIAL (fn_info_ary) = build_constructor (fn_info_ary_type, ctor);
  varpool_node::finalize_decl (fn_info_ary);
  
  DECL_INITIAL (gcov_info_var)
    = build_info (TREE_TYPE (gcov_info_var), fn_info_ary);
  varpool_node::finalize_decl (gcov_info_var);
}

/* Perform file-level initialization. Read in data file, generate name
   of notes file.  */

void
coverage_init (const char *filename)
{
  int len = strlen (filename);
  int prefix_len = 0;

  /* Since coverage_init is invoked very early, before the pass
     manager, we need to set up the dumping explicitly. This is
     similar to the handling in finish_optimization_passes.  */
  int profile_pass_num =
    g->get_passes ()->get_pass_profile ()->static_pass_number;
  g->get_dumps ()->dump_start (profile_pass_num, NULL);

  if (!profile_data_prefix && !IS_ABSOLUTE_PATH (filename))
    profile_data_prefix = getpwd ();

  if (profile_data_prefix)
    prefix_len = strlen (profile_data_prefix);

  /* Name of da file.  */
  da_file_name = XNEWVEC (char, len + strlen (GCOV_DATA_SUFFIX)
			  + prefix_len + 2);

  if (profile_data_prefix)
    {
      memcpy (da_file_name, profile_data_prefix, prefix_len);
      da_file_name[prefix_len++] = '/';
    }
  memcpy (da_file_name + prefix_len, filename, len);
  strcpy (da_file_name + prefix_len + len, GCOV_DATA_SUFFIX);

  bbg_file_stamp = local_tick;
  
  if (flag_auto_profile)
    read_autofdo_file ();
  else if (flag_branch_probabilities)
    read_counts_file ();

  /* Name of bbg file.  */
  if (flag_test_coverage && !flag_compare_debug)
    {
      bbg_file_name = XNEWVEC (char, len + strlen (GCOV_NOTE_SUFFIX) + 1);
      memcpy (bbg_file_name, filename, len);
      strcpy (bbg_file_name + len, GCOV_NOTE_SUFFIX);

      if (!gcov_open (bbg_file_name, -1))
	{
	  error ("cannot open %s", bbg_file_name);
	  bbg_file_name = NULL;
	}
      else
	{
	  gcov_write_unsigned (GCOV_NOTE_MAGIC);
	  gcov_write_unsigned (GCOV_VERSION);
	  gcov_write_unsigned (bbg_file_stamp);
	}
    }

  g->get_dumps ()->dump_finish (profile_pass_num);
}

/* Performs file-level cleanup.  Close notes file, generate coverage
   variables and constructor.  */

void
coverage_finish (void)
{
  if (bbg_file_name && gcov_close ())
    unlink (bbg_file_name);

  if (!flag_branch_probabilities && flag_test_coverage
      && (!local_tick || local_tick == (unsigned)-1))
    /* Only remove the da file, if we're emitting coverage code and
       cannot uniquely stamp it.  If we can stamp it, libgcov will DTRT.  */
    unlink (da_file_name);

  if (coverage_obj_init ())
    {
      vec<constructor_elt, va_gc> *fn_ctor = NULL;
      struct coverage_data *fn;
      
      for (fn = functions_head; fn; fn = fn->next)
	fn_ctor = coverage_obj_fn (fn_ctor, fn->fn_decl, fn);
      coverage_obj_finish (fn_ctor);
    }

  XDELETEVEC (da_file_name);
  da_file_name = NULL;
}

#include "gt-coverage.h"
