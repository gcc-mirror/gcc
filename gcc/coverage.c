/* Read and write coverage files, and associated functionality.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2003  Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Further mangled by Nathan Sidwell, CodeSourcery

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


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
#include "toplev.h"
#include "ggc.h"
#include "target.h"
#include "coverage.h"
#include "libfuncs.h"
#include "langhooks.h"
#include "hashtab.h"

#include "gcov-io.c"

struct function_list
{
  struct function_list *next; 	 /* next function */
  unsigned ident; 		 /* function ident */
  unsigned checksum;	         /* function checksum */
  unsigned n_ctrs[GCOV_COUNTERS];/* number of counters.  */
};

/* Counts information for a function.  */
typedef struct counts_entry
{
  /* We hash by  */
  unsigned ident;
  unsigned ctr;
  
  /* Store  */
  unsigned checksum;
  gcov_type *counts;
  struct gcov_ctr_summary summary;

  /* Workspace */
  struct counts_entry *chain;
  
} counts_entry_t;

static unsigned fn_ident = 1;
static struct function_list *functions_head = 0;
static struct function_list **functions_tail = &functions_head;

/* Cumulative counter information for whole program.  */
static unsigned prg_ctr_mask; /* Mask of counter types generated.  */
static unsigned prg_n_ctrs[GCOV_COUNTERS];

/* Counter information for current function.  */
static unsigned fn_ctr_mask;
static unsigned fn_n_ctrs[GCOV_COUNTERS];

/* Name of the output file for coverage output file.  */
static char *bbg_file_name;
static unsigned bbg_file_opened;
static int bbg_function_announced;

/* Name of the count data file.  */
static char *da_file_name;

/* Hash table of count data.  */
static htab_t counts_hash = NULL;

/* The names of the counter tables.  */
static GTY(()) rtx ctr_labels[GCOV_COUNTERS];

/* Forward declarations.  */
static hashval_t htab_counts_entry_hash PARAMS ((const void *));
static int htab_counts_entry_eq PARAMS ((const void *, const void *));
static void htab_counts_entry_del PARAMS ((void *));
static void read_counts_file PARAMS ((void));
static unsigned compute_checksum PARAMS ((void));
static unsigned checksum_string PARAMS ((unsigned, const char *));
static tree build_fn_info_type PARAMS ((unsigned));
static tree build_fn_info_value PARAMS ((const struct function_list *, tree));
static tree build_ctr_info_type PARAMS ((void));
static tree build_ctr_info_value PARAMS ((unsigned, tree));
static tree build_gcov_info PARAMS ((void));
static void create_coverage PARAMS ((void));


static hashval_t
htab_counts_entry_hash (of)
     const void *of;
{
  const counts_entry_t *entry = of;

  return entry->ident * GCOV_COUNTERS + entry->ctr;
}

static int
htab_counts_entry_eq (of1, of2)
     const void *of1;
     const void *of2;
{
  const counts_entry_t *entry1 = of1;
  const counts_entry_t *entry2 = of2;

  return entry1->ident == entry2->ident && entry1->ctr == entry2->ctr;
}

static void
htab_counts_entry_del (of)
     void *of;
{
  counts_entry_t *entry = of;

  free (entry->counts);
  free (entry);
}

/* Read in the counts file, if available.  */

static void
read_counts_file ()
{
  unsigned fn_ident = 0;
  unsigned version, ix, checksum = -1;
  counts_entry_t *summaried = NULL;
  unsigned seen_summary = 0;
  
  if (!gcov_open (da_file_name, 1))
    return;
  
  if (gcov_read_unsigned () != GCOV_DATA_MAGIC)
    {
      warning ("`%s' is not a gcov data file", da_file_name);
      gcov_close ();
      return;
    }
  else if ((version = gcov_read_unsigned ()) != GCOV_VERSION)
    {
      char v[4], e[4];
      unsigned required = GCOV_VERSION;
      
      for (ix = 4; ix--; required >>= 8, version >>= 8)
	{
	  v[ix] = version;
	  e[ix] = required;
	}
      warning ("`%s' is version `%.4s', expected version `%.4s'",
	       da_file_name, v, e);
      gcov_close ();
      return;
    }
  
  counts_hash = htab_create (10,
			     htab_counts_entry_hash, htab_counts_entry_eq,
			     htab_counts_entry_del);
  while (!gcov_is_eof ())
    {
      unsigned tag, length;
      unsigned long offset;
      int error;
      
      tag = gcov_read_unsigned ();
      length = gcov_read_unsigned ();
      offset = gcov_position ();
      if (tag == GCOV_TAG_FUNCTION)
	{
	  fn_ident = gcov_read_unsigned ();
	  checksum = gcov_read_unsigned ();
	  if (seen_summary)
	    {
	      /* We have already seen a summary, this means that this
		 new function begins a new set of program runs. We
		 must unlink the summaried chain.  */
	      counts_entry_t *entry, *chain;
	      
	      for (entry = summaried; entry; entry = chain)
		{
		  chain = entry->chain;
		  entry->chain = NULL;
		}
	      summaried = NULL;
	      seen_summary = 0;
	    }
	}
      else if (tag == GCOV_TAG_PROGRAM_SUMMARY)
	{
	  counts_entry_t *entry;
	  struct gcov_summary summary;
	  
	  gcov_read_summary (&summary);
	  seen_summary = 1;
	  for (entry = summaried; entry; entry = entry->chain)
	    {
	      struct gcov_ctr_summary *csum = &summary.ctrs[entry->ctr];
	      
	      entry->summary.runs += csum->runs;
	      entry->summary.sum_all += csum->sum_all;
	      if (entry->summary.run_max < csum->run_max)
		entry->summary.run_max = csum->run_max;
	      entry->summary.sum_max += csum->sum_max;
	    }
	}
      else if (GCOV_TAG_IS_COUNTER (tag) && fn_ident)
	{
	  counts_entry_t **slot, *entry, elt;
	  unsigned n_counts = length / 8;
	  unsigned ix;

	  elt.ident = fn_ident;
	  elt.ctr = GCOV_COUNTER_FOR_TAG (tag);

	  slot = (counts_entry_t **) htab_find_slot
	    (counts_hash, &elt, INSERT);
	  entry = *slot;
	  if (!entry)
	    {
	      *slot = entry = xcalloc (1, sizeof (counts_entry_t));
	      entry->ident = elt.ident;
	      entry->ctr = elt.ctr;
	      entry->checksum = checksum;
	      entry->summary.num = n_counts;
	      entry->counts = xcalloc (n_counts, sizeof (gcov_type));
	    }
	  else if (entry->checksum != checksum
		   || entry->summary.num != n_counts)
	    {
	      warning ("coverage mismatch for function %u", fn_ident);
	      htab_delete (counts_hash);
	      break;
	    }
	  
	  /* This should always be true for a just allocated entry,
	     and always false for an existing one. Check this way, in
	     case the gcov file is corrupt.  */
	  if (!entry->chain || summaried != entry)
	    {
	      entry->chain = summaried;
	      summaried = entry;
	    }
	  for (ix = 0; ix != n_counts; ix++)
	    entry->counts[ix] += gcov_read_counter ();
	}
      gcov_seek (offset, length);
      if ((error = gcov_is_error ()))
	{
	  warning (error < 0 ? "`%s' has overflowed" : "`%s' is corrupted",
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
		     const struct gcov_ctr_summary **summary)
{
  counts_entry_t *entry, elt;

  /* No hash table, no counts. */
  if (!counts_hash)
    {
      static int warned = 0;

      if (!warned++)
	warning ("file %s not found, execution counts assumed to be zero",
		 da_file_name);
      return NULL;
    }

  elt.ident = fn_ident;
  elt.ctr = counter;
  entry = htab_find (counts_hash, &elt);
  if (!entry)
    {
      warning ("no coverage for function '%s' found.", IDENTIFIER_POINTER
	       (DECL_ASSEMBLER_NAME (current_function_decl)));
      return 0;
    }
  
  if (expected != entry->summary.num
      || compute_checksum () != entry->checksum)
    {
      warning ("coverage mismatch for `%s'", IDENTIFIER_POINTER
	       (DECL_ASSEMBLER_NAME (current_function_decl)));
      return NULL;
    }
  
  if (summary)
    *summary = &entry->summary;

  return entry->counts;
}

/* Generate a MEM rtl to access COUNTER NO .  */

rtx
coverage_counter_ref (unsigned counter, unsigned no)
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx ref;

  if (!ctr_labels[counter])
    {
      /* Generate and save a copy of this so it can be shared.  */
      char buf[20];
      
      ASM_GENERATE_INTERNAL_LABEL (buf, "LPBX", counter + 1);
      ctr_labels[counter] = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
    }
  if (no + 1 > fn_n_ctrs[counter])
    {
      fn_n_ctrs[counter] = no + 1;
      fn_ctr_mask |= 1 << counter;
    }

  no += prg_n_ctrs[counter];
  ref = plus_constant (ctr_labels[counter],
		       GCOV_TYPE_SIZE / BITS_PER_UNIT * no);
  ref = gen_rtx_MEM (mode, ref);
  set_mem_alias_set (ref, new_alias_set ());

  return ref;
}

/* Generate a checksum for a string.  CHKSUM is the current
   checksum. */

static unsigned
checksum_string (unsigned chksum, const char *string)
{
  do
    {
      unsigned value = *string << 24;
      unsigned ix;

      for (ix = 8; ix--; value <<= 1)
	{
	  unsigned feedback;
	  
	  feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
	  chksum <<= 1;
	  chksum ^= feedback;
	}
    }
  while (*string++);
  
  return chksum;
}

/* Compute checksum for the current function.  We generate a CRC32.  */

static unsigned
compute_checksum ()
{
  unsigned chksum = DECL_SOURCE_LINE (current_function_decl);

  chksum = checksum_string (chksum, DECL_SOURCE_FILE (current_function_decl));
  chksum = checksum_string
    (chksum, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl)));

  return chksum;
}

/* Begin output to the graph file for the current function.
   Opens the output file, if not already done. Writes the
   function header, if not already done. Returns non-zero if data
   should be output.  */

int
coverage_begin_output ()
{
  if (!bbg_function_announced)
    {
      const char *file = DECL_SOURCE_FILE (current_function_decl);
      unsigned line = DECL_SOURCE_LINE (current_function_decl);
      unsigned long offset;
      
      if (!bbg_file_opened)
	{
	  if (!gcov_open (bbg_file_name, -1))
	    error ("cannot open %s", bbg_file_name);
	  else
	    {
	      gcov_write_unsigned (GCOV_GRAPH_MAGIC);
	      gcov_write_unsigned (GCOV_VERSION);
	    }
	  bbg_file_opened = 1;
	}
      
      /* Announce function */
      offset = gcov_write_tag (GCOV_TAG_FUNCTION);
      gcov_write_unsigned (fn_ident);
      gcov_write_unsigned (compute_checksum ());
      gcov_write_string (IDENTIFIER_POINTER
			 (DECL_ASSEMBLER_NAME (current_function_decl)));
      gcov_write_string (file);
      gcov_write_unsigned (line);
      gcov_write_length (offset);

      bbg_function_announced = 1;
    }
  return !gcov_is_error ();
}

/* Finish coverage data for the current function. Verify no output
   error has occurred.  Save function coverage counts.  */

void
coverage_end_function ()
{
  unsigned i;
  
  if (bbg_file_opened > 1 && gcov_is_error ())
    {	
      warning ("error writing `%s'", bbg_file_name);
      bbg_file_opened = -1;
    }

  if (fn_ctr_mask)
    {
      struct function_list *item;
      
      item = xmalloc (sizeof (struct function_list));
      
      *functions_tail = item;
      functions_tail = &item->next;
	
      item->next = 0;
      /* It would be nice to use the unique source location. */
      item->ident = fn_ident;
      item->checksum = compute_checksum ();
      for (i = 0; i != GCOV_COUNTERS; i++)
	{
	  item->n_ctrs[i] = fn_n_ctrs[i];
	  prg_n_ctrs[i] += fn_n_ctrs[i];
	  fn_n_ctrs[i] = 0;
	}
      prg_ctr_mask |= fn_ctr_mask;
      fn_ctr_mask = 0;
    }
  bbg_function_announced = 0;
  fn_ident++;
}

/* Creates the gcov_fn_info RECORD_TYPE.  */

static tree
build_fn_info_type (counters)
     unsigned counters;
{
  tree type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  tree field, fields;
  tree array_type;
  
  /* ident */
  fields = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);

  /* checksum */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  array_type = build_index_type (build_int_2 (counters - 1, 0));
  array_type = build_array_type (unsigned_type_node, array_type);
  
  /* counters */
  field = build_decl (FIELD_DECL, NULL_TREE, array_type);
  TREE_CHAIN (field) = fields;
  fields = field;

  finish_builtin_struct (type, "__gcov_fn_info", fields, NULL_TREE);

  return type;
}

/* Creates a CONSTRUCTOR for a gcov_fn_info. FUNCTION is
   the function being processed and TYPE is the gcov_fn_info
   RECORD_TYPE.  */

static tree
build_fn_info_value (function, type)
     const struct function_list *function;
     tree type;
{
  tree value = NULL_TREE;
  tree fields = TYPE_FIELDS (type);
  unsigned ix;
  tree array_value = NULL_TREE;
  
  /* ident */
  value = tree_cons (fields,
		     convert (unsigned_type_node,
			      build_int_2 (function->ident, 0)),
		     value);
  fields = TREE_CHAIN (fields);
  
  /* checksum */
  value = tree_cons (fields,
		     convert (unsigned_type_node,
			      build_int_2 (function->checksum, 0)),
		     value);
  fields = TREE_CHAIN (fields);
  
  /* counters */
  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if (prg_ctr_mask & (1 << ix))
      {
	tree counters = convert (unsigned_type_node,
				 build_int_2 (function->n_ctrs[ix], 0));
	
	array_value = tree_cons (NULL_TREE, counters, array_value);
      }
  
  array_value = build_constructor (TREE_TYPE (fields), nreverse (array_value));
  value = tree_cons (fields, array_value, value);

  value = build_constructor (type, nreverse (value));
  
  return value;
}

/* Creates the gcov_ctr_info RECORD_TYPE.  */

static tree
build_ctr_info_type ()
{
  tree type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  tree field, fields = NULL_TREE;
  
  /* counters */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  /* values */
  field = build_decl (FIELD_DECL, NULL_TREE,
		      build_pointer_type (make_signed_type (GCOV_TYPE_SIZE)));
  TREE_CHAIN (field) = fields;
  fields = field;

  finish_builtin_struct (type, "__gcov_ctr_info", fields, NULL_TREE);

  return type;
}

/* Creates a CONSTRUCTOR for a gcov_ctr_info. COUNTER is
   the counter being processed and TYPE is the gcov_ctr_info
   RECORD_TYPE.  */

static tree
build_ctr_info_value (counter, type)
     unsigned counter;
     tree type;
{
  tree value = NULL_TREE;
  tree fields = TYPE_FIELDS (type);

  /* counters */
  value = tree_cons (fields,
		     convert (unsigned_type_node,
			      build_int_2 (prg_n_ctrs[counter], 0)),
		     value);
  fields = TREE_CHAIN (fields);

  if (prg_n_ctrs[counter])
    {
      tree array_type, array;
      
      array_type = build_index_type (build_int_2 (prg_n_ctrs[counter] - 1, 0));
      array_type = build_array_type (TREE_TYPE (TREE_TYPE (fields)),
				     array_type);
      
      array = build (VAR_DECL, array_type, NULL_TREE, NULL_TREE);
      TREE_STATIC (array) = 1;
      DECL_NAME (array) = get_identifier (XSTR (ctr_labels[counter], 0));
      assemble_variable (array, 0, 0, 0);
      
      value = tree_cons (fields,
			 build1 (ADDR_EXPR, TREE_TYPE (fields), array),
			 value);
    }
  else
    value = tree_cons (fields, null_pointer_node, value);

  value = build_constructor (type, nreverse (value));
  
  return value;
}

/* Creates the gcov_info RECORD_TYPE and initializer for it. Returns a
   CONSTRUCTOR.  */

static tree
build_gcov_info ()
{
  unsigned n_ctr_types, ix;
  tree type, const_type;
  tree fn_info_type, fn_info_value = NULL_TREE;
  tree fn_info_ptr_type;
  tree ctr_info_type, ctr_info_ary_type, ctr_info_value = NULL_TREE;
  tree field, fields = NULL_TREE;
  tree value = NULL_TREE;
  tree filename_string;
  char *filename;
  int filename_len;
  unsigned n_fns;
  const struct function_list *fn;
  tree string_type;
  
  /* Count the number of active counters.  */
  for (n_ctr_types = 0, ix = 0; ix != GCOV_COUNTERS; ix++)
    if (prg_ctr_mask & (1 << ix))
      n_ctr_types++;
  
  type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  const_type = build_qualified_type (type, TYPE_QUAL_CONST);
  
  /* Version ident */
  field = build_decl (FIELD_DECL, NULL_TREE, long_unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field, convert (long_unsigned_type_node,
				     build_int_2 (GCOV_VERSION, 0)),
		     value);
  
  /* next -- NULL */
  field = build_decl (FIELD_DECL, NULL_TREE, build_pointer_type (const_type));
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field, null_pointer_node, value);
  
  /* Filename */
  string_type = build_pointer_type (build_qualified_type (char_type_node,
						    TYPE_QUAL_CONST));
  field = build_decl (FIELD_DECL, NULL_TREE, string_type);
  TREE_CHAIN (field) = fields;
  fields = field;
  filename = getpwd ();
  filename = (filename && da_file_name[0] != '/'
	      ? concat (filename, "/", da_file_name, NULL)
	      : da_file_name);
  filename_len = strlen (filename);
  filename_string = build_string (filename_len + 1, filename);
  if (filename != da_file_name)
    free (filename);
  TREE_TYPE (filename_string) =
	  build_array_type (char_type_node,
			    build_index_type (build_int_2 (filename_len, 0)));
  value = tree_cons (field, build1 (ADDR_EXPR, string_type, filename_string),
		     value);
  
  /* Build the fn_info type and initializer.  */
  fn_info_type = build_fn_info_type (n_ctr_types);
  fn_info_ptr_type = build_pointer_type (build_qualified_type
					 (fn_info_type, TYPE_QUAL_CONST));
  for (fn = functions_head, n_fns = 0; fn; fn = fn->next, n_fns++)
    fn_info_value = tree_cons (NULL_TREE,
			       build_fn_info_value (fn, fn_info_type),
			       fn_info_value);
  if (n_fns)
    {
      tree array_type;

      array_type = build_index_type (build_int_2 (n_fns - 1, 0));
      array_type = build_array_type (fn_info_type, array_type);
      
      fn_info_value = build_constructor (array_type, nreverse (fn_info_value));
      fn_info_value = build1 (ADDR_EXPR, fn_info_ptr_type, fn_info_value);
    }
  else
    fn_info_value = null_pointer_node;
  
  /* number of functions */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field,
		     convert (unsigned_type_node, build_int_2 (n_fns, 0)),
		     value);
  
  /* fn_info table */
  field = build_decl (FIELD_DECL, NULL_TREE, fn_info_ptr_type);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field, fn_info_value, value);

  /* counter_mask */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field,
		     convert (unsigned_type_node,
			      build_int_2 (prg_ctr_mask, 0)),
		     value);
  
  /* counters */
  ctr_info_type = build_ctr_info_type ();
  ctr_info_ary_type = build_index_type (build_int_2 (n_ctr_types, 0));
  ctr_info_ary_type = build_array_type (ctr_info_type, ctr_info_ary_type);
  for (ix = 0; ix != GCOV_COUNTERS; ix++)
    if (prg_ctr_mask & (1 << ix))
      ctr_info_value = tree_cons (NULL_TREE,
				  build_ctr_info_value (ix, ctr_info_type),
				  ctr_info_value);
  ctr_info_value = build_constructor (ctr_info_ary_type,
				      nreverse (ctr_info_value));

  field = build_decl (FIELD_DECL, NULL_TREE, ctr_info_ary_type);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (field, ctr_info_value, value);
  
  finish_builtin_struct (type, "__gcov_info", fields, NULL_TREE);

  value = build_constructor (type, nreverse (value));
  
  return value;
}

/* Write out the structure which libgcov uses to locate all the
   counters.  The structures used here must match those defined in
   gcov-io.h.  Write out the constructor to call __gcov_init.  */

static void
create_coverage ()
{
  tree gcov_info, gcov_info_value;
  char name[20];
  char *ctor_name;
  tree ctor;
  rtx gcov_info_address;
  int save_flag_inline_functions = flag_inline_functions;

  if (!prg_ctr_mask)
    return;
  
  gcov_info_value = build_gcov_info ();

  gcov_info = build (VAR_DECL, TREE_TYPE (gcov_info_value),
		     NULL_TREE, NULL_TREE);
  DECL_INITIAL (gcov_info) = gcov_info_value;

  TREE_STATIC (gcov_info) = 1;
  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 0);
  DECL_NAME (gcov_info) = get_identifier (name);
  
  /* Build structure.  */
  assemble_variable (gcov_info, 0, 0, 0);

  /* Build the constructor function to invoke __gcov_init.  */
  ctor_name = concat (IDENTIFIER_POINTER (get_file_function_name ('I')),
		      "_GCOV", NULL);
  ctor = build_decl (FUNCTION_DECL, get_identifier (ctor_name),
		     build_function_type (void_type_node, NULL_TREE));
  free (ctor_name);
  DECL_EXTERNAL (ctor) = 0;

  /* It can be a static function as long as collect2 does not have
     to scan the object file to find its ctor/dtor routine.  */
  TREE_PUBLIC (ctor) = ! targetm.have_ctors_dtors;
  TREE_USED (ctor) = 1;
  DECL_RESULT (ctor) = build_decl (RESULT_DECL, NULL_TREE, void_type_node);

  ctor = (*lang_hooks.decls.pushdecl) (ctor);
  rest_of_decl_compilation (ctor, 0, 1, 0);
  announce_function (ctor);
  current_function_decl = ctor;
  DECL_INITIAL (ctor) = error_mark_node;
  make_decl_rtl (ctor, NULL);
  init_function_start (ctor, input_filename, input_line);
  (*lang_hooks.decls.pushlevel) (0);
  expand_function_start (ctor, 0);
  cfun->arc_profile = 0;

  /* Actually generate the code to call __gcov_init.  */
  gcov_info_address = force_reg (Pmode, XEXP (DECL_RTL (gcov_info), 0));
  emit_library_call (gcov_init_libfunc, LCT_NORMAL, VOIDmode, 1,
		     gcov_info_address, Pmode);

  expand_function_end (input_filename, input_line, 0);
  (*lang_hooks.decls.poplevel) (1, 0, 1);

  /* Since ctor isn't in the list of globals, it would never be emitted
     when it's considered to be 'safe' for inlining, so turn off
     flag_inline_functions.  */
  flag_inline_functions = 0;

  rest_of_compilation (ctor);

  /* Reset flag_inline_functions to its original value.  */
  flag_inline_functions = save_flag_inline_functions;

  if (! quiet_flag)
    fflush (asm_out_file);
  current_function_decl = NULL_TREE;

  if (targetm.have_ctors_dtors)
    (* targetm.asm_out.constructor) (XEXP (DECL_RTL (ctor), 0),
				     DEFAULT_INIT_PRIORITY);
}

/* Perform file-level initialization. Read in data file, generate name
   of graph file. */

void
coverage_init (filename)
  const char *filename;
{
  int len = strlen (filename);

  /* Name of da file.  */
  da_file_name = (char *) xmalloc (len + strlen (GCOV_DATA_SUFFIX) + 1);
  strcpy (da_file_name, filename);
  strcat (da_file_name, GCOV_DATA_SUFFIX);
  
  /* Name of bbg file.  */
  bbg_file_name = (char *) xmalloc (len + strlen (GCOV_GRAPH_SUFFIX) + 1);
  strcpy (bbg_file_name, filename);
  strcat (bbg_file_name, GCOV_GRAPH_SUFFIX);

  read_counts_file ();
}

/* Performs file-level cleanup.  Close graph file, generate coverage
   variables and constructor.  */

void
coverage_finish ()
{
  create_coverage ();
  if (bbg_file_opened)
    {
      int error = gcov_close ();
      
      if (error)
	unlink (bbg_file_name);
#if SELF_COVERAGE
      /* If the compiler is instrumented, we should not
         unconditionally remove the counts file, because we might be
         recompiling ourselves. The .da files are all removed during
         copying the stage1 files.  */
      if (error)
#endif
	unlink (da_file_name);
    }
}

#include "gt-coverage.h"
