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
  struct function_list *next; 	/* next function */
  const char *name; 		/* function name */
  unsigned cfg_checksum;	/* function checksum */
  unsigned n_counter_sections;	/* number of counter sections */
  struct gcov_counter_section counter_sections[MAX_COUNTER_SECTIONS];
  				/* the sections */
};

/* Counts information for a function.  */
typedef struct counts_entry
{
  /* We hash by  */
  char *function_name;
  unsigned section;
  
  /* Store  */
  unsigned checksum;
  unsigned n_counts;
  gcov_type *counts;
  unsigned merged;
  gcov_type max_counter;
  gcov_type max_counter_sum;

  /* Workspace */
  struct counts_entry *chain;
  
} counts_entry_t;

static struct function_list *functions_head = 0;
static struct function_list **functions_tail = &functions_head;

/* Instantiate the profile info structure.  */

struct profile_info profile_info;

/* Name of the output file for coverage output file.  */
static char *bbg_file_name;
static unsigned bbg_file_opened;
static int bbg_function_announced;

/* Name of the count data file.  */
static char *da_file_name;

/* Hash table of count data.  */
static htab_t counts_hash = NULL;

/* The name of the count table. Used by the edge profiling code.  */
static GTY(()) rtx profiler_label;

/* Forward declarations.  */
static hashval_t htab_counts_entry_hash PARAMS ((const void *));
static int htab_counts_entry_eq PARAMS ((const void *, const void *));
static void htab_counts_entry_del PARAMS ((void *));
static void read_counts_file PARAMS ((void));
static unsigned compute_checksum PARAMS ((void));
static unsigned checksum_string PARAMS ((unsigned, const char *));
static void set_purpose PARAMS ((tree, tree));
static rtx label_for_tag PARAMS ((unsigned));
static tree build_counter_section_fields PARAMS ((void));
static tree build_counter_section_value PARAMS ((unsigned, unsigned));
static tree build_counter_section_data_fields PARAMS ((void));
static tree build_counter_section_data_value PARAMS ((unsigned, unsigned));
static tree build_function_info_fields PARAMS ((void));
static tree build_function_info_value PARAMS ((struct function_list *));
static tree build_gcov_info_fields PARAMS ((tree));
static tree build_gcov_info_value PARAMS ((void));
static void create_coverage PARAMS ((void));


static hashval_t
htab_counts_entry_hash (of)
     const void *of;
{
  const counts_entry_t *entry = of;

  return htab_hash_string (entry->function_name) ^ entry->section;
}

static int
htab_counts_entry_eq (of1, of2)
     const void *of1;
     const void *of2;
{
  const counts_entry_t *entry1 = of1;
  const counts_entry_t *entry2 = of2;

  return !strcmp (entry1->function_name, entry2->function_name)
    && entry1->section == entry2->section;
}

static void
htab_counts_entry_del (of)
     void *of;
{
  counts_entry_t *entry = of;

  free (entry->function_name);
  free (entry->counts);
  free (entry);
}

/* Read in the counts file, if available.  */

static void
read_counts_file ()
{
  char *function_name_buffer = NULL;
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
	  const char *string = gcov_read_string ();
	  free (function_name_buffer);
	  function_name_buffer = string ? xstrdup (string) : NULL;
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
		  
		  entry->max_counter_sum += entry->max_counter;
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
	      entry->merged += summary.runs;
	      if (entry->max_counter < summary.arc_sum_max)
		entry->max_counter = summary.arc_sum_max;
	    }
	}
      else if (GCOV_TAG_IS_SUBTAG (GCOV_TAG_FUNCTION, tag)
	       && function_name_buffer)
	{
	  counts_entry_t **slot, *entry, elt;
	  unsigned n_counts = length / 8;
	  unsigned ix;

	  elt.function_name = function_name_buffer;
	  elt.section = tag;

	  slot = (counts_entry_t **) htab_find_slot
	    (counts_hash, &elt, INSERT);
	  entry = *slot;
	  if (!entry)
	    {
	      *slot = entry = xmalloc (sizeof (counts_entry_t));
	      entry->function_name = xstrdup (function_name_buffer);
	      entry->section = tag;
	      entry->checksum = checksum;
	      entry->n_counts = n_counts;
	      entry->counts = xcalloc (n_counts, sizeof (gcov_type));
	    }
	  else if (entry->checksum != checksum || entry->n_counts != n_counts)
	    {
	      warning ("profile mismatch for `%s'", function_name_buffer);
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

  free (function_name_buffer);
  gcov_close ();
}

/* Returns the counters for a particular tag.  */

gcov_type *
get_coverage_counts (unsigned tag, unsigned expected)
{
  counts_entry_t *entry, elt;

  profile_info.max_counter_in_program = 0;
  profile_info.count_profiles_merged = 0;

  /* No hash table, no counts. */
  if (!counts_hash)
    {
      static int warned = 0;

      if (!warned++)
	warning ("file %s not found, execution counts assumed to be zero",
		 da_file_name);
      return NULL;
    }

  elt.function_name
    = (char *) IDENTIFIER_POINTER
    (DECL_ASSEMBLER_NAME (current_function_decl));
  elt.section = tag;
  entry = htab_find (counts_hash, &elt);
  if (!entry)
    {
      warning ("No profile for function '%s' found.", elt.function_name);
      return 0;
    }
  
  if (expected != entry->n_counts
      || compute_checksum () != entry->checksum)
    {
      warning ("profile mismatch for `%s'", elt.function_name);
      return NULL;
    }

  profile_info.count_profiles_merged = entry->merged;
  profile_info.max_counter_in_program = entry->max_counter_sum;

  return entry->counts;
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
      gcov_write_string (IDENTIFIER_POINTER
			 (DECL_ASSEMBLER_NAME (current_function_decl)));
      gcov_write_unsigned (compute_checksum ());
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
  
  for (i = 0; i != profile_info.n_sections; i++)
    if (profile_info.section_info[i].n_counters_now)
      {
	struct function_list *item;
      
	/* ??? Probably should re-use the existing struct function.  */
	item = xmalloc (sizeof (struct function_list));
      
	*functions_tail = item;
	functions_tail = &item->next;
	
	item->next = 0;
	item->name = xstrdup (IDENTIFIER_POINTER
			      (DECL_ASSEMBLER_NAME (current_function_decl)));
	item->cfg_checksum = compute_checksum ();
	item->n_counter_sections = 0;
	for (i = 0; i < profile_info.n_sections; i++)
	  if (profile_info.section_info[i].n_counters_now)
	    {
	      item->counter_sections[item->n_counter_sections].tag = 
		profile_info.section_info[i].tag;
	      item->counter_sections[item->n_counter_sections].n_counters =
		profile_info.section_info[i].n_counters_now;
	      item->n_counter_sections++;
	      profile_info.section_info[i].n_counters
		+= profile_info.section_info[i].n_counters_now;
	      profile_info.section_info[i].n_counters_now = 0;
	    }
	break;
      }
  bbg_function_announced = 0;
}

/* Set FIELDS as purpose to VALUE.  */
static void
set_purpose (value, fields)
     tree value;
     tree fields;
{
  tree act_field, act_value;
  
  for (act_field = fields, act_value = value;
       act_field;
       act_field = TREE_CHAIN (act_field), act_value = TREE_CHAIN (act_value))
    TREE_PURPOSE (act_value) = act_field;
}

/* Returns label for base of counters inside TAG section.  */
static rtx
label_for_tag (tag)
     unsigned tag;
{
  switch (tag)
    {
    case GCOV_TAG_ARC_COUNTS:
      return profiler_label;
    default:
      abort ();
    }
}

/* Creates fields of struct counter_section (in gcov-io.h).  */
static tree
build_counter_section_fields ()
{
  tree field, fields;

  /* tag */
  fields = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);

  /* n_counters */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  return fields;
}

/* Creates value of struct counter_section (in gcov-io.h).  */
static tree
build_counter_section_value (tag, n_counters)
     unsigned tag;
     unsigned n_counters;
{
  tree value = NULL_TREE;

  /* tag */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (tag, 0)),
		     value);
  
  /* n_counters */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (n_counters, 0)),
		     value);

  return value;
}

/* Creates fields of struct counter_section_data (in gcov-io.h).  */
static tree
build_counter_section_data_fields ()
{
  tree field, fields, gcov_type, gcov_ptr_type;

  gcov_type = make_signed_type (GCOV_TYPE_SIZE);
  gcov_ptr_type =
	  build_pointer_type (build_qualified_type (gcov_type,
						    TYPE_QUAL_CONST));

  /* tag */
  fields = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);

  /* n_counters */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  /* counters */
  field = build_decl (FIELD_DECL, NULL_TREE, gcov_ptr_type);
  TREE_CHAIN (field) = fields;
  fields = field;

  return fields;
}

/* Creates value of struct counter_section_data (in gcov-io.h).  */
static tree
build_counter_section_data_value (tag, n_counters)
     unsigned tag;
     unsigned n_counters;
{
  tree value = NULL_TREE, counts_table, gcov_type, gcov_ptr_type;

  gcov_type = make_signed_type (GCOV_TYPE_SIZE);
  gcov_ptr_type
    = build_pointer_type (build_qualified_type
			  (gcov_type, TYPE_QUAL_CONST));

  /* tag */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (tag, 0)),
		     value);
  
  /* n_counters */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (n_counters, 0)),
		     value);

  /* counters */
  if (n_counters)
    {
      tree gcov_type_array_type =
	      build_array_type (gcov_type,
				build_index_type (build_int_2 (n_counters - 1,
							       0)));
      counts_table =
	      build (VAR_DECL, gcov_type_array_type, NULL_TREE, NULL_TREE);
      TREE_STATIC (counts_table) = 1;
      DECL_NAME (counts_table) = get_identifier (XSTR (label_for_tag (tag), 0));
      assemble_variable (counts_table, 0, 0, 0);
      counts_table = build1 (ADDR_EXPR, gcov_ptr_type, counts_table);
    }
  else
    counts_table = null_pointer_node;

  value = tree_cons (NULL_TREE, counts_table, value);

  return value;
}

/* Creates fields for struct function_info type (in gcov-io.h).  */
static tree
build_function_info_fields ()
{
  tree field, fields, counter_section_fields, counter_section_type;
  tree counter_sections_ptr_type;
  tree string_type =
	  build_pointer_type (build_qualified_type (char_type_node,
						    TYPE_QUAL_CONST));
  /* name */
  fields = build_decl (FIELD_DECL, NULL_TREE, string_type);

  /* checksum */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  /* n_counter_sections */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  /* counter_sections */
  counter_section_fields = build_counter_section_fields ();
  counter_section_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  finish_builtin_struct (counter_section_type, "__counter_section",
			 counter_section_fields, NULL_TREE);
  counter_sections_ptr_type =
	  build_pointer_type
	  	(build_qualified_type (counter_section_type,
				       TYPE_QUAL_CONST));
  field = build_decl (FIELD_DECL, NULL_TREE, counter_sections_ptr_type);
  TREE_CHAIN (field) = fields;
  fields = field;

  return fields;
}

/* Creates value for struct function_info (in gcov-io.h).  */
static tree
build_function_info_value (function)
     struct function_list *function;
{
  tree value = NULL_TREE;
  size_t name_len = strlen (function->name);
  tree fname = build_string (name_len + 1, function->name);
  tree string_type =
	  build_pointer_type (build_qualified_type (char_type_node,
						    TYPE_QUAL_CONST));
  tree counter_section_fields, counter_section_type, counter_sections_value;
  tree counter_sections_ptr_type, counter_sections_array_type;
  unsigned i;

  /* name */
  TREE_TYPE (fname) =
	  build_array_type (char_type_node,
			    build_index_type (build_int_2 (name_len, 0)));
  value = tree_cons (NULL_TREE,
		     build1 (ADDR_EXPR,
			     string_type,
			     fname),
		     value);

  /* checksum */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (function->cfg_checksum, 0)),
		     value);

  /* n_counter_sections */

  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (function->n_counter_sections, 0)),
	    	    value);

  /* counter_sections */
  counter_section_fields = build_counter_section_fields ();
  counter_section_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  counter_sections_ptr_type =
	  build_pointer_type
	  	(build_qualified_type (counter_section_type,
				       TYPE_QUAL_CONST));
  counter_sections_array_type =
	  build_array_type (counter_section_type,
			    build_index_type (
      				build_int_2 (function->n_counter_sections - 1,
		  			     0)));

  counter_sections_value = NULL_TREE;
  for (i = 0; i < function->n_counter_sections; i++)
    {
      tree counter_section_value
	= build_counter_section_value (function->counter_sections[i].tag,
				       function->counter_sections[i].n_counters);
      set_purpose (counter_section_value, counter_section_fields);
      counter_sections_value =
	tree_cons (NULL_TREE,
		   build_constructor (counter_section_type,
				      nreverse (counter_section_value)),
		   counter_sections_value);
    }
  finish_builtin_struct (counter_section_type, "__counter_section",
			 counter_section_fields, NULL_TREE);

  if (function->n_counter_sections)
    {
      counter_sections_value = 
	      build_constructor (counter_sections_array_type,
				 nreverse (counter_sections_value)),
      counter_sections_value = build1 (ADDR_EXPR,
				       counter_sections_ptr_type,
				       counter_sections_value);
    }
  else
    counter_sections_value = null_pointer_node;

  value = tree_cons (NULL_TREE, counter_sections_value, value);

  return value;
}

/* Creates fields of struct gcov_info type (in gcov-io.h).  */
static tree
build_gcov_info_fields (gcov_info_type)
     tree gcov_info_type;
{
  tree field, fields;
  char *filename;
  int filename_len;
  tree string_type =
	  build_pointer_type (build_qualified_type (char_type_node,
						    TYPE_QUAL_CONST));
  tree function_info_fields, function_info_type, function_info_ptr_type;
  tree counter_section_data_fields, counter_section_data_type;
  tree counter_section_data_ptr_type;

  /* Version ident */
  fields = build_decl (FIELD_DECL, NULL_TREE, long_unsigned_type_node);

  /* next -- NULL */
  field = build_decl (FIELD_DECL, NULL_TREE,
		      build_pointer_type
		      (build_qualified_type
		       (gcov_info_type, TYPE_QUAL_CONST)));
  TREE_CHAIN (field) = fields;
  fields = field;
  
  /* Filename */
  filename = getpwd ();
  filename = (filename && da_file_name[0] != '/'
	      ? concat (filename, "/", da_file_name, NULL)
	      : da_file_name);
  filename_len = strlen (filename);
  if (filename != da_file_name)
    free (filename);

  field = build_decl (FIELD_DECL, NULL_TREE, string_type);
  TREE_CHAIN (field) = fields;
  fields = field;
  
  /* Workspace */
  field = build_decl (FIELD_DECL, NULL_TREE, long_integer_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;

  /* number of functions */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
      
  /* function_info table */
  function_info_fields = build_function_info_fields ();
  function_info_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  finish_builtin_struct (function_info_type, "__function_info",
			 function_info_fields, NULL_TREE);
  function_info_ptr_type =
	  build_pointer_type
	  	(build_qualified_type (function_info_type,
				       TYPE_QUAL_CONST));
  field = build_decl (FIELD_DECL, NULL_TREE, function_info_ptr_type);
  TREE_CHAIN (field) = fields;
  fields = field;
    
  /* n_counter_sections  */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  
  /* counter sections */
  counter_section_data_fields = build_counter_section_data_fields ();
  counter_section_data_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  finish_builtin_struct (counter_section_data_type, "__counter_section_data",
			 counter_section_data_fields, NULL_TREE);
  counter_section_data_ptr_type =
	  build_pointer_type
	  	(build_qualified_type (counter_section_data_type,
				       TYPE_QUAL_CONST));
  field = build_decl (FIELD_DECL, NULL_TREE, counter_section_data_ptr_type);
  TREE_CHAIN (field) = fields;
  fields = field;

  return fields;
}

/* Creates struct gcov_info value (in gcov-io.h).  */
static tree
build_gcov_info_value ()
{
  tree value = NULL_TREE;
  tree filename_string;
  char *filename;
  int filename_len;
  unsigned n_functions, i;
  struct function_list *item;
  tree string_type =
	  build_pointer_type (build_qualified_type (char_type_node,
						    TYPE_QUAL_CONST));
  tree function_info_fields, function_info_type, function_info_ptr_type;
  tree functions;
  tree counter_section_data_fields, counter_section_data_type;
  tree counter_section_data_ptr_type, counter_sections;

  /* Version ident */
  value = tree_cons (NULL_TREE,
		     convert (long_unsigned_type_node,
			      build_int_2 (GCOV_VERSION, 0)),
		     value);

  /* next -- NULL */
  value = tree_cons (NULL_TREE, null_pointer_node, value);
  
  /* Filename */
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
  value = tree_cons (NULL_TREE,
		     build1 (ADDR_EXPR,
			     string_type,
		       	     filename_string),
		     value);
  
  /* Workspace */
  value = tree_cons (NULL_TREE,
		     convert (long_integer_type_node, integer_zero_node),
		     value);
      
  /* number of functions */
  n_functions = 0;
  for (item = functions_head; item != 0; item = item->next, n_functions++)
    continue;
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (n_functions, 0)),
		     value);

  /* function_info table */
  function_info_fields = build_function_info_fields ();
  function_info_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  function_info_ptr_type =
	  build_pointer_type (
		build_qualified_type (function_info_type,
	       			      TYPE_QUAL_CONST));
  functions = NULL_TREE;
  for (item = functions_head; item != 0; item = item->next)
    {
      tree function_info_value = build_function_info_value (item);
      set_purpose (function_info_value, function_info_fields);
      functions
	= tree_cons (NULL_TREE,
		     build_constructor (function_info_type,
					nreverse (function_info_value)),
		     functions);
    }
  finish_builtin_struct (function_info_type, "__function_info",
			 function_info_fields, NULL_TREE);

  /* Create constructor for array.  */
  if (n_functions)
    {
      tree array_type;

      array_type = build_array_type (
			function_info_type,
   			build_index_type (build_int_2 (n_functions - 1, 0)));
      functions = build_constructor (array_type, nreverse (functions));
      functions = build1 (ADDR_EXPR,
			  function_info_ptr_type,
			  functions);
    }
  else
    functions = null_pointer_node;

  value = tree_cons (NULL_TREE, functions, value);

  /* n_counter_sections  */
  value = tree_cons (NULL_TREE,
		     convert (unsigned_type_node,
			      build_int_2 (profile_info.n_sections, 0)),
		     value);
  
  /* counter sections */
  counter_section_data_fields = build_counter_section_data_fields ();
  counter_section_data_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  counter_sections = NULL_TREE;
  for (i = 0; i < profile_info.n_sections; i++)
    {
      tree counter_sections_value =
	      build_counter_section_data_value (
		profile_info.section_info[i].tag,
		profile_info.section_info[i].n_counters);
      set_purpose (counter_sections_value, counter_section_data_fields);
      counter_sections =
	tree_cons (NULL_TREE,
		   build_constructor (counter_section_data_type,
				      nreverse (counter_sections_value)),
		   counter_sections);
    }
  finish_builtin_struct (counter_section_data_type, "__counter_section_data",
			 counter_section_data_fields, NULL_TREE);
  counter_section_data_ptr_type =
	  build_pointer_type
	  	(build_qualified_type (counter_section_data_type,
				       TYPE_QUAL_CONST));

  if (profile_info.n_sections)
    {
      tree cst_type = build_index_type (build_int_2 (profile_info.n_sections-1,
						     0));
      cst_type = build_array_type (counter_section_data_type, cst_type);
      counter_sections = build_constructor (cst_type,
					    nreverse (counter_sections));
      counter_sections = build1 (ADDR_EXPR,
				 counter_section_data_ptr_type,
				 counter_sections);
    }
  else
    counter_sections = null_pointer_node;
  value = tree_cons (NULL_TREE, counter_sections, value);

  return value;
}

/* Write out the structure which libgcc uses to locate all the arc
   counters.  The structures used here must match those defined in
   gcov-io.h.  Write out the constructor to call __gcov_init.  */

static void
create_coverage ()
{
  tree gcov_info_fields, gcov_info_type, gcov_info_value, gcov_info;
  char name[20];
  char *ctor_name;
  tree ctor;
  rtx gcov_info_address;
  int save_flag_inline_functions = flag_inline_functions;
  unsigned i;

  for (i = 0; i < profile_info.n_sections; i++)
    if (profile_info.section_info[i].n_counters)
      break;
  if (i == profile_info.n_sections)
    return;
  
  gcov_info_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  gcov_info_fields = build_gcov_info_fields (gcov_info_type);
  gcov_info_value = build_gcov_info_value ();
  set_purpose (gcov_info_value, gcov_info_fields);
  finish_builtin_struct (gcov_info_type, "__gcov_info",
			 gcov_info_fields, NULL_TREE);

  gcov_info = build (VAR_DECL, gcov_info_type, NULL_TREE, NULL_TREE);
  DECL_INITIAL (gcov_info) =
    build_constructor (gcov_info_type, nreverse (gcov_info_value));

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
  init_function_start (ctor, input_filename, lineno);
  (*lang_hooks.decls.pushlevel) (0);
  expand_function_start (ctor, 0);
  cfun->arc_profile = 0;

  /* Actually generate the code to call __gcov_init.  */
  gcov_info_address = force_reg (Pmode, XEXP (DECL_RTL (gcov_info), 0));
  emit_library_call (gcov_init_libfunc, LCT_NORMAL, VOIDmode, 1,
		     gcov_info_address, Pmode);

  expand_function_end (input_filename, lineno, 0);
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

/* Find (and create if not present) a section with TAG for the current
   function.  */
struct section_info *
find_counters_section (tag)
     unsigned tag;
{
  unsigned i;

  for (i = 0; i < profile_info.n_sections; i++)
    if (profile_info.section_info[i].tag == tag)
      return profile_info.section_info + i;

  if (i == MAX_COUNTER_SECTIONS)
    abort ();

  profile_info.section_info[i].tag = tag;
  profile_info.section_info[i].present = 0;
  profile_info.section_info[i].n_counters = 0;
  profile_info.section_info[i].n_counters_now = 0;
  profile_info.n_sections++;

  return profile_info.section_info + i;
}

/* Generate a MEM rtl to access counter NO in counter section TAG.  */

rtx
coverage_counter_ref (unsigned tag, unsigned no)
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  struct section_info *sect = find_counters_section (tag);
  rtx ref;

  if (!profiler_label)
    {
      /* Generate and save a copy of this so it can be shared.  */
      char buf[20];
      
      ASM_GENERATE_INTERNAL_LABEL (buf, "LPBX", 2);
      profiler_label = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
    }
  if (no + 1 > (unsigned) sect->n_counters_now)
    sect->n_counters_now = no + 1;

  no += sect->n_counters;
  ref = plus_constant (profiler_label, GCOV_TYPE_SIZE / BITS_PER_UNIT * no);
  ref = gen_rtx_MEM (mode, ref);
  set_mem_alias_set (ref, new_alias_set ());

  return ref;
}


/* Perform file-level initialization. Read in data file, generate name
   of graph file. */

void
coverage_init (filename)
  const char *filename;
{
  int len = strlen (filename);

  da_file_name = (char *) xmalloc (len + strlen (GCOV_DATA_SUFFIX) + 1);
  strcpy (da_file_name, filename);
  strcat (da_file_name, GCOV_DATA_SUFFIX);
  
  read_counts_file ();

  /* Open the bbg output file.  */
  bbg_file_name = (char *) xmalloc (len + strlen (GCOV_GRAPH_SUFFIX) + 1);
  strcpy (bbg_file_name, filename);
  strcat (bbg_file_name, GCOV_GRAPH_SUFFIX);
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
