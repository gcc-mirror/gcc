/* Top-level LTO routines.
   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "opts.h"
#include "toplev.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "cgraph.h"
#include "ggc.h"
#include "tree-ssa-operands.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "vec.h"
#include "bitmap.h"
#include "pointer-set.h"
#include "ipa-prop.h"
#include "common.h"
#include "debug.h"
#include "timevar.h"
#include "gimple.h"
#include "lto.h"
#include "lto-tree.h"
#include "lto-streamer.h"
#include "splay-tree.h"
#include "params.h"

static GTY(()) tree first_personality_decl;

/* Returns a hash code for P.  */

static hashval_t
hash_name (const void *p)
{
  const struct lto_section_slot *ds = (const struct lto_section_slot *) p;
  return (hashval_t) htab_hash_string (ds->name);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_name (const void *p1, const void *p2)
{
  const struct lto_section_slot *s1 =
    (const struct lto_section_slot *) p1;
  const struct lto_section_slot *s2 =
    (const struct lto_section_slot *) p2;

  return strcmp (s1->name, s2->name) == 0;
}

/* Free lto_section_slot */

static void
free_with_string (void *arg)
{
  struct lto_section_slot *s = (struct lto_section_slot *)arg;

  free (CONST_CAST (char *, s->name));
  free (arg);
}

/* Create section hash table */

htab_t 
lto_obj_create_section_hash_table (void)
{
  return htab_create (37, hash_name, eq_name, free_with_string);
}

/* Read the constructors and inits.  */

static void
lto_materialize_constructors_and_inits (struct lto_file_decl_data * file_data)
{
  size_t len;
  const char *data = lto_get_section_data (file_data, 
					   LTO_section_static_initializer,
					   NULL, &len);
  lto_input_constructors_and_inits (file_data, data);
  lto_free_section_data (file_data, LTO_section_static_initializer, NULL,
			 data, len);
}

/* Return true when NODE has a clone that is analyzed (i.e. we need
   to load its body even if the node itself is not needed).  */

static bool
has_analyzed_clone_p (struct cgraph_node *node)
{
  struct cgraph_node *orig = node;
  node = node->clones;
  if (node)
    while (node != orig)
      {
	if (node->analyzed)
	  return true;
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
  return false;
}

/* Read the function body for the function associated with NODE.  */

static void
lto_materialize_function (struct cgraph_node *node)
{
  tree decl;
  struct lto_file_decl_data *file_data;
  const char *data, *name;
  size_t len;

  decl = node->decl;
  /* Read in functions with body (analyzed nodes)
     and also functions that are needed to produce virtual clones.  */
  if (node->analyzed || has_analyzed_clone_p (node))
    {
      /* Clones don't need to be read.  */
      if (node->clone_of)
	return;
      file_data = node->local.lto_file_data;
      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)); 

      /* We may have renamed the declaration, e.g., a static function.  */
      name = lto_get_decl_name_mapping (file_data, name);

      data = lto_get_section_data (file_data, LTO_section_function_body,
				   name, &len);
      if (!data)
	fatal_error ("%s: section %s is missing",
		     file_data->file_name,
		     name);

      gcc_assert (DECL_STRUCT_FUNCTION (decl) == NULL);

      /* Load the function body only if not operating in WPA mode.  In
	 WPA mode, the body of the function is not needed.  */
      if (!flag_wpa)
	{
	  allocate_struct_function (decl, false);
	  announce_function (decl);
	  lto_input_function_body (file_data, decl, data);
	  if (DECL_FUNCTION_PERSONALITY (decl) && !first_personality_decl)
	    first_personality_decl = DECL_FUNCTION_PERSONALITY (decl);
	  lto_stats.num_function_bodies++;
	}

      lto_free_section_data (file_data, LTO_section_function_body, name,
			     data, len);
      if (!flag_wpa)
	ggc_collect ();
    }

  /* Let the middle end know about the function.  */
  rest_of_decl_compilation (decl, 1, 0);
}


/* Decode the content of memory pointed to by DATA in the the
   in decl state object STATE. DATA_IN points to a data_in structure for
   decoding. Return the address after the decoded object in the input.  */

static const uint32_t *
lto_read_in_decl_state (struct data_in *data_in, const uint32_t *data,
			struct lto_in_decl_state *state)
{
  uint32_t ix;
  tree decl;
  uint32_t i, j;

  ix = *data++;
  decl = lto_streamer_cache_get (data_in->reader_cache, (int) ix);
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      gcc_assert (decl == void_type_node);
      decl = NULL_TREE;
    }
  state->fn_decl = decl;

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      uint32_t size = *data++;
      tree *decls = ggc_alloc_vec_tree (size);

      for (j = 0; j < size; j++)
	{
	  decls[j] = lto_streamer_cache_get (data_in->reader_cache, data[j]);

	  /* Register every type in the global type table.  If the
	     type existed already, use the existing type.  */
	  if (TYPE_P (decls[j]))
	    decls[j] = gimple_register_type (decls[j]);
	}

      state->streams[i].size = size;
      state->streams[i].trees = decls;
      data += size;
    }

  return data;
}


/* Read all the symbols from buffer DATA, using descriptors in DECL_DATA.
   RESOLUTIONS is the set of symbols picked by the linker (read from the
   resolution file when the linker plugin is being used).  */

static void
lto_read_decls (struct lto_file_decl_data *decl_data, const void *data,
		VEC(ld_plugin_symbol_resolution_t,heap) *resolutions)
{
  const struct lto_decl_header *header = (const struct lto_decl_header *) data;
  const int decl_offset = sizeof (struct lto_decl_header);
  const int main_offset = decl_offset + header->decl_state_size;
  const int string_offset = main_offset + header->main_size;
  struct lto_input_block ib_main;
  struct data_in *data_in;
  unsigned int i;
  const uint32_t *data_ptr, *data_end;
  uint32_t num_decl_states;

  LTO_INIT_INPUT_BLOCK (ib_main, (const char *) data + main_offset, 0,
			header->main_size);

  data_in = lto_data_in_create (decl_data, (const char *) data + string_offset,
				header->string_size, resolutions);

  /* Read the global declarations and types.  */
  while (ib_main.p < ib_main.len)
    {
      tree t = lto_input_tree (&ib_main, data_in);
      gcc_assert (t && ib_main.p <= ib_main.len);
    }

  /* Read in lto_in_decl_state objects.  */
  data_ptr = (const uint32_t *) ((const char*) data + decl_offset); 
  data_end =
     (const uint32_t *) ((const char*) data_ptr + header->decl_state_size);
  num_decl_states = *data_ptr++;
  
  gcc_assert (num_decl_states > 0);
  decl_data->global_decl_state = lto_new_in_decl_state ();
  data_ptr = lto_read_in_decl_state (data_in, data_ptr,
				     decl_data->global_decl_state);

  /* Read in per-function decl states and enter them in hash table.  */
  decl_data->function_decl_states =
    htab_create_ggc (37, lto_hash_in_decl_state, lto_eq_in_decl_state, NULL);

  for (i = 1; i < num_decl_states; i++)
    {
      struct lto_in_decl_state *state = lto_new_in_decl_state ();
      void **slot;

      data_ptr = lto_read_in_decl_state (data_in, data_ptr, state);
      slot = htab_find_slot (decl_data->function_decl_states, state, INSERT);
      gcc_assert (*slot == NULL);
      *slot = state;
    }

  if (data_ptr != data_end)
    internal_error ("bytecode stream: garbage at the end of symbols section");

  /* Set the current decl state to be the global state. */
  decl_data->current_decl_state = decl_data->global_decl_state;

  lto_data_in_delete (data_in);
}

/* Custom version of strtoll, which is not portable.  */

static HOST_WIDEST_INT
lto_parse_hex (const char *p)
{
  HOST_WIDEST_INT ret = 0;

  for (; *p != '\0'; ++p)
    {
      char c = *p;
      unsigned char part;
      ret <<= 4;
      if (c >= '0' && c <= '9')
        part = c - '0';
      else if (c >= 'a' && c <= 'f')
        part = c - 'a' + 10;
      else if (c >= 'A' && c <= 'F')
        part = c - 'A' + 10;
      else
        internal_error ("could not parse hex number");
      ret |= part;
    }

  return ret;
}

/* Read resolution for file named FILE_NAME. The resolution is read from
   RESOLUTION. */

static void
lto_resolution_read (splay_tree file_ids, FILE *resolution, lto_file *file)
{
  /* We require that objects in the resolution file are in the same
     order as the lto1 command line. */
  unsigned int name_len;
  char *obj_name;
  unsigned int num_symbols;
  unsigned int i;
  struct lto_file_decl_data *file_data;
  unsigned max_index = 0;
  splay_tree_node nd = NULL; 

  if (!resolution)
    return;

  name_len = strlen (file->filename);
  obj_name = XNEWVEC (char, name_len + 1);
  fscanf (resolution, " ");   /* Read white space. */

  fread (obj_name, sizeof (char), name_len, resolution);
  obj_name[name_len] = '\0';
  if (strcmp (obj_name, file->filename) != 0)
    internal_error ("unexpected file name %s in linker resolution file. "
		    "Expected %s", obj_name, file->filename);
  if (file->offset != 0)
    {
      int t;
      char offset_p[17];
      HOST_WIDEST_INT offset;
      t = fscanf (resolution, "@0x%16s", offset_p);
      if (t != 1)
        internal_error ("could not parse file offset");
      offset = lto_parse_hex (offset_p);
      if (offset != file->offset)
        internal_error ("unexpected offset");
    }

  free (obj_name);

  fscanf (resolution, "%u", &num_symbols);

  for (i = 0; i < num_symbols; i++)
    {
      int t;
      unsigned index, id;
      char r_str[27];
      enum ld_plugin_symbol_resolution r = (enum ld_plugin_symbol_resolution) 0;
      unsigned int j;
      unsigned int lto_resolution_str_len =
	sizeof (lto_resolution_str) / sizeof (char *);

      t = fscanf (resolution, "%u %x %26s %*[^\n]\n", &index, &id, r_str);
      if (t != 3)
        internal_error ("invalid line in the resolution file");
      if (index > max_index)
	max_index = index;

      for (j = 0; j < lto_resolution_str_len; j++)
	{
	  if (strcmp (lto_resolution_str[j], r_str) == 0)
	    {
	      r = (enum ld_plugin_symbol_resolution) j;
	      break;
	    }
	}
      if (j == lto_resolution_str_len)
	internal_error ("invalid resolution in the resolution file");

      if (!(nd && nd->key == id))
	{
	  nd = splay_tree_lookup (file_ids, id);
	  if (nd == NULL)
	    internal_error ("resolution sub id %x not in object file", id);
	}

      file_data = (struct lto_file_decl_data *)nd->value;
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, "Adding resolution %u %u to id %x\n",
		 index, r, file_data->id);
      VEC_safe_grow_cleared (ld_plugin_symbol_resolution_t, heap, 
			     file_data->resolutions,
			     max_index + 1);
      VEC_replace (ld_plugin_symbol_resolution_t, 
		   file_data->resolutions, index, r);
    }
}

/* Is the name for a id'ed LTO section? */

static int 
lto_section_with_id (const char *name, unsigned *id)
{
  const char *s;

  if (strncmp (name, LTO_SECTION_NAME_PREFIX, strlen (LTO_SECTION_NAME_PREFIX)))
    return 0;
  s = strrchr (name, '.');
  return s && sscanf (s, ".%x", id) == 1;
}

/* Create file_data of each sub file id */

static int 
create_subid_section_table (void **slot, void *data)
{
  struct lto_section_slot s_slot, *new_slot;
  struct lto_section_slot *ls = *(struct lto_section_slot **)slot;
  splay_tree file_ids = (splay_tree)data;
  unsigned id;
  splay_tree_node nd;
  void **hash_slot;
  char *new_name;
  struct lto_file_decl_data *file_data;

  if (!lto_section_with_id (ls->name, &id))
    return 1;
  
  /* Find hash table of sub module id */
  nd = splay_tree_lookup (file_ids, id);
  if (nd != NULL)
    {
      file_data = (struct lto_file_decl_data *)nd->value;
    }
  else
    {
      file_data = ggc_alloc_lto_file_decl_data ();
      memset(file_data, 0, sizeof (struct lto_file_decl_data));
      file_data->id = id;
      file_data->section_hash_table = lto_obj_create_section_hash_table ();;
      splay_tree_insert (file_ids, id, (splay_tree_value)file_data);
    }

  /* Copy section into sub module hash table */
  new_name = XDUPVEC (char, ls->name, strlen (ls->name) + 1);
  s_slot.name = new_name;
  hash_slot = htab_find_slot (file_data->section_hash_table, &s_slot, INSERT);
  gcc_assert (*hash_slot == NULL);

  new_slot = XDUP (struct lto_section_slot, ls);
  new_slot->name = new_name;
  *hash_slot = new_slot;
  return 1;
}

/* Read declarations and other initializations for a FILE_DATA. */

static void
lto_file_finalize (struct lto_file_decl_data *file_data, lto_file *file)
{
  const char *data;
  size_t len;

  file_data->renaming_hash_table = lto_create_renaming_table ();
  file_data->file_name = file->filename;
  data = lto_get_section_data (file_data, LTO_section_decls, NULL, &len);
  if (data == NULL)
    {
      internal_error ("cannot read LTO decls from %s", file_data->file_name);
      return;
    }
  lto_read_decls (file_data, data, file_data->resolutions);
  lto_free_section_data (file_data, LTO_section_decls, NULL, data, len);
}

struct lwstate
{
  lto_file *file;
  struct lto_file_decl_data **file_data;
  int *count;
};

/* Traverse ids and create a list of file_datas out of it. */      

static int lto_create_files_from_ids (splay_tree_node node, void *data)
{
  struct lwstate *lw = (struct lwstate *)data;
  struct lto_file_decl_data *file_data = (struct lto_file_decl_data *)node->value;

  lto_file_finalize (file_data, lw->file);
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Creating file %s with sub id %x\n", 
	     file_data->file_name, file_data->id);
  file_data->next = *lw->file_data;
  *lw->file_data = file_data;
  (*lw->count)++;
  return 0;
}

/* Generate a TREE representation for all types and external decls
   entities in FILE.  

   Read all of the globals out of the file.  Then read the cgraph
   and process the .o index into the cgraph nodes so that it can open
   the .o file to load the functions and ipa information.   */

static struct lto_file_decl_data *
lto_file_read (lto_file *file, FILE *resolution_file, int *count)
{
  struct lto_file_decl_data *file_data = NULL;
  splay_tree file_ids;
  htab_t section_hash_table;
  struct lwstate state;
  
  section_hash_table = lto_obj_build_section_table (file);

  /* Find all sub modules in the object and put their sections into new hash
     tables in a splay tree. */
  file_ids = splay_tree_new (splay_tree_compare_ints, NULL, NULL);
  htab_traverse (section_hash_table, create_subid_section_table, file_ids);
  
  /* Add resolutions to file ids */
  lto_resolution_read (file_ids, resolution_file, file);

  /* Finalize each lto file for each submodule in the merged object
     and create list for returning. */
  state.file = file;
  state.file_data = &file_data;
  state.count = count;
  splay_tree_foreach (file_ids, lto_create_files_from_ids, &state);
    
  splay_tree_delete (file_ids);
  htab_delete (section_hash_table);

  return file_data;
}

#if HAVE_MMAP_FILE && HAVE_SYSCONF && defined _SC_PAGE_SIZE
#define LTO_MMAP_IO 1
#endif

#if LTO_MMAP_IO
/* Page size of machine is used for mmap and munmap calls.  */
static size_t page_mask;
#endif

/* Get the section data of length LEN from FILENAME starting at
   OFFSET.  The data segment must be freed by the caller when the
   caller is finished.  Returns NULL if all was not well.  */

static char *
lto_read_section_data (struct lto_file_decl_data *file_data,
		       intptr_t offset, size_t len)
{
  char *result;
  static int fd = -1;
  static char *fd_name;
#if LTO_MMAP_IO
  intptr_t computed_len;
  intptr_t computed_offset;
  intptr_t diff;
#endif

  /* Keep a single-entry file-descriptor cache.  The last file we
     touched will get closed at exit.
     ???  Eventually we want to add a more sophisticated larger cache
     or rather fix function body streaming to not stream them in
     practically random order.  */
  if (fd != -1
      && strcmp (fd_name, file_data->file_name) != 0)
    {
      free (fd_name);
      close (fd);
      fd = -1;
    }
  if (fd == -1)
    {
      fd = open (file_data->file_name, O_RDONLY|O_BINARY);
      if (fd == -1)
	return NULL;
      fd_name = xstrdup (file_data->file_name);
    }

#if LTO_MMAP_IO
  if (!page_mask)
    {
      size_t page_size = sysconf (_SC_PAGE_SIZE);
      page_mask = ~(page_size - 1);
    }

  computed_offset = offset & page_mask;
  diff = offset - computed_offset;
  computed_len = len + diff;

  result = (char *) mmap (NULL, computed_len, PROT_READ, MAP_PRIVATE,
			  fd, computed_offset);
  if (result == MAP_FAILED)
    return NULL;

  return result + diff;
#else
  result = (char *) xmalloc (len);
  if (lseek (fd, offset, SEEK_SET) != offset
      || read (fd, result, len) != (ssize_t) len)
    {
      free (result);
      result = NULL;
    }
#ifdef __MINGW32__
  /* Native windows doesn't supports delayed unlink on opened file. So
     we close file here again. This produces higher I/O load, but at least
     it prevents to have dangling file handles preventing unlink.  */
  free (fd_name);
  fd_name = NULL;
  close (fd);
  fd = -1;
#endif
  return result;
#endif
}    


/* Get the section data from FILE_DATA of SECTION_TYPE with NAME.
   NAME will be NULL unless the section type is for a function
   body.  */

static const char *
get_section_data (struct lto_file_decl_data *file_data,
		      enum lto_section_type section_type,
		      const char *name,
		      size_t *len)
{
  htab_t section_hash_table = file_data->section_hash_table;
  struct lto_section_slot *f_slot;
  struct lto_section_slot s_slot;
  const char *section_name = lto_get_section_name (section_type, name, file_data);
  char *data = NULL;

  *len = 0;
  s_slot.name = section_name;
  f_slot = (struct lto_section_slot *) htab_find (section_hash_table, &s_slot);
  if (f_slot)
    {
      data = lto_read_section_data (file_data, f_slot->start, f_slot->len);
      *len = f_slot->len;
    }

  free (CONST_CAST (char *, section_name));
  return data;
}


/* Free the section data from FILE_DATA of SECTION_TYPE with NAME that
   starts at OFFSET and has LEN bytes.  */

static void
free_section_data (struct lto_file_decl_data *file_data ATTRIBUTE_UNUSED,
		   enum lto_section_type section_type ATTRIBUTE_UNUSED,
		   const char *name ATTRIBUTE_UNUSED,
		   const char *offset, size_t len ATTRIBUTE_UNUSED)
{
#if LTO_MMAP_IO
  intptr_t computed_len;
  intptr_t computed_offset;
  intptr_t diff;
#endif

#if LTO_MMAP_IO
  computed_offset = ((intptr_t) offset) & page_mask;
  diff = (intptr_t) offset - computed_offset;
  computed_len = len + diff;

  munmap ((caddr_t) computed_offset, computed_len);
#else
  free (CONST_CAST(char *, offset));
#endif
}

/* Structure describing ltrans partitions.  */

struct GTY (()) ltrans_partition_def
{
  cgraph_node_set cgraph_set;
  varpool_node_set varpool_set;
  const char * GTY ((skip)) name;
  int insns;
};

typedef struct ltrans_partition_def *ltrans_partition;
DEF_VEC_P(ltrans_partition);
DEF_VEC_ALLOC_P(ltrans_partition,gc);

static GTY (()) VEC(ltrans_partition, gc) *ltrans_partitions;

static void add_cgraph_node_to_partition (ltrans_partition part, struct cgraph_node *node);
static void add_varpool_node_to_partition (ltrans_partition part, struct varpool_node *vnode);

/* Create new partition with name NAME.  */
static ltrans_partition
new_partition (const char *name)
{
  ltrans_partition part = ggc_alloc_ltrans_partition_def ();
  part->cgraph_set = cgraph_node_set_new ();
  part->varpool_set = varpool_node_set_new ();
  part->name = name;
  part->insns = 0;
  VEC_safe_push (ltrans_partition, gc, ltrans_partitions, part);
  return part;
}

/* See all references that go to comdat objects and bring them into partition too.  */
static void
add_references_to_partition (ltrans_partition part, struct ipa_ref_list *refs)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (refs, i, ref); i++)
    {
      if (ref->refered_type == IPA_REF_CGRAPH
	  && DECL_COMDAT (ipa_ref_node (ref)->decl)
	  && !cgraph_node_in_set_p (ipa_ref_node (ref), part->cgraph_set))
	add_cgraph_node_to_partition (part, ipa_ref_node (ref));
      else
	if (ref->refered_type == IPA_REF_VARPOOL
	    && DECL_COMDAT (ipa_ref_varpool_node (ref)->decl)
	    && !varpool_node_in_set_p (ipa_ref_varpool_node (ref), part->varpool_set))
	  add_varpool_node_to_partition (part, ipa_ref_varpool_node (ref));
    }
}

/* Add NODE to partition as well as the inline callees and referred comdats into partition PART. */

static void
add_cgraph_node_to_partition (ltrans_partition part, struct cgraph_node *node)
{
  struct cgraph_edge *e;

  part->insns += node->local.inline_summary.self_size;

  if (node->aux)
    {
      node->in_other_partition = 1;
      if (cgraph_dump_file)
        fprintf (cgraph_dump_file, "Node %s/%i now used in multiple partitions\n",
		 cgraph_node_name (node), node->uid);
    }
  node->aux = (void *)((size_t)node->aux + 1);

  cgraph_node_set_add (part->cgraph_set, node);

  for (e = node->callees; e; e = e->next_callee)
    if ((!e->inline_failed || DECL_COMDAT (e->callee->decl))
	&& !cgraph_node_in_set_p (e->callee, part->cgraph_set))
      add_cgraph_node_to_partition (part, e->callee);

  add_references_to_partition (part, &node->ref_list);

  if (node->same_comdat_group
      && !cgraph_node_in_set_p (node->same_comdat_group, part->cgraph_set))
    add_cgraph_node_to_partition (part, node->same_comdat_group);
}

/* Add VNODE to partition as well as comdat references partition PART. */

static void
add_varpool_node_to_partition (ltrans_partition part, struct varpool_node *vnode)
{
  varpool_node_set_add (part->varpool_set, vnode);

  if (vnode->aux)
    {
      vnode->in_other_partition = 1;
      if (cgraph_dump_file)
        fprintf (cgraph_dump_file, "Varpool node %s now used in multiple partitions\n",
		 varpool_node_name (vnode));
    }
  vnode->aux = (void *)((size_t)vnode->aux + 1);

  add_references_to_partition (part, &vnode->ref_list);

  if (vnode->same_comdat_group
      && !varpool_node_in_set_p (vnode->same_comdat_group, part->varpool_set))
    add_varpool_node_to_partition (part, vnode->same_comdat_group);
}

/* Undo all additions until number of cgraph nodes in PARITION is N_CGRAPH_NODES
   and number of varpool nodes is N_VARPOOL_NODES.  */

static void
undo_partition (ltrans_partition partition, unsigned int n_cgraph_nodes,
		unsigned int n_varpool_nodes)
{
  while (VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes) >
	 n_cgraph_nodes)
    {
      struct cgraph_node *node = VEC_index (cgraph_node_ptr,
					    partition->cgraph_set->nodes,
					    n_cgraph_nodes);
      partition->insns -= node->local.inline_summary.self_size;
      cgraph_node_set_remove (partition->cgraph_set, node);
      node->aux = (void *)((size_t)node->aux - 1);
    }
  while (VEC_length (varpool_node_ptr, partition->varpool_set->nodes) >
	 n_varpool_nodes)
    {
      struct varpool_node *node = VEC_index (varpool_node_ptr,
					     partition->varpool_set->nodes,
					     n_varpool_nodes);
      varpool_node_set_remove (partition->varpool_set, node);
      node->aux = (void *)((size_t)node->aux - 1);
    }
}

/* Return true if NODE should be partitioned.
   This means that partitioning algorithm should put NODE into one of partitions.
   This apply to most functions with bodies.  Functions that are not partitions
   are put into every unit needing them.  This is the case of i.e. COMDATs.  */

static bool
partition_cgraph_node_p (struct cgraph_node *node)
{
  /* We will get proper partition based on function they are inlined to.  */
  if (node->global.inlined_to)
    return false;
  /* Nodes without a body do not need partitioning.  */
  if (!node->analyzed)
    return false;
  /* Extern inlines and comdat are always only in partitions they are needed.  */
  if (DECL_EXTERNAL (node->decl)
      || (DECL_COMDAT (node->decl)
	  && !cgraph_used_from_object_file_p (node)))
    return false;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (node->decl)))
    return false;
  return true;
}

/* Return true if VNODE should be partitioned. 
   This means that partitioning algorithm should put VNODE into one of partitions. */

static bool
partition_varpool_node_p (struct varpool_node *vnode)
{
  if (vnode->alias || !vnode->needed)
    return false;
  /* Constant pool and comdat are always only in partitions they are needed.  */
  if (DECL_IN_CONSTANT_POOL (vnode->decl)
      || (DECL_COMDAT (vnode->decl)
	  && !vnode->force_output
	  && !varpool_used_from_object_file_p (vnode)))
    return false;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (vnode->decl)))
    return false;
  return true;
}

/* Group cgrah nodes by input files.  This is used mainly for testing
   right now.  */

static void
lto_1_to_1_map (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  struct lto_file_decl_data *file_data;
  struct pointer_map_t *pmap;
  ltrans_partition partition;
  void **slot;
  int npartitions = 0;

  timevar_push (TV_WHOPR_WPA);

  pmap = pointer_map_create ();

  for (node = cgraph_nodes; node; node = node->next)
    {
      if (!partition_cgraph_node_p (node))
	continue;

      file_data = node->local.lto_file_data;
      gcc_assert (!node->same_body_alias);

      if (file_data)
	{
          slot = pointer_map_contains (pmap, file_data);
          if (slot)
	    partition = (ltrans_partition) *slot;
	  else
	    {
	      partition = new_partition (file_data->file_name);
	      slot = pointer_map_insert (pmap, file_data);
	      *slot = partition;
	      npartitions++;
	    }
	}
      else if (!file_data
	       && VEC_length (ltrans_partition, ltrans_partitions))
	partition = VEC_index (ltrans_partition, ltrans_partitions, 0);
      else
	{
	  partition = new_partition ("");
	  slot = pointer_map_insert (pmap, NULL);
	  *slot = partition;
	  npartitions++;
	}

      if (!node->aux)
        add_cgraph_node_to_partition (partition, node);
    }

  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    {
      if (!partition_varpool_node_p (vnode))
	continue;
      file_data = vnode->lto_file_data;
      slot = pointer_map_contains (pmap, file_data);
      if (slot)
	partition = (ltrans_partition) *slot;
      else
	{
	  partition = new_partition (file_data->file_name);
	  slot = pointer_map_insert (pmap, file_data);
	  *slot = partition;
	  npartitions++;
	}

      if (!vnode->aux)
        add_varpool_node_to_partition (partition, vnode);
    }
  for (node = cgraph_nodes; node; node = node->next)
    node->aux = NULL;
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    vnode->aux = NULL;

  /* If the cgraph is empty, create one cgraph node set so that there is still
     an output file for any variables that need to be exported in a DSO.  */
  if (!npartitions)
    new_partition ("empty");

  pointer_map_destroy (pmap);

  timevar_pop (TV_WHOPR_WPA);

  lto_stats.num_cgraph_partitions += VEC_length (ltrans_partition, 
						 ltrans_partitions);
}


/* Group cgraph nodes in qually sized partitions.

   The algorithm deciding paritions are simple: nodes are taken in predefined
   order.  The order correspond to order we wish to have functions in final
   output.  In future this will be given by function reordering pass, but at
   the moment we use topological order that serve a good approximation.

   The goal is to partition this linear order into intervals (partitions) such
   that all partitions have approximately the same size and that the number of
   callgraph or IPA reference edgess crossing boundaries is minimal.

   This is a lot faster (O(n) in size of callgraph) than algorithms doing
   priority based graph clustering that are generally O(n^2) and since WHOPR
   is designed to make things go well across partitions, it leads to good results.

   We compute the expected size of partition as
   max (total_size / lto_partitions, min_partition_size).
   We use dynamic expected size of partition, so small programs
   are partitioning into enough partitions to allow use of multiple CPUs while
   large programs are not partitioned too much. Creating too many partition
   increase streaming overhead significandly.

   In the future we would like to bound maximal size of partition to avoid
   ltrans stage consuming too much memory.  At the moment however WPA stage is
   most memory intensive phase at large benchmark since too many types and
   declarations are read into memory.

   The function implement simple greedy algorithm.  Nodes are begin added into
   current partition until 3/4th of expected partition size is reached.
   After this threshold we keep track of boundary size (number of edges going to
   other partitions) and continue adding functions until the current partition
   grows into a double of expected partition size.  Then the process is undone
   till the point when minimal ration of boundary size and in partition calls
   was reached.  */

static void
lto_balanced_map (void)
{
  int n_nodes = 0;
  struct cgraph_node **postorder =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  struct cgraph_node **order = XNEWVEC (struct cgraph_node *, cgraph_max_uid);
  int i, postorder_len;
  struct cgraph_node *node;
  int total_size = 0, best_total_size = 0;
  int partition_size;
  ltrans_partition partition;
  unsigned int last_visited_cgraph_node = 0, last_visited_varpool_node = 0;
  struct varpool_node *vnode;
  int cost = 0, internal = 0;
  int best_n_nodes = 0, best_n_varpool_nodes = 0, best_i = 0, best_cost =
    INT_MAX, best_internal = 0;
  int npartitions;

  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    gcc_assert (!vnode->aux);
  /* Until we have better ordering facility, use toplogical order.
     Include only nodes we will partition and compute estimate of program
     size.  Note that since nodes that are not partitioned might be put into
     multiple partitions, this is just an estimate of real size.  This is why
     we keep partition_size updated after every partition is finalized.  */
  postorder_len = cgraph_postorder (postorder);
  for (i = 0; i < postorder_len; i++)
    {
      node = postorder[i];
      if (partition_cgraph_node_p (node))
	{
	  order[n_nodes++] = node;
          total_size += node->global.size;
	}
    }
  free (postorder);

  /* Compute partition size and create the first partition.  */
  partition_size = total_size / PARAM_VALUE (PARAM_LTO_PARTITIONS);
  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
  npartitions = 1;
  partition = new_partition ("");
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Total unit size: %i, partition size: %i\n",
	     total_size, partition_size);

  for (i = 0; i < n_nodes; i++)
    {
      if (!order[i]->aux)
        add_cgraph_node_to_partition (partition, order[i]);
      total_size -= order[i]->global.size;

      /* Once we added a new node to the partition, we also want to add
         all referenced variables unless they was already added into some
         earlier partition.
	 add_cgraph_node_to_partition adds possibly multiple nodes and
	 variables that are needed to satisfy needs of ORDER[i].
         We remember last visited cgraph and varpool node from last iteration
         of outer loop that allows us to process every new addition. 

	 At the same time we compute size of the boundary into COST.  Every
         callgraph or IPA reference edge leaving the partition contributes into
         COST.  Every edge inside partition was earlier computed as one leaving
	 it and thus we need to subtract it from COST.  */
      while (last_visited_cgraph_node <
	     VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes)
	     || last_visited_varpool_node < VEC_length (varpool_node_ptr,
							partition->varpool_set->
							nodes))
	{
	  struct ipa_ref_list *refs;
	  int j;
	  struct ipa_ref *ref;
	  bool cgraph_p = false;

	  if (last_visited_cgraph_node <
	      VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes))
	    {
	      struct cgraph_edge *edge;

	      cgraph_p = true;
	      node = VEC_index (cgraph_node_ptr, partition->cgraph_set->nodes,
				last_visited_cgraph_node);
	      refs = &node->ref_list;

	      last_visited_cgraph_node++;

	      gcc_assert (node->analyzed);

	      /* Compute boundary cost of callgrpah edges.  */
	      for (edge = node->callees; edge; edge = edge->next_callee)
		if (edge->callee->analyzed)
		  {
		    int edge_cost = edge->frequency;
		    cgraph_node_set_iterator csi;

		    if (!edge_cost)
		      edge_cost = 1;
		    gcc_assert (edge_cost > 0);
		    csi = cgraph_node_set_find (partition->cgraph_set, edge->callee);
		    if (!csi_end_p (csi)
		        && csi.index < last_visited_cgraph_node - 1)
		      cost -= edge_cost, internal+= edge_cost;
		    else
		      cost += edge_cost;
		  }
	      for (edge = node->callers; edge; edge = edge->next_caller)
		{
		  int edge_cost = edge->frequency;
		  cgraph_node_set_iterator csi;

		  gcc_assert (edge->caller->analyzed);
		  if (!edge_cost)
		    edge_cost = 1;
		  gcc_assert (edge_cost > 0);
		  csi = cgraph_node_set_find (partition->cgraph_set, edge->caller);
		  if (!csi_end_p (csi)
		      && csi.index < last_visited_cgraph_node)
		    cost -= edge_cost;
		  else
		    cost += edge_cost;
		}
	    }
	  else
	    {
	      refs =
		&VEC_index (varpool_node_ptr, partition->varpool_set->nodes,
			    last_visited_varpool_node)->ref_list;
	      last_visited_varpool_node++;
	    }

	  /* Compute boundary cost of IPA REF edges and at the same time look into
	     variables referenced from current partition and try to add them.  */
	  for (j = 0; ipa_ref_list_reference_iterate (refs, j, ref); j++)
	    if (ref->refered_type == IPA_REF_VARPOOL)
	      {
		varpool_node_set_iterator vsi;

		vnode = ipa_ref_varpool_node (ref);
		if (!vnode->finalized)
		  continue;
		if (!vnode->aux && partition_varpool_node_p (vnode))
		  add_varpool_node_to_partition (partition, vnode);
		vsi = varpool_node_set_find (partition->varpool_set, vnode);
		if (!vsi_end_p (vsi)
		    && vsi.index < last_visited_varpool_node - !cgraph_p)
		  cost--, internal++;
		else
		  cost++;
	      }
	    else
	      {
		cgraph_node_set_iterator csi;

		node = ipa_ref_node (ref);
		if (!node->analyzed)
		  continue;
		csi = cgraph_node_set_find (partition->cgraph_set, node);
		if (!csi_end_p (csi)
		    && csi.index < last_visited_cgraph_node - cgraph_p)
		  cost--, internal++;
		else
		  cost++;
	      }
	  for (j = 0; ipa_ref_list_refering_iterate (refs, j, ref); j++)
	    if (ref->refering_type == IPA_REF_VARPOOL)
	      {
		varpool_node_set_iterator vsi;

		vnode = ipa_ref_refering_varpool_node (ref);
		gcc_assert (vnode->finalized);
		if (!vnode->aux && partition_varpool_node_p (vnode))
		  add_varpool_node_to_partition (partition, vnode);
		vsi = varpool_node_set_find (partition->varpool_set, vnode);
		if (!vsi_end_p (vsi)
		    && vsi.index < last_visited_varpool_node)
		  cost--;
		else
		  cost++;
	      }
	    else
	      {
		cgraph_node_set_iterator csi;

		node = ipa_ref_refering_node (ref);
		gcc_assert (node->analyzed);
		csi = cgraph_node_set_find (partition->cgraph_set, node);
		if (!csi_end_p (csi)
		    && csi.index < last_visited_cgraph_node)
		  cost--;
		else
		  cost++;
	      }
	}

      /* If the partition is large enough, start looking for smallest boundary cost.  */
      if (partition->insns < partition_size * 3 / 4
	  || best_cost == INT_MAX
	  || ((!cost 
	       || (best_internal * (HOST_WIDE_INT) cost
		   > (internal * (HOST_WIDE_INT)best_cost)))
  	      && partition->insns < partition_size * 5 / 4))
	{
	  best_cost = cost;
	  best_internal = internal;
	  best_i = i;
	  best_n_nodes = VEC_length (cgraph_node_ptr,
				     partition->cgraph_set->nodes);
	  best_n_varpool_nodes = VEC_length (varpool_node_ptr,
					     partition->varpool_set->nodes);
	  best_total_size = total_size;
	}
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, "Step %i: added %s/%i, size %i, cost %i/%i best %i/%i, step %i\n", i,
		 cgraph_node_name (order[i]), order[i]->uid, partition->insns, cost, internal,
		 best_cost, best_internal, best_i);
      /* Partition is too large, unwind into step when best cost was reached and
	 start new partition.  */
      if (partition->insns > 2 * partition_size)
	{
	  if (best_i != i)
	    {
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file, "Unwinding %i insertions to step %i\n",
			 i - best_i, best_i);
	      undo_partition (partition, best_n_nodes, best_n_varpool_nodes);
	    }
	  i = best_i;
 	  /* When we are finished, avoid creating empty partition.  */
	  if (i == n_nodes - 1)
	    break;
	  partition = new_partition ("");
	  last_visited_cgraph_node = 0;
	  last_visited_varpool_node = 0;
	  total_size = best_total_size;
	  cost = 0;

	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, "New partition\n");
	  best_n_nodes = 0;
	  best_n_varpool_nodes = 0;
	  best_cost = INT_MAX;

	  /* Since the size of partitions is just approximate, update the size after
	     we finished current one.  */
	  if (npartitions < PARAM_VALUE (PARAM_LTO_PARTITIONS))
	    partition_size = total_size
	      / (PARAM_VALUE (PARAM_LTO_PARTITIONS) - npartitions);
	  else
	    partition_size = INT_MAX;

	  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
	    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
	  npartitions ++;
	}
    }

  /* Varables that are not reachable from the code go into last partition.  */
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    if (partition_varpool_node_p (vnode) && !vnode->aux)
      add_varpool_node_to_partition (partition, vnode);
  free (order);
}

/* Promote variable VNODE to be static.  */

static bool
promote_var (struct varpool_node *vnode)
{
  if (TREE_PUBLIC (vnode->decl) || DECL_EXTERNAL (vnode->decl))
    return false;
  gcc_assert (flag_wpa);
  TREE_PUBLIC (vnode->decl) = 1;
  DECL_VISIBILITY (vnode->decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (vnode->decl) = true;
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	    "Promoting var as hidden: %s\n", varpool_node_name (vnode));
  return true;
}

/* Promote function NODE to be static.  */

static bool
promote_fn (struct cgraph_node *node)
{
  gcc_assert (flag_wpa);
  if (TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl))
    return false;
  TREE_PUBLIC (node->decl) = 1;
  DECL_VISIBILITY (node->decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (node->decl) = true;
  if (node->same_body)
    {
      struct cgraph_node *alias;
      for (alias = node->same_body;
	   alias; alias = alias->next)
	{
	  TREE_PUBLIC (alias->decl) = 1;
	  DECL_VISIBILITY (alias->decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (alias->decl) = true;
	}
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	     "Promoting function as hidden: %s/%i\n",
	     cgraph_node_name (node), node->uid);
  return true;
}

/* Find out all static decls that need to be promoted to global because
   of cross file sharing.  This function must be run in the WPA mode after
   all inlinees are added.  */

static void
lto_promote_cross_file_statics (void)
{
  struct varpool_node *vnode;
  unsigned i, n_sets;
  cgraph_node_set set;
  varpool_node_set vset;
  cgraph_node_set_iterator csi;
  varpool_node_set_iterator vsi;
  VEC(varpool_node_ptr, heap) *promoted_initializers = NULL;
  struct pointer_set_t *inserted = pointer_set_create ();

  gcc_assert (flag_wpa);

  n_sets = VEC_length (ltrans_partition, ltrans_partitions);
  for (i = 0; i < n_sets; i++)
    {
      ltrans_partition part = VEC_index (ltrans_partition, ltrans_partitions, i);
      set = part->cgraph_set;
      vset = part->varpool_set;

      /* If node has either address taken (and we have no clue from where)
	 or it is called from other partition, it needs to be globalized.  */
      for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
	{
	  struct cgraph_node *node = csi_node (csi);
	  if (node->local.externally_visible)
	    continue;
	  if (node->global.inlined_to)
	    continue;
	  if ((!DECL_EXTERNAL (node->decl) && !DECL_COMDAT (node->decl))
	      && (referenced_from_other_partition_p (&node->ref_list, set, vset)
		  || reachable_from_other_partition_p (node, set)))
	    promote_fn (node);
	}
      for (vsi = vsi_start (vset); !vsi_end_p (vsi); vsi_next (&vsi))
	{
	  vnode = vsi_node (vsi);
	  /* Constant pool references use internal labels and thus can not
	     be made global.  It is sensible to keep those ltrans local to
	     allow better optimization.  */
	  if (!DECL_IN_CONSTANT_POOL (vnode->decl) && !DECL_COMDAT (vnode->decl)
	      && !vnode->externally_visible && vnode->analyzed
	      && referenced_from_other_partition_p (&vnode->ref_list,
						    set, vset))
	    promote_var (vnode);
	}

      /* We export initializers of read-only var into each partition
	 referencing it.  Folding might take declarations from the
	 initializers and use it; so everything referenced from the
	 initializers needs can be accessed from this partition after
	 folding.

	 This means that we need to promote all variables and functions
	 referenced from all initializers from readonly vars referenced
	 from this partition that are not in this partition.
	 This needs to be done recursively.  */
      for (vnode = varpool_nodes; vnode; vnode = vnode->next)
	if (const_value_known_p (vnode->decl)
	    && DECL_INITIAL (vnode->decl)
	    && !varpool_node_in_set_p (vnode, vset)
	    && referenced_from_this_partition_p (&vnode->ref_list, set, vset)
	    && !pointer_set_insert (inserted, vnode))
	VEC_safe_push (varpool_node_ptr, heap, promoted_initializers, vnode);
      while (!VEC_empty (varpool_node_ptr, promoted_initializers))
	{
	  int i;
	  struct ipa_ref *ref;

	  vnode = VEC_pop (varpool_node_ptr, promoted_initializers);
	  for (i = 0; ipa_ref_list_reference_iterate (&vnode->ref_list, i, ref); i++)
	    {
	      if (ref->refered_type == IPA_REF_CGRAPH)
		{
		  struct cgraph_node *n = ipa_ref_node (ref);
		  gcc_assert (!n->global.inlined_to);
		  if (!n->local.externally_visible
		      && !cgraph_node_in_set_p (n, set))
		    promote_fn (n);
		}
	      else
		{
		  struct varpool_node *v = ipa_ref_varpool_node (ref);
		  if (varpool_node_in_set_p (v, vset))
		    continue;
		  /* Constant pool references use internal labels and thus can not
		     be made global.  It is sensible to keep those ltrans local to
		     allow better optimization.  */
		  if (DECL_IN_CONSTANT_POOL (v->decl))
		    {
		      if (!pointer_set_insert (inserted, vnode))
			VEC_safe_push (varpool_node_ptr, heap,
				       promoted_initializers, v);
		    }
		  else if (!DECL_IN_CONSTANT_POOL (v->decl)
			   && !v->externally_visible && v->analyzed)
		    {
		      if (promote_var (v)
			  && DECL_INITIAL (v->decl)
			  && const_value_known_p (v->decl)
			  && !pointer_set_insert (inserted, vnode))
			VEC_safe_push (varpool_node_ptr, heap,
				       promoted_initializers, v);
		    }
		}
	    }
	}
    }
  pointer_set_destroy (inserted);
}

static lto_file *current_lto_file;

/* Helper for qsort; compare partitions and return one with smaller size.
   We sort from greatest to smallest so parallel build doesn't stale on the
   longest compilation being executed too late.  */

static int
cmp_partitions (const void *a, const void *b)
{
  const struct ltrans_partition_def *pa
     = *(struct ltrans_partition_def *const *)a;
  const struct ltrans_partition_def *pb
     = *(struct ltrans_partition_def *const *)b;
  return pb->insns - pa->insns;
}

/* Write all output files in WPA mode and the file with the list of
   LTRANS units.  */

static void
lto_wpa_write_files (void)
{
  unsigned i, n_sets;
  lto_file *file;
  cgraph_node_set set;
  varpool_node_set vset;
  ltrans_partition part;
  FILE *ltrans_output_list_stream;
  char *temp_filename;
  size_t blen;

  /* Open the LTRANS output list.  */
  if (!ltrans_output_list)
    fatal_error ("no LTRANS output list filename provided");
  ltrans_output_list_stream = fopen (ltrans_output_list, "w");
  if (ltrans_output_list_stream == NULL)
    fatal_error ("opening LTRANS output list %s: %m", ltrans_output_list);

  timevar_push (TV_WHOPR_WPA);

  FOR_EACH_VEC_ELT (ltrans_partition, ltrans_partitions, i, part)
    lto_stats.num_output_cgraph_nodes += VEC_length (cgraph_node_ptr,
						     part->cgraph_set->nodes);

  /* Find out statics that need to be promoted
     to globals with hidden visibility because they are accessed from multiple
     partitions.  */
  lto_promote_cross_file_statics ();

  timevar_pop (TV_WHOPR_WPA);

  timevar_push (TV_WHOPR_WPA_IO);

  /* Generate a prefix for the LTRANS unit files.  */
  blen = strlen (ltrans_output_list);
  temp_filename = (char *) xmalloc (blen + sizeof ("2147483648.o"));
  strcpy (temp_filename, ltrans_output_list);
  if (blen > sizeof (".out")
      && strcmp (temp_filename + blen - sizeof (".out") + 1,
		 ".out") == 0)
    temp_filename[blen - sizeof (".out") + 1] = '\0';
  blen = strlen (temp_filename);

  n_sets = VEC_length (ltrans_partition, ltrans_partitions);
  VEC_qsort (ltrans_partition, ltrans_partitions, cmp_partitions);
  for (i = 0; i < n_sets; i++)
    {
      size_t len;
      ltrans_partition part = VEC_index (ltrans_partition, ltrans_partitions, i);

      set = part->cgraph_set;
      vset = part->varpool_set;

      /* Write all the nodes in SET.  */
      sprintf (temp_filename + blen, "%u.o", i);
      file = lto_obj_file_open (temp_filename, true);
      if (!file)
	fatal_error ("lto_obj_file_open() failed");

      if (!quiet_flag)
	fprintf (stderr, " %s (%s %i insns)", temp_filename, part->name, part->insns);
      if (cgraph_dump_file)
	{
	  fprintf (cgraph_dump_file, "Writting partition %s to file %s, %i insns\n",
		   part->name, temp_filename, part->insns);
	  fprintf (cgraph_dump_file, "cgraph nodes:");
	  dump_cgraph_node_set (cgraph_dump_file, set);
	  fprintf (cgraph_dump_file, "varpool nodes:");
	  dump_varpool_node_set (cgraph_dump_file, vset);
	}
      gcc_checking_assert (cgraph_node_set_nonempty_p (set)
			   || varpool_node_set_nonempty_p (vset) || !i);

      lto_set_current_out_file (file);

      ipa_write_optimization_summaries (set, vset);

      lto_set_current_out_file (NULL);
      lto_obj_file_close (file);

      len = strlen (temp_filename);
      if (fwrite (temp_filename, 1, len, ltrans_output_list_stream) < len
	  || fwrite ("\n", 1, 1, ltrans_output_list_stream) < 1)
	fatal_error ("writing to LTRANS output list %s: %m",
		     ltrans_output_list);
    }

  lto_stats.num_output_files += n_sets;

  /* Close the LTRANS output list.  */
  if (fclose (ltrans_output_list_stream))
    fatal_error ("closing LTRANS output list %s: %m", ltrans_output_list);

  timevar_pop (TV_WHOPR_WPA_IO);
}


typedef struct {
  struct pointer_set_t *seen;
} lto_fixup_data_t;

#define LTO_FIXUP_SUBTREE(t) \
  do \
    walk_tree (&(t), lto_fixup_tree, data, NULL); \
  while (0)

#define LTO_REGISTER_TYPE_AND_FIXUP_SUBTREE(t) \
  do \
    { \
      if (t) \
	(t) = gimple_register_type (t); \
      walk_tree (&(t), lto_fixup_tree, data, NULL); \
    } \
  while (0)

static tree lto_fixup_tree (tree *, int *, void *);

/* Return true if T does not need to be fixed up recursively.  */

static inline bool
no_fixup_p (tree t)
{
  return (t == NULL
	  || CONSTANT_CLASS_P (t)
	  || TREE_CODE (t) == IDENTIFIER_NODE);
}

/* Fix up fields of a tree_common T.  DATA points to fix-up states.  */

static void
lto_fixup_common (tree t, void *data)
{
  /* The following re-creates the TYPE_REFERENCE_TO and TYPE_POINTER_TO
     lists.  We do not stream TYPE_REFERENCE_TO, TYPE_POINTER_TO or
     TYPE_NEXT_PTR_TO and TYPE_NEXT_REF_TO.
     First remove us from any pointer list we are on.  */
  if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TYPE_POINTER_TO (TREE_TYPE (t)) == t)
	TYPE_POINTER_TO (TREE_TYPE (t)) = TYPE_NEXT_PTR_TO (t);
      else
	{
	  tree tem = TYPE_POINTER_TO (TREE_TYPE (t));
	  while (tem && TYPE_NEXT_PTR_TO (tem) != t)
	    tem = TYPE_NEXT_PTR_TO (tem);
	  if (tem)
	    TYPE_NEXT_PTR_TO (tem) = TYPE_NEXT_PTR_TO (t);
	}
      TYPE_NEXT_PTR_TO (t) = NULL_TREE;
    }
  else if (TREE_CODE (t) == REFERENCE_TYPE)
    {
      if (TYPE_REFERENCE_TO (TREE_TYPE (t)) == t)
	TYPE_REFERENCE_TO (TREE_TYPE (t)) = TYPE_NEXT_REF_TO (t);
      else
	{
	  tree tem = TYPE_REFERENCE_TO (TREE_TYPE (t));
	  while (tem && TYPE_NEXT_REF_TO (tem) != t)
	    tem = TYPE_NEXT_REF_TO (tem);
	  if (tem)
	    TYPE_NEXT_REF_TO (tem) = TYPE_NEXT_REF_TO (t);
	}
      TYPE_NEXT_REF_TO (t) = NULL_TREE;
    }

  /* Fixup our type.  */
  LTO_REGISTER_TYPE_AND_FIXUP_SUBTREE (TREE_TYPE (t));

  /* Second put us on the list of pointers of the new pointed-to type
     if we are a main variant.  This is done in lto_fixup_type after
     fixing up our main variant.  */

  /* This is not very efficient because we cannot do tail-recursion with
     a long chain of trees. */
  LTO_FIXUP_SUBTREE (TREE_CHAIN (t));
}

/* Fix up fields of a decl_minimal T.  DATA points to fix-up states.  */

static void
lto_fixup_decl_minimal (tree t, void *data)
{
  lto_fixup_common (t, data);
  LTO_FIXUP_SUBTREE (DECL_NAME (t));
  LTO_FIXUP_SUBTREE (DECL_CONTEXT (t));
}

/* Fix up fields of a decl_common T.  DATA points to fix-up states.  */

static void
lto_fixup_decl_common (tree t, void *data)
{
  lto_fixup_decl_minimal (t, data);
  LTO_FIXUP_SUBTREE (DECL_SIZE (t));
  LTO_FIXUP_SUBTREE (DECL_SIZE_UNIT (t));
  LTO_FIXUP_SUBTREE (DECL_INITIAL (t));
  LTO_FIXUP_SUBTREE (DECL_ATTRIBUTES (t));
  LTO_FIXUP_SUBTREE (DECL_ABSTRACT_ORIGIN (t));
}

/* Fix up fields of a decl_with_vis T.  DATA points to fix-up states.  */

static void
lto_fixup_decl_with_vis (tree t, void *data)
{
  lto_fixup_decl_common (t, data);

  /* Accessor macro has side-effects, use field-name here. */
  LTO_FIXUP_SUBTREE (t->decl_with_vis.assembler_name);

  gcc_assert (no_fixup_p (DECL_SECTION_NAME (t)));
}

/* Fix up fields of a decl_non_common T.  DATA points to fix-up states.  */

static void
lto_fixup_decl_non_common (tree t, void *data)
{
  lto_fixup_decl_with_vis (t, data);
  LTO_FIXUP_SUBTREE (DECL_ARGUMENT_FLD (t));
  LTO_FIXUP_SUBTREE (DECL_RESULT_FLD (t));
  LTO_FIXUP_SUBTREE (DECL_VINDEX (t));

  /* SAVED_TREE should not cleared by now.  Also no accessor for base type. */
  gcc_assert (no_fixup_p (t->decl_non_common.saved_tree));
}

/* Fix up fields of a decl_non_common T.  DATA points to fix-up states.  */

static void
lto_fixup_function (tree t, void *data)
{
  lto_fixup_decl_non_common (t, data);
  LTO_FIXUP_SUBTREE (DECL_FUNCTION_PERSONALITY (t));
}

/* Fix up fields of a field_decl T.  DATA points to fix-up states.  */

static void
lto_fixup_field_decl (tree t, void *data)
{
  lto_fixup_decl_common (t, data);
  LTO_FIXUP_SUBTREE (DECL_FIELD_OFFSET (t));
  LTO_FIXUP_SUBTREE (DECL_BIT_FIELD_TYPE (t));
  LTO_FIXUP_SUBTREE (DECL_QUALIFIER (t));
  gcc_assert (no_fixup_p (DECL_FIELD_BIT_OFFSET (t)));
  LTO_FIXUP_SUBTREE (DECL_FCONTEXT (t));
}

/* Fix up fields of a type T.  DATA points to fix-up states.  */

static void
lto_fixup_type (tree t, void *data)
{
  tree tem, mv;

  lto_fixup_common (t, data);
  LTO_FIXUP_SUBTREE (TYPE_CACHED_VALUES (t));
  LTO_FIXUP_SUBTREE (TYPE_SIZE (t));
  LTO_FIXUP_SUBTREE (TYPE_SIZE_UNIT (t));
  LTO_FIXUP_SUBTREE (TYPE_ATTRIBUTES (t));
  LTO_FIXUP_SUBTREE (TYPE_NAME (t));

  /* Accessors are for derived node types only. */
  if (!POINTER_TYPE_P (t))
    LTO_FIXUP_SUBTREE (t->type.minval);
  LTO_FIXUP_SUBTREE (t->type.maxval);

  /* Accessor is for derived node types only. */
  LTO_FIXUP_SUBTREE (t->type.binfo);

  if (TYPE_CONTEXT (t))
    {
      if (TYPE_P (TYPE_CONTEXT (t)))
	LTO_REGISTER_TYPE_AND_FIXUP_SUBTREE (TYPE_CONTEXT (t));
      else
	LTO_FIXUP_SUBTREE (TYPE_CONTEXT (t));
    }

  /* Compute the canonical type of t and fix that up.  From this point
     there are no longer any types with TYPE_STRUCTURAL_EQUALITY_P
     and its type-based alias problems.  */
  if (!TYPE_CANONICAL (t))
    {
      TYPE_CANONICAL (t) = gimple_register_canonical_type (t);
      LTO_FIXUP_SUBTREE (TYPE_CANONICAL (t));
    }

  /* The following re-creates proper variant lists while fixing up
     the variant leaders.  We do not stream TYPE_NEXT_VARIANT so the
     variant list state before fixup is broken.  */

  /* Remove us from our main variant list if we are not the variant leader.  */
  if (TYPE_MAIN_VARIANT (t) != t)
    {
      tem = TYPE_MAIN_VARIANT (t);
      while (tem && TYPE_NEXT_VARIANT (tem) != t)
	tem = TYPE_NEXT_VARIANT (tem);
      if (tem)
	TYPE_NEXT_VARIANT (tem) = TYPE_NEXT_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = NULL_TREE;
    }

  /* Query our new main variant.  */
  mv = gimple_register_type (TYPE_MAIN_VARIANT (t));

  /* If we were the variant leader and we get replaced ourselves drop
     all variants from our list.  */
  if (TYPE_MAIN_VARIANT (t) == t
      && mv != t)
    {
      tem = t;
      while (tem)
	{
	  tree tem2 = TYPE_NEXT_VARIANT (tem);
	  TYPE_NEXT_VARIANT (tem) = NULL_TREE;
	  tem = tem2;
	}
    }

  /* If we are not our own variant leader link us into our new leaders
     variant list.  */
  if (mv != t)
    {
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (mv);
      TYPE_NEXT_VARIANT (mv) = t;
    }

  /* Finally adjust our main variant and fix it up.  */
  TYPE_MAIN_VARIANT (t) = mv;
  LTO_FIXUP_SUBTREE (TYPE_MAIN_VARIANT (t));

  /* As the second step of reconstructing the pointer chains put us
     on the list of pointers of the new pointed-to type
     if we are a main variant.  See lto_fixup_common for the first step.  */
  if (TREE_CODE (t) == POINTER_TYPE
      && TYPE_MAIN_VARIANT (t) == t)
    {
      TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (TREE_TYPE (t));
      TYPE_POINTER_TO (TREE_TYPE (t)) = t;
    }
  else if (TREE_CODE (t) == REFERENCE_TYPE
	   && TYPE_MAIN_VARIANT (t) == t)
    {
      TYPE_NEXT_REF_TO (t) = TYPE_REFERENCE_TO (TREE_TYPE (t));
      TYPE_REFERENCE_TO (TREE_TYPE (t)) = t;
    }
}

/* Fix up fields of a BINFO T.  DATA points to fix-up states.  */

static void
lto_fixup_binfo (tree t, void *data)
{
  unsigned HOST_WIDE_INT i, n;
  tree base, saved_base;

  lto_fixup_common (t, data);
  gcc_assert (no_fixup_p (BINFO_OFFSET (t)));
  LTO_FIXUP_SUBTREE (BINFO_VTABLE (t));
  LTO_FIXUP_SUBTREE (BINFO_VIRTUALS (t));
  LTO_FIXUP_SUBTREE (BINFO_VPTR_FIELD (t));
  n = VEC_length (tree, BINFO_BASE_ACCESSES (t));
  for (i = 0; i < n; i++)
    {
      saved_base = base = BINFO_BASE_ACCESS (t, i);
      LTO_FIXUP_SUBTREE (base);
      if (base != saved_base)
	VEC_replace (tree, BINFO_BASE_ACCESSES (t), i, base);
    }
  LTO_FIXUP_SUBTREE (BINFO_INHERITANCE_CHAIN (t));
  LTO_FIXUP_SUBTREE (BINFO_SUBVTT_INDEX (t));
  LTO_FIXUP_SUBTREE (BINFO_VPTR_INDEX (t));
  n = BINFO_N_BASE_BINFOS (t);
  for (i = 0; i < n; i++)
    {
      saved_base = base = BINFO_BASE_BINFO (t, i);
      LTO_FIXUP_SUBTREE (base);
      if (base != saved_base)
	VEC_replace (tree, BINFO_BASE_BINFOS (t), i, base);
    }
}

/* Fix up fields of a CONSTRUCTOR T.  DATA points to fix-up states.  */

static void
lto_fixup_constructor (tree t, void *data)
{
  unsigned HOST_WIDE_INT idx;
  constructor_elt *ce;

  LTO_REGISTER_TYPE_AND_FIXUP_SUBTREE (TREE_TYPE (t));

  for (idx = 0;
       VEC_iterate(constructor_elt, CONSTRUCTOR_ELTS (t), idx, ce);
       idx++)
    {
      LTO_FIXUP_SUBTREE (ce->index);
      LTO_FIXUP_SUBTREE (ce->value);
    }
}

/* A walk_tree callback used by lto_fixup_state. TP is the pointer to the
   current tree. WALK_SUBTREES indicates if the subtrees will be walked.
   DATA is a pointer set to record visited nodes. */

static tree
lto_fixup_tree (tree *tp, int *walk_subtrees, void *data)
{
  tree t;
  lto_fixup_data_t *fixup_data = (lto_fixup_data_t *) data;
  tree prevailing;

  t = *tp;
  *walk_subtrees = 0;
  if (!t || pointer_set_contains (fixup_data->seen, t))
    return NULL;

  if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == FUNCTION_DECL)
    {
      prevailing = lto_symtab_prevailing_decl (t);

      if (t != prevailing)
	{
	   /* Also replace t with prevailing defintion.  We don't want to
	      insert the other defintion in the seen set as we want to
	      replace all instances of it.  */
	  *tp = prevailing;
	  t = prevailing;
	}
    }
  else if (TYPE_P (t))
    {
      /* Replace t with the prevailing type.  We don't want to insert the
         other type in the seen set as we want to replace all instances of it.  */
      t = gimple_register_type (t);
      *tp = t;
    }

  if (pointer_set_insert (fixup_data->seen, t))
    return NULL;

  /* walk_tree does not visit all reachable nodes that need to be fixed up.
     Hence we do special processing here for those kind of nodes. */
  switch (TREE_CODE (t))
    {
    case FIELD_DECL:
      lto_fixup_field_decl (t, data);
      break;

    case LABEL_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case IMPORTED_DECL:
      lto_fixup_decl_common (t, data);
      break;

    case VAR_DECL:
      lto_fixup_decl_with_vis (t, data);
      break;	

    case TYPE_DECL:
      lto_fixup_decl_non_common (t, data);
      break;

    case FUNCTION_DECL:
      lto_fixup_function (t, data);
      break;

    case TREE_BINFO:
      lto_fixup_binfo (t, data);
      break;

    default:
      if (TYPE_P (t))
	lto_fixup_type (t, data);
      else if (TREE_CODE (t) == CONSTRUCTOR)
	lto_fixup_constructor (t, data);
      else if (CONSTANT_CLASS_P (t))
	LTO_REGISTER_TYPE_AND_FIXUP_SUBTREE (TREE_TYPE (t));
      else if (EXPR_P (t))
	{
	  /* walk_tree only handles TREE_OPERANDs. Do the rest here.  */
	  lto_fixup_common (t, data);
	  LTO_FIXUP_SUBTREE (t->exp.block);
	  *walk_subtrees = 1;
	}
      else
	{
	  /* Let walk_tree handle sub-trees.  */
	  *walk_subtrees = 1;
	}
    }

  return NULL;
}

/* Helper function of lto_fixup_decls. Walks the var and fn streams in STATE,
   replaces var and function decls with the corresponding prevailing def and
   records the old decl in the free-list in DATA. We also record visted nodes
   in the seen-set in DATA to avoid multiple visit for nodes that need not
   to be replaced.  */

static void
lto_fixup_state (struct lto_in_decl_state *state, lto_fixup_data_t *data)
{
  unsigned i, si;
  struct lto_tree_ref_table *table;

  /* Although we only want to replace FUNCTION_DECLs and VAR_DECLs,
     we still need to walk from all DECLs to find the reachable
     FUNCTION_DECLs and VAR_DECLs.  */
  for (si = 0; si < LTO_N_DECL_STREAMS; si++)
    {
      table = &state->streams[si];
      for (i = 0; i < table->size; i++)
	walk_tree (table->trees + i, lto_fixup_tree, data, NULL);
    }
}

/* A callback of htab_traverse. Just extract a state from SLOT and the
   lto_fixup_data_t object from AUX and calls lto_fixup_state. */

static int
lto_fixup_state_aux (void **slot, void *aux)
{
  struct lto_in_decl_state *state = (struct lto_in_decl_state *) *slot;
  lto_fixup_state (state, (lto_fixup_data_t *) aux);
  return 1;
}

/* Fix the decls from all FILES. Replaces each decl with the corresponding
   prevailing one.  */

static void
lto_fixup_decls (struct lto_file_decl_data **files)
{
  unsigned int i;
  tree decl;
  struct pointer_set_t *seen = pointer_set_create ();
  lto_fixup_data_t data;

  data.seen = seen;
  for (i = 0; files[i]; i++)
    {
      struct lto_file_decl_data *file = files[i];
      struct lto_in_decl_state *state = file->global_decl_state;
      lto_fixup_state (state, &data);

      htab_traverse (file->function_decl_states, lto_fixup_state_aux, &data);
    }

  FOR_EACH_VEC_ELT (tree, lto_global_var_decls, i, decl)
    {
      tree saved_decl = decl;
      walk_tree (&decl, lto_fixup_tree, &data, NULL);
      if (decl != saved_decl)
	VEC_replace (tree, lto_global_var_decls, i, decl);
    }

  pointer_set_destroy (seen);
}

/* Read the options saved from each file in the command line.  Called
   from lang_hooks.post_options which is called by process_options
   right before all the options are used to initialize the compiler.
   This assumes that decode_options has already run, so the
   num_in_fnames and in_fnames are properly set.

   Note that this assumes that all the files had been compiled with
   the same options, which is not a good assumption.  In general,
   options ought to be read from all the files in the set and merged.
   However, it is still unclear what the merge rules should be.  */

void
lto_read_all_file_options (void)
{
  size_t i;

  /* Clear any file options currently saved.  */
  lto_clear_file_options ();

  /* Set the hooks to read ELF sections.  */
  lto_set_in_hooks (NULL, get_section_data, free_section_data);
  if (!quiet_flag)
    fprintf (stderr, "Reading command line options:");

  for (i = 0; i < num_in_fnames; i++)
    {
      struct lto_file_decl_data *file_data;
      lto_file *file = lto_obj_file_open (in_fnames[i], false);
      if (!file)
	break;
      if (!quiet_flag)
	{
	  fprintf (stderr, " %s", in_fnames[i]);
	  fflush (stderr);
	}

      file_data = XCNEW (struct lto_file_decl_data);
      file_data->file_name = file->filename;
      file_data->section_hash_table = lto_obj_build_section_table (file);

      lto_read_file_options (file_data);

      lto_obj_file_close (file);
      htab_delete (file_data->section_hash_table);
      free (file_data);
    }

  if (!quiet_flag)
    fprintf (stderr, "\n");

  /* Apply globally the options read from all the files.  */
  lto_reissue_options ();
}

static GTY((length ("lto_stats.num_input_files + 1"))) struct lto_file_decl_data **all_file_decl_data;

/* Turn file datas for sub files into a single array, so that they look
   like separate files for further passes. */

static void
lto_flatten_files (struct lto_file_decl_data **orig, int count, int last_file_ix)
{
  struct lto_file_decl_data *n, *next;
  int i, k;

  lto_stats.num_input_files = count;
  all_file_decl_data
    = ggc_alloc_cleared_vec_lto_file_decl_data_ptr (count + 1);
  /* Set the hooks so that all of the ipa passes can read in their data.  */
  lto_set_in_hooks (all_file_decl_data, get_section_data, free_section_data);
  for (i = 0, k = 0; i < last_file_ix; i++) 
    {
      for (n = orig[i]; n != NULL; n = next)
	{
	  all_file_decl_data[k++] = n;
	  next = n->next;
	  n->next = NULL;
	}
    }
  all_file_decl_data[k] = NULL;
  gcc_assert (k == count);
}

/* Input file data before flattening (i.e. splitting them to subfiles to support
   incremental linking.  */
static int real_file_count;
static GTY((length ("real_file_count + 1"))) struct lto_file_decl_data **real_file_decl_data;

/* Read all the symbols from the input files FNAMES.  NFILES is the
   number of files requested in the command line.  Instantiate a
   global call graph by aggregating all the sub-graphs found in each
   file.  */

static void
read_cgraph_and_symbols (unsigned nfiles, const char **fnames)
{
  unsigned int i, last_file_ix;
  FILE *resolution;
  struct cgraph_node *node;
  int count = 0;
  struct lto_file_decl_data **decl_data;

  init_cgraph ();

  timevar_push (TV_IPA_LTO_DECL_IN);

  real_file_decl_data
    = decl_data = ggc_alloc_cleared_vec_lto_file_decl_data_ptr (nfiles + 1);
  real_file_count = nfiles;

  /* Read the resolution file.  */
  resolution = NULL;
  if (resolution_file_name)
    {
      int t;
      unsigned num_objects;

      resolution = fopen (resolution_file_name, "r");
      if (resolution == NULL)
	fatal_error ("could not open symbol resolution file: %m");

      t = fscanf (resolution, "%u", &num_objects);
      gcc_assert (t == 1);

      /* True, since the plugin splits the archives.  */
      gcc_assert (num_objects == nfiles);
    }

  if (!quiet_flag)
    fprintf (stderr, "Reading object files:");

  /* Read all of the object files specified on the command line.  */
  for (i = 0, last_file_ix = 0; i < nfiles; ++i)
    {
      struct lto_file_decl_data *file_data = NULL;
      if (!quiet_flag)
	{
	  fprintf (stderr, " %s", fnames[i]);
	  fflush (stderr);
	}

      current_lto_file = lto_obj_file_open (fnames[i], false);
      if (!current_lto_file)
	break;

      file_data = lto_file_read (current_lto_file, resolution, &count);
      if (!file_data)
	{
	  lto_obj_file_close (current_lto_file);
	  current_lto_file = NULL;
	  break;
	}

      decl_data[last_file_ix++] = file_data;

      lto_obj_file_close (current_lto_file);
      current_lto_file = NULL;
      ggc_collect ();
    }

  lto_flatten_files (decl_data, count, last_file_ix);
  lto_stats.num_input_files = count;
  ggc_free(decl_data);
  real_file_decl_data = NULL;

  if (resolution_file_name)
    fclose (resolution);

  /* Set the hooks so that all of the ipa passes can read in their data.  */
  lto_set_in_hooks (all_file_decl_data, get_section_data, free_section_data);

  timevar_pop (TV_IPA_LTO_DECL_IN);

  if (!quiet_flag)
    fprintf (stderr, "\nReading the callgraph\n");

  timevar_push (TV_IPA_LTO_CGRAPH_IO);
  /* Read the callgraph.  */
  input_cgraph ();
  timevar_pop (TV_IPA_LTO_CGRAPH_IO);

  if (!quiet_flag)
    fprintf (stderr, "Merging declarations\n");

  timevar_push (TV_IPA_LTO_DECL_MERGE);
  /* Merge global decls.  */
  lto_symtab_merge_decls ();

  /* If there were errors during symbol merging bail out, we have no
     good way to recover here.  */
  if (seen_error ())
    fatal_error ("errors during merging of translation units");

  /* Fixup all decls and types and free the type hash tables.  */
  lto_fixup_decls (all_file_decl_data);
  free_gimple_type_tables ();
  ggc_collect ();

  timevar_pop (TV_IPA_LTO_DECL_MERGE);
  /* Each pass will set the appropriate timer.  */

  if (!quiet_flag)
    fprintf (stderr, "Reading summaries\n");

  /* Read the IPA summary data.  */
  if (flag_ltrans)
    ipa_read_optimization_summaries ();
  else
    ipa_read_summaries ();

  /* Finally merge the cgraph according to the decl merging decisions.  */
  timevar_push (TV_IPA_LTO_CGRAPH_MERGE);
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Before merging:\n");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  lto_symtab_merge_cgraph_nodes ();
  ggc_collect ();

  if (flag_ltrans)
    for (node = cgraph_nodes; node; node = node->next)
      {
	/* FIXME: ipa_transforms_to_apply holds list of passes that have optimization
	   summaries computed and needs to apply changes.  At the moment WHOPR only
	   supports inlining, so we can push it here by hand.  In future we need to stream
	   this field into ltrans compilation.  */
	if (node->analyzed)
	  VEC_safe_push (ipa_opt_pass, heap,
			 node->ipa_transforms_to_apply,
			 (ipa_opt_pass)&pass_ipa_inline);
      }
  lto_symtab_free ();

  timevar_pop (TV_IPA_LTO_CGRAPH_MERGE);

  timevar_push (TV_IPA_LTO_DECL_INIT_IO);

  /* FIXME lto. This loop needs to be changed to use the pass manager to
     call the ipa passes directly.  */
  if (!seen_error ())
    for (i = 0; i < last_file_ix; i++)
      {
	struct lto_file_decl_data *file_data = all_file_decl_data [i];
	lto_materialize_constructors_and_inits (file_data);
      }

  /* Indicate that the cgraph is built and ready.  */
  cgraph_function_flags_ready = true;

  timevar_pop (TV_IPA_LTO_DECL_INIT_IO);
  ggc_free (all_file_decl_data);
  all_file_decl_data = NULL;
}


/* Materialize all the bodies for all the nodes in the callgraph.  */

static void
materialize_cgraph (void)
{
  tree decl;
  struct cgraph_node *node; 
  unsigned i;
  timevar_id_t lto_timer;

  if (!quiet_flag)
    fprintf (stderr,
	     flag_wpa ? "Materializing decls:" : "Reading function bodies:");


  /* Now that we have input the cgraph, we need to clear all of the aux
     nodes and read the functions if we are not running in WPA mode.  */
  timevar_push (TV_IPA_LTO_GIMPLE_IN);

  for (node = cgraph_nodes; node; node = node->next)
    {
      if (node->local.lto_file_data)
	{
	  lto_materialize_function (node);
	  lto_stats.num_input_cgraph_nodes++;
	}
    }

  timevar_pop (TV_IPA_LTO_GIMPLE_IN);

  /* Start the appropriate timer depending on the mode that we are
     operating in.  */
  lto_timer = (flag_wpa) ? TV_WHOPR_WPA
	      : (flag_ltrans) ? TV_WHOPR_LTRANS
	      : TV_LTO;
  timevar_push (lto_timer);

  current_function_decl = NULL;
  set_cfun (NULL);

  /* Inform the middle end about the global variables we have seen.  */
  FOR_EACH_VEC_ELT (tree, lto_global_var_decls, i, decl)
    rest_of_decl_compilation (decl, 1, 0);

  if (!quiet_flag)
    fprintf (stderr, "\n");

  timevar_pop (lto_timer);
}


/* Perform whole program analysis (WPA) on the callgraph and write out the
   optimization plan.  */

static void
do_whole_program_analysis (void)
{
  /* Note that since we are in WPA mode, materialize_cgraph will not
     actually read in all the function bodies.  It only materializes
     the decls and cgraph nodes so that analysis can be performed.  */
  materialize_cgraph ();

  /* Reading in the cgraph uses different timers, start timing WPA now.  */
  timevar_push (TV_WHOPR_WPA);

  if (pre_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption before IPA\n");
      dump_memory_report (false);
    }

  cgraph_function_flags_ready = true;

  if (cgraph_dump_file)
    {
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  bitmap_obstack_initialize (NULL);
  ipa_register_cgraph_hooks ();
  cgraph_state = CGRAPH_STATE_IPA_SSA;

  execute_ipa_pass_list (all_regular_ipa_passes);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  verify_cgraph ();
  bitmap_obstack_release (NULL);

  /* We are about to launch the final LTRANS phase, stop the WPA timer.  */
  timevar_pop (TV_WHOPR_WPA);

  if (flag_lto_partition_1to1)
    lto_1_to_1_map ();
  else
    lto_balanced_map ();

  if (!quiet_flag)
    {
      fprintf (stderr, "\nStreaming out");
      fflush (stderr);
    }
  lto_wpa_write_files ();
  ggc_collect ();
  if (!quiet_flag)
    fprintf (stderr, "\n");

  if (post_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption after IPA\n");
      dump_memory_report (false);
    }

  /* Show the LTO report before launching LTRANS.  */
  if (flag_lto_report)
    print_lto_report ();
}


static GTY(()) tree lto_eh_personality_decl;

/* Return the LTO personality function decl.  */

tree
lto_eh_personality (void)
{
  if (!lto_eh_personality_decl)
    {
      /* Use the first personality DECL for our personality if we don't
	 support multiple ones.  This ensures that we don't artificially
	 create the need for them in a single-language program.  */
      if (first_personality_decl && !dwarf2out_do_cfi_asm ())
	lto_eh_personality_decl = first_personality_decl;
      else
	lto_eh_personality_decl = lhd_gcc_personality ();
    }

  return lto_eh_personality_decl;
}

/* Set the process name based on the LTO mode. */

static void 
lto_process_name (void)
{
  if (flag_lto)
    setproctitle ("lto1-lto");
  if (flag_wpa)
    setproctitle ("lto1-wpa");
  if (flag_ltrans)
    setproctitle ("lto1-ltrans");
}

/* Main entry point for the GIMPLE front end.  This front end has
   three main personalities:

   - LTO (-flto).  All the object files on the command line are
     loaded in memory and processed as a single translation unit.
     This is the traditional link-time optimization behavior.

   - WPA (-fwpa).  Only the callgraph and summary information for
     files in the command file are loaded.  A single callgraph
     (without function bodies) is instantiated for the whole set of
     files.  IPA passes are only allowed to analyze the call graph
     and make transformation decisions.  The callgraph is
     partitioned, each partition is written to a new object file
     together with the transformation decisions.

   - LTRANS (-fltrans).  Similar to -flto but it prevents the IPA
     summary files from running again.  Since WPA computed summary
     information and decided what transformations to apply, LTRANS
     simply applies them.  */

void
lto_main (void)
{
  lto_process_name ();

  lto_init_reader ();

  /* Read all the symbols and call graph from all the files in the
     command line.  */
  read_cgraph_and_symbols (num_in_fnames, in_fnames);

  if (!seen_error ())
    {
      /* If WPA is enabled analyze the whole call graph and create an
	 optimization plan.  Otherwise, read in all the function
	 bodies and continue with optimization.  */
      if (flag_wpa)
	do_whole_program_analysis ();
      else
	{
	  materialize_cgraph ();

	  /* Let the middle end know that we have read and merged all of
	     the input files.  */ 
	  cgraph_optimize ();

	  /* FIXME lto, if the processes spawned by WPA fail, we miss
	     the chance to print WPA's report, so WPA will call
	     print_lto_report before launching LTRANS.  If LTRANS was
	     launched directly by the driver we would not need to do
	     this.  */
	  if (flag_lto_report)
	    print_lto_report ();
	}
    }
}

#include "gt-lto-lto.h"
