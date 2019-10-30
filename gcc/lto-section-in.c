/* Input functions for reading LTO sections.

   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "lto-compress.h"

/* Section names.  These must correspond to the values of
   enum lto_section_type.  */
const char *lto_section_name[LTO_N_SECTION_TYPES] =
{
  "decls",
  "function_body",
  "statics",
  "symtab",
  "refs",
  "asm",
  "jmpfuncs",
  "pureconst",
  "reference",
  "profile",
  "symbol_nodes",
  "opts",
  "cgraphopt",
  "inline",
  "ipcp_trans",
  "icf",
  "offload_table",
  "mode_table",
  "hsa",
  "lto",
  "ipa_sra"
};

/* Hooks so that the ipa passes can call into the lto front end to get
   sections.  */

static struct lto_file_decl_data ** file_decl_data;
static lto_get_section_data_f* get_section_f;
static lto_free_section_data_f* free_section_f;


/* This is called from the lto front end to set up the hooks that are
   used by the ipa passes to get the data that they will
   deserialize.  */

void
lto_set_in_hooks (struct lto_file_decl_data ** data,
		  lto_get_section_data_f* get_f,
		  lto_free_section_data_f* free_f)
{
  file_decl_data = data;
  get_section_f = get_f;
  free_section_f = free_f;
}


/* Return an array of file decl datas for all of the files passed to
   this compilation.  */

struct lto_file_decl_data **
lto_get_file_decl_data (void)
{
  gcc_assert (file_decl_data);
  return file_decl_data;
}

/* Buffer structure for accumulating data from compression callbacks.  */

struct lto_buffer
{
  char *data;
  size_t length;
};

/* Compression callback, append LENGTH bytes from DATA to the buffer pointed
   to by OPAQUE.  */

static void
lto_append_data (const char *data, unsigned length, void *opaque)
{
  struct lto_buffer *buffer = (struct lto_buffer *) opaque;

  buffer->data = (char *) xrealloc (buffer->data, buffer->length + length);
  memcpy (buffer->data + buffer->length, data, length);
  buffer->length += length;
}

/* Header placed in returned uncompressed data streams.  Allows the
   uncompressed allocated data to be mapped back to the underlying
   compressed data for use with free_section_f.  */

struct lto_data_header
{
  const char *data;
  size_t len;
};

/* Return a char pointer to the start of a data stream for an LTO pass
   or function.  FILE_DATA indicates where to obtain the data.
   SECTION_TYPE is the type of information to be obtained.  NAME is
   the name of the function and is only used when finding a function
   body; otherwise it is NULL.  LEN is the size of the data
   returned.  */

const char *
lto_get_section_data (struct lto_file_decl_data *file_data,
		      enum lto_section_type section_type,
		      const char *name, int order,
		      size_t *len, bool decompress)
{
  const char *data = (get_section_f) (file_data, section_type, name, order,
				      len);
  const size_t header_length = sizeof (struct lto_data_header);
  struct lto_data_header *header;
  struct lto_buffer buffer;
  struct lto_compression_stream *stream;
  lto_stats.section_size[section_type] += *len;

  if (data == NULL)
    return NULL;

  /* WPA->ltrans streams are not compressed with exception of function bodies
     and variable initializers that has been verbatim copied from earlier
     compilations.  */
  if ((!flag_ltrans || decompress) && section_type != LTO_section_lto)
    {
      /* Create a mapping header containing the underlying data and length,
	 and prepend this to the uncompression buffer.  The uncompressed data
	 then follows, and a pointer to the start of the uncompressed data is
	 returned.  */
      header = (struct lto_data_header *) xmalloc (header_length);
      header->data = data;
      header->len = *len;

      buffer.data = (char *) header;
      buffer.length = header_length;

      stream = lto_start_uncompression (lto_append_data, &buffer);
      lto_uncompress_block (stream, data, *len);
      lto_end_uncompression (stream,
			     file_data->lto_section_header.get_compression ());

      *len = buffer.length - header_length;
      data = buffer.data + header_length;
    }

  return data;
}

/* Return a char pointer to the start of a data stream for an LTO pass.
   FILE_DATA indicates where to obtain the data.
   SECTION_TYPE is the type of information to be obtained.
   LEN is the size of the data returned.  */

const char *
lto_get_summary_section_data (struct lto_file_decl_data *file_data,
			      enum lto_section_type section_type, size_t *len)
{
  return lto_get_section_data (file_data, section_type, NULL, 0, len);
}

/* Get the section data without any header parsing or uncompression.  */

const char *
lto_get_raw_section_data (struct lto_file_decl_data *file_data,
			  enum lto_section_type section_type,
			  const char *name, int order,
			  size_t *len)
{
  return (get_section_f) (file_data, section_type, name, order, len);
}

/* Free the data found from the above call.  The first three
   parameters are the same as above.  DATA is the data to be freed and
   LEN is the length of that data.  */

void
lto_free_section_data (struct lto_file_decl_data *file_data,
		       enum lto_section_type section_type,
		       const char *name,
		       const char *data,
		       size_t len, bool decompress)
{
  const size_t header_length = sizeof (struct lto_data_header);
  const char *real_data = data - header_length;
  const struct lto_data_header *header
    = (const struct lto_data_header *) real_data;

  gcc_assert (free_section_f);

  if (flag_ltrans && !decompress)
    {
      (free_section_f) (file_data, section_type, name, data, len);
      return;
    }

  /* The underlying data address has been extracted from the mapping header.
     Free that, then free the allocated uncompression buffer.  */
  (free_section_f) (file_data, section_type, name, header->data, header->len);
  free (CONST_CAST (char *, real_data));
}

/* Free data allocated by lto_get_raw_section_data.  */

void
lto_free_raw_section_data (struct lto_file_decl_data *file_data,
		           enum lto_section_type section_type,
		           const char *name,
		           const char *data,
		           size_t len)
{
  (free_section_f) (file_data, section_type, name, data, len);
}

/* Load a section of type SECTION_TYPE from FILE_DATA, parse the
   header and then return an input block pointing to the section.  The
   raw pointer to the section is returned in DATAR and LEN.  These are
   used to free the section.  Return NULL if the section is not present.  */

class lto_input_block *
lto_create_simple_input_block (struct lto_file_decl_data *file_data,
			       enum lto_section_type section_type,
			       const char **datar, size_t *len)
{
  const char *data = lto_get_section_data (file_data, section_type, NULL, 0,
					   len);
  const struct lto_simple_header * header
    = (const struct lto_simple_header *) data;

  int main_offset = sizeof (struct lto_simple_header);

  if (!data)
    return NULL;

  *datar = data;
  return new lto_input_block (data + main_offset, header->main_size,
			      file_data->mode_table);
}


/* Close the section returned from a call to
   LTO_CREATE_SIMPLE_INPUT_BLOCK.  IB is the input block returned from
   that call.  The FILE_DATA and SECTION_TYPE are the same as what was
   passed to that call and the DATA and LEN are what was returned from
   that call.  */

void
lto_destroy_simple_input_block (struct lto_file_decl_data *file_data,
				enum lto_section_type section_type,
				class lto_input_block *ib,
				const char *data, size_t len)
{
  delete ib;
  lto_free_section_data (file_data, section_type, NULL, data, len);
}

/*****************************************************************************/
/* Record renamings of static declarations                                   */
/*****************************************************************************/

struct lto_renaming_slot
{
  const char *old_name;
  const char *new_name;
};

/* Returns a hash code for P.  */

static hashval_t
hash_name (const void *p)
{
  const struct lto_renaming_slot *ds = (const struct lto_renaming_slot *) p;
  return (hashval_t) htab_hash_string (ds->new_name);
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_name (const void *p1, const void *p2)
{
  const struct lto_renaming_slot *s1 =
    (const struct lto_renaming_slot *) p1;
  const struct lto_renaming_slot *s2 =
    (const struct lto_renaming_slot *) p2;

  return strcmp (s1->new_name, s2->new_name) == 0;
}

/* Free a renaming table entry.  */

static void
renaming_slot_free (void *slot)
{
  struct lto_renaming_slot *s = (struct lto_renaming_slot *) slot;

  free (CONST_CAST (void *, (const void *) s->old_name));
  free (CONST_CAST (void *, (const void *) s->new_name));
  free ((void *) s);
}

/* Create an empty hash table for recording declaration renamings.  */

htab_t
lto_create_renaming_table (void)
{
  return htab_create (37, hash_name, eq_name, renaming_slot_free);
}

/* Record a declaration name mapping OLD_NAME -> NEW_NAME.  DECL_DATA
   holds the renaming hash table to use.  */

void
lto_record_renamed_decl (struct lto_file_decl_data *decl_data,
			 const char *old_name, const char *new_name)
{
  void **slot;
  struct lto_renaming_slot r_slot;

  r_slot.new_name = new_name;
  slot = htab_find_slot (decl_data->renaming_hash_table, &r_slot, INSERT);
  if (*slot == NULL)
    {
      struct lto_renaming_slot *new_slot = XNEW (struct lto_renaming_slot);
      new_slot->old_name = xstrdup (old_name);
      new_slot->new_name = xstrdup (new_name);
      *slot = new_slot;
    }
  else
    gcc_unreachable ();
}


/* Given a string NAME, return the string that it has been mapped to
   by lto_record_renamed_decl.  If NAME was not renamed, it is
   returned unchanged.  DECL_DATA holds the renaming hash table to use.  */

const char *
lto_get_decl_name_mapping (struct lto_file_decl_data *decl_data,
			   const char *name)
{
  htab_t renaming_hash_table = decl_data->renaming_hash_table;
  struct lto_renaming_slot *slot;
  struct lto_renaming_slot r_slot;

  r_slot.new_name = name;
  slot = (struct lto_renaming_slot *) htab_find (renaming_hash_table, &r_slot);
  if (slot)
    return slot->old_name;
  else
    return name;
}

/*****************************************************************************/
/* Input decl state object.                                                  */
/*****************************************************************************/

/* Return a newly created in-decl state object. */

struct lto_in_decl_state *
lto_new_in_decl_state (void)
{
  return ggc_cleared_alloc<lto_in_decl_state> ();
}

/* Delete STATE and its components. */

void
lto_delete_in_decl_state (struct lto_in_decl_state *state)
{
  int i;

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    vec_free (state->streams[i]);
  ggc_free (state);
}

/* Search the in-decl state of a function FUNC contained in the file
   associated with FILE_DATA.  Return NULL if not found.  */

struct lto_in_decl_state*
lto_get_function_in_decl_state (struct lto_file_decl_data *file_data,
				tree func)
{
  struct lto_in_decl_state temp;
  lto_in_decl_state **slot;

  temp.fn_decl = func;
  slot = file_data->function_decl_states->find_slot (&temp, NO_INSERT);
  return slot? *slot : NULL;
}

/* Free decl_states.  */

void
lto_free_function_in_decl_state (struct lto_in_decl_state *state)
{
  int i;
  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    vec_free (state->streams[i]);
  ggc_free (state);
}

/* Free decl_states associated with NODE.  This makes it possible to furhter
   release trees needed by the NODE's body.  */

void
lto_free_function_in_decl_state_for_node (symtab_node *node)
{
  struct lto_in_decl_state temp;
  lto_in_decl_state **slot;

  if (!node->lto_file_data)
    return;

  temp.fn_decl = node->decl;
  slot
    = node->lto_file_data->function_decl_states->find_slot (&temp, NO_INSERT);
  if (slot && *slot)
    {
      lto_free_function_in_decl_state (*slot);
      node->lto_file_data->function_decl_states->clear_slot (slot);
    }
  node->lto_file_data = NULL;
}


/* Report read pass end of the section.  */

void
lto_section_overrun (class lto_input_block *ib)
{
  fatal_error (input_location, "bytecode stream: trying to read %d bytes "
	       "after the end of the input buffer", ib->p - ib->len);
}

/* Report out of range value.  */

void
lto_value_range_error (const char *purpose, HOST_WIDE_INT val,
		       HOST_WIDE_INT min, HOST_WIDE_INT max)
{
  fatal_error (input_location,
	       "%s out of range: Range is %i to %i, value is %i",
	       purpose, (int)min, (int)max, (int)val);
}
