/* Functions for writing LTO sections.

   Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "expr.h"
#include "params.h"
#include "input.h"
#include "hashtab.h"
#include "function.h"
#include "except.h"
#include "langhooks.h"
#include "data-streamer.h"
#include "lto-streamer.h"
#include "lto-compress.h"

static vec<lto_out_decl_state_ptr> decl_state_stack;

/* List of out decl states used by functions.  We use this to
   generate the decl directory later. */

vec<lto_out_decl_state_ptr> lto_function_decl_states;


/*****************************************************************************
   Output routines shared by all of the serialization passes.
*****************************************************************************/


/* Flush compressed stream data function, sends NUM_CHARS from CHARS
   to the append lang hook, OPAQUE is currently always NULL.  */

static void
lto_append_data (const char *chars, unsigned int num_chars, void *opaque)
{
  gcc_assert (opaque == NULL);
  lang_hooks.lto.append_data (chars, num_chars, opaque);
}

/* Pointer to the current compression stream.  */

static struct lto_compression_stream *compression_stream = NULL;

/* Begin a new output section named NAME. If COMPRESS is true, zlib compress
   the section. */

void
lto_begin_section (const char *name, bool compress)
{
  lang_hooks.lto.begin_section (name);

  /* FIXME lto: for now, suppress compression if the lang_hook that appends
     data is anything other than assembler output.  The effect here is that
     we get compression of IL only in non-ltrans object files.  */
  gcc_assert (compression_stream == NULL);
  if (compress)
    compression_stream = lto_start_compression (lto_append_data, NULL);
}


/* End the current output section.  */

void
lto_end_section (void)
{
  if (compression_stream)
    {
      lto_end_compression (compression_stream);
      compression_stream = NULL;
    }
  lang_hooks.lto.end_section ();
}


/* Write all of the chars in OBS to the assembler.  Recycle the blocks
   in obs as this is being done.  */

void
lto_write_stream (struct lto_output_stream *obs)
{
  unsigned int block_size = 1024;
  struct lto_char_ptr_base *block;
  struct lto_char_ptr_base *next_block;
  if (!obs->first_block)
    return;

  for (block = obs->first_block; block; block = next_block)
    {
      const char *base = ((char *)block) + sizeof (struct lto_char_ptr_base);
      unsigned int num_chars = block_size - sizeof (struct lto_char_ptr_base);

      /* If this is not the last block, it is full.  If it is the last
	 block, left_in_block indicates how many chars are unoccupied in
	 this block; subtract from num_chars to obtain occupancy.  */
      next_block = (struct lto_char_ptr_base *) block->ptr;
      if (!next_block)
	num_chars -= obs->left_in_block;

      /* FIXME lto: WPA mode uses an ELF function as a lang_hook to append
         output data.  This hook is not happy with the way that compression
         blocks up output differently to the way it's blocked here.  So for
         now, we don't compress WPA output.  */
      if (compression_stream)
	{
	  lto_compress_block (compression_stream, base, num_chars);
	  lang_hooks.lto.append_data (NULL, 0, block);
	}
      else
	lang_hooks.lto.append_data (base, num_chars, block);
      block_size *= 2;
    }
}


/* Adds a new block to output stream OBS.  */

void
lto_append_block (struct lto_output_stream *obs)
{
  struct lto_char_ptr_base *new_block;

  gcc_assert (obs->left_in_block == 0);

  if (obs->first_block == NULL)
    {
      /* This is the first time the stream has been written
	 into.  */
      obs->block_size = 1024;
      new_block = (struct lto_char_ptr_base*) xmalloc (obs->block_size);
      obs->first_block = new_block;
    }
  else
    {
      struct lto_char_ptr_base *tptr;
      /* Get a new block that is twice as big as the last block
	 and link it into the list.  */
      obs->block_size *= 2;
      new_block = (struct lto_char_ptr_base*) xmalloc (obs->block_size);
      /* The first bytes of the block are reserved as a pointer to
	 the next block.  Set the chain of the full block to the
	 pointer to the new block.  */
      tptr = obs->current_block;
      tptr->ptr = (char *) new_block;
    }

  /* Set the place for the next char at the first position after the
     chain to the next block.  */
  obs->current_pointer
    = ((char *) new_block) + sizeof (struct lto_char_ptr_base);
  obs->current_block = new_block;
  /* Null out the newly allocated block's pointer to the next block.  */
  new_block->ptr = NULL;
  obs->left_in_block = obs->block_size - sizeof (struct lto_char_ptr_base);
}


/* Write raw DATA of length LEN to the output block OB.  */

void
lto_output_data_stream (struct lto_output_stream *obs, const void *data,
			size_t len)
{
  while (len)
    {
      size_t copy;

      /* No space left.  */
      if (obs->left_in_block == 0)
	lto_append_block (obs);

      /* Determine how many bytes to copy in this loop.  */
      if (len <= obs->left_in_block)
	copy = len;
      else
	copy = obs->left_in_block;

      /* Copy the data and do bookkeeping.  */
      memcpy (obs->current_pointer, data, copy);
      obs->current_pointer += copy;
      obs->total_size += copy;
      obs->left_in_block -= copy;
      data = (const char *) data + copy;
      len -= copy;
    }
}


/* Lookup NAME in ENCODER.  If NAME is not found, create a new entry in
   ENCODER for NAME with the next available index of ENCODER,  then
   print the index to OBS.  True is returned if NAME was added to
   ENCODER.  The resulting index is stored in THIS_INDEX.

   If OBS is NULL, the only action is to add NAME to the encoder. */

bool
lto_output_decl_index (struct lto_output_stream *obs,
		       struct lto_tree_ref_encoder *encoder,
		       tree name, unsigned int *this_index)
{
  unsigned *slot;
  unsigned int index;
  bool new_entry_p = FALSE;
  bool existed_p;

  slot = encoder->tree_hash_table->insert (name, &existed_p);
  if (!existed_p)
    {
      index = encoder->trees.length ();
      *slot = index;
      encoder->trees.safe_push (name);
      new_entry_p = TRUE;
    }
  else
    index = *slot;

  if (obs)
    streamer_write_uhwi_stream (obs, index);
  *this_index = index;
  return new_entry_p;
}

/* Output a field DECL to OBS.  */

void
lto_output_field_decl_index (struct lto_out_decl_state *decl_state,
			     struct lto_output_stream * obs, tree decl)
{
  unsigned int index;
  lto_output_decl_index (obs, &decl_state->streams[LTO_DECL_STREAM_FIELD_DECL],
			 decl, &index);
}

/* Output a function DECL to OBS.  */

void
lto_output_fn_decl_index (struct lto_out_decl_state *decl_state,
			  struct lto_output_stream * obs, tree decl)
{
  unsigned int index;
  lto_output_decl_index (obs, &decl_state->streams[LTO_DECL_STREAM_FN_DECL],
			 decl, &index);
}

/* Output a namespace DECL to OBS.  */

void
lto_output_namespace_decl_index (struct lto_out_decl_state *decl_state,
				 struct lto_output_stream * obs, tree decl)
{
  unsigned int index;
  lto_output_decl_index (obs,
			 &decl_state->streams[LTO_DECL_STREAM_NAMESPACE_DECL],
			 decl, &index);
}

/* Output a static or extern var DECL to OBS.  */

void
lto_output_var_decl_index (struct lto_out_decl_state *decl_state,
			   struct lto_output_stream * obs, tree decl)
{
  unsigned int index;
  lto_output_decl_index (obs, &decl_state->streams[LTO_DECL_STREAM_VAR_DECL],
			 decl, &index);
}

/* Output a type DECL to OBS.  */

void
lto_output_type_decl_index (struct lto_out_decl_state *decl_state,
			    struct lto_output_stream * obs, tree decl)
{
  unsigned int index;
  lto_output_decl_index (obs, &decl_state->streams[LTO_DECL_STREAM_TYPE_DECL],
			 decl, &index);
}

/* Output a type REF to OBS.  */

void
lto_output_type_ref_index (struct lto_out_decl_state *decl_state,
			   struct lto_output_stream *obs, tree ref)
{
  unsigned int index;
  lto_output_decl_index (obs, &decl_state->streams[LTO_DECL_STREAM_TYPE],
			 ref, &index);
}


/* Create the output block and return it.  */

struct lto_simple_output_block *
lto_create_simple_output_block (enum lto_section_type section_type)
{
  struct lto_simple_output_block *ob
    = ((struct lto_simple_output_block *)
       xcalloc (1, sizeof (struct lto_simple_output_block)));

  ob->section_type = section_type;
  ob->decl_state = lto_get_out_decl_state ();
  ob->main_stream = ((struct lto_output_stream *)
		     xcalloc (1, sizeof (struct lto_output_stream)));

  return ob;
}


/* Produce a simple section for one of the ipa passes.  */

void
lto_destroy_simple_output_block (struct lto_simple_output_block *ob)
{
  char *section_name;
  struct lto_simple_header header;
  struct lto_output_stream *header_stream;

  section_name = lto_get_section_name (ob->section_type, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* Write the header which says how to decode the pieces of the
     t.  */
  memset (&header, 0, sizeof (struct lto_simple_header));
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;

  header.compressed_size = 0;

  header.main_size = ob->main_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof header);
  lto_write_stream (header_stream);
  free (header_stream);

  lto_write_stream (ob->main_stream);

  /* Put back the assembly section that was there before we started
     writing lto info.  */
  lto_end_section ();

  free (ob->main_stream);
  free (ob);
}


/* Return a new lto_out_decl_state. */

struct lto_out_decl_state *
lto_new_out_decl_state (void)
{
  struct lto_out_decl_state *state = XCNEW (struct lto_out_decl_state);
  int i;

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    lto_init_tree_ref_encoder (&state->streams[i]);

  return state;
}


/* Delete STATE and components.  */

void
lto_delete_out_decl_state (struct lto_out_decl_state *state)
{
  int i;

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    lto_destroy_tree_ref_encoder (&state->streams[i]);

  free (state);
}


/* Get the currently used lto_out_decl_state structure. */

struct lto_out_decl_state *
lto_get_out_decl_state (void)
{
  return decl_state_stack.last ();
}

/* Push STATE to top of out decl stack. */

void
lto_push_out_decl_state (struct lto_out_decl_state *state)
{
  decl_state_stack.safe_push (state);
}

/* Pop the currently used out-decl state from top of stack. */

struct lto_out_decl_state *
lto_pop_out_decl_state (void)
{
  return decl_state_stack.pop ();
}

/* Record STATE after it has been used in serializing the body of
   FN_DECL.  STATE should no longer be used by the caller.  The ownership
   of it is taken over from this point.  */

void
lto_record_function_out_decl_state (tree fn_decl,
				    struct lto_out_decl_state *state)
{
  int i;

  /* Strip all hash tables to save some memory. */
  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    if (state->streams[i].tree_hash_table)
      {
	delete state->streams[i].tree_hash_table;
	state->streams[i].tree_hash_table = NULL;
      }
  state->fn_decl = fn_decl;
  lto_function_decl_states.safe_push (state);
}
