/* Functions for writing LTO sections.

   Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
#include "data-streamer.h"
#include "langhooks.h"
#include "lto-compress.h"
#include "print-tree.h"

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

  if (streamer_dump_file)
    {
      if (flag_dump_unnumbered || flag_dump_noaddr)
	  fprintf (streamer_dump_file, "Creating %ssection\n",
		   compress ? "compressed " : "");
	else
	  fprintf (streamer_dump_file, "Creating %ssection %s\n",
		   compress ? "compressed " : "", name);
    }
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

/* Write SIZE bytes starting at DATA to the assembler.  */

void
lto_write_data (const void *data, unsigned int size)
{
  if (compression_stream)
    lto_compress_block (compression_stream, (const char *)data, size);
  else
    lang_hooks.lto.append_data ((const char *)data, size, NULL);
}

/* Write SIZE bytes starting at DATA to the assembler.  */

void
lto_write_raw_data (const void *data, unsigned int size)
{
  lang_hooks.lto.append_data ((const char *)data, size, NULL);
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

      if (compression_stream)
	lto_compress_block (compression_stream, base, num_chars);
      else
	lang_hooks.lto.append_data (base, num_chars, block);
      free (block);
      block_size *= 2;
    }
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

  section_name = lto_get_section_name (ob->section_type, NULL, 0, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* Write the header which says how to decode the pieces of the
     t.  */
  memset (&header, 0, sizeof (struct lto_simple_header));
  header.main_size = ob->main_stream->total_size;
  lto_write_data (&header, sizeof header);

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

  /* At WPA time we do not compress sections by default.  */
  state->compressed = !flag_wpa;

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
