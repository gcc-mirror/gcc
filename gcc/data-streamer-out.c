/* Routines for saving various data types to a file stream.  This deals
   with various data types like strings, integers, enums, etc.

   Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "data-streamer.h"

/* Return index used to reference STRING of LEN characters in the string table
   in OB.  The string might or might not include a trailing '\0'.
   Then put the index onto the INDEX_STREAM.  
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

unsigned
streamer_string_index (struct output_block *ob, const char *s, unsigned int len,
		       bool persistent)
{
  struct string_slot **slot;
  struct string_slot s_slot;

  s_slot.s = s;
  s_slot.len = len;
  s_slot.slot_num = 0;

  slot = ob->string_hash_table.find_slot (&s_slot, INSERT);
  if (*slot == NULL)
    {
      struct lto_output_stream *string_stream = ob->string_stream;
      unsigned int start = string_stream->total_size;
      struct string_slot *new_slot = XOBNEW (&ob->obstack, struct string_slot);
      const char *string;

      if (!persistent)
	{
	  char *tmp;
	  string = tmp = XOBNEWVEC (&ob->obstack, char, len);
          memcpy (tmp, s, len);
        }
      else
	string = s;

      new_slot->s = string;
      new_slot->len = len;
      new_slot->slot_num = start;
      *slot = new_slot;
      streamer_write_uhwi_stream (string_stream, len);
      lto_output_data_stream (string_stream, string, len);
      return start + 1;
    }
  else
    {
      struct string_slot *old_slot = *slot;
      return old_slot->slot_num + 1;
    }
}


/* Output STRING of LEN characters to the string table in OB. The
   string might or might not include a trailing '\0'. Then put the
   index onto the INDEX_STREAM. 
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

void
streamer_write_string_with_length (struct output_block *ob,
				   struct lto_output_stream *index_stream,
				   const char *s, unsigned int len,
				   bool persistent)
{
  if (s)
    streamer_write_uhwi_stream (index_stream,
			        streamer_string_index (ob, s, len, persistent));
  else
    streamer_write_char_stream (index_stream, 0);
}


/* Output the '\0' terminated STRING to the string
   table in OB.  Then put the index onto the INDEX_STREAM.
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

void
streamer_write_string (struct output_block *ob,
		       struct lto_output_stream *index_stream,
		       const char *string, bool persistent)
{
  if (string)
    streamer_write_string_with_length (ob, index_stream, string,
				       strlen (string) + 1,
				       persistent);
  else
    streamer_write_char_stream (index_stream, 0);
}


/* Output STRING of LEN characters to the string table in OB.  Then
   put the index into BP.
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

void
bp_pack_string_with_length (struct output_block *ob, struct bitpack_d *bp,
			    const char *s, unsigned int len, bool persistent)
{
  unsigned index = 0;
  if (s)
    index = streamer_string_index (ob, s, len, persistent);
  bp_pack_var_len_unsigned (bp, index);
}


/* Output the '\0' terminated STRING to the string
   table in OB.  Then put the index onto the bitpack BP.
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

void
bp_pack_string (struct output_block *ob, struct bitpack_d *bp,
		const char *s, bool persistent)
{
  unsigned index = 0;
  if (s)
    index = streamer_string_index (ob, s, strlen (s) + 1, persistent);
  bp_pack_var_len_unsigned (bp, index);
}



/* Write a zero to the output stream.  */

void
streamer_write_zero (struct output_block *ob)
{
  streamer_write_char_stream (ob->main_stream, 0);
}


/* Write an unsigned HOST_WIDE_INT value WORK to OB->main_stream.  */

void
streamer_write_uhwi (struct output_block *ob, unsigned HOST_WIDE_INT work)
{
  streamer_write_uhwi_stream (ob->main_stream, work);
}


/* Write a HOST_WIDE_INT value WORK to OB->main_stream.  */

void
streamer_write_hwi (struct output_block *ob, HOST_WIDE_INT work)
{
  streamer_write_hwi_stream (ob->main_stream, work);
}

/* Write a gcov counter value WORK to OB->main_stream.  */

void
streamer_write_gcov_count (struct output_block *ob, gcov_type work)
{
  streamer_write_gcov_count_stream (ob->main_stream, work);
}

/* Write an unsigned HOST_WIDE_INT value WORK to OBS.  */

void
streamer_write_uhwi_stream (struct lto_output_stream *obs,
                            unsigned HOST_WIDE_INT work)
{
  if (obs->left_in_block == 0)
    lto_append_block (obs);
  char *current_pointer = obs->current_pointer;
  unsigned int left_in_block = obs->left_in_block;
  unsigned int size = 0;
  do
    {
      unsigned int byte = (work & 0x7f);
      work >>= 7;
      if (work != 0)
	/* More bytes to follow.  */
	byte |= 0x80;

      *(current_pointer++) = byte;
      left_in_block--;
      size++;
    }
  while (work != 0 && left_in_block > 0);
  if (work != 0)
    {
      obs->left_in_block = 0;
      lto_append_block (obs);
      current_pointer = obs->current_pointer;
      left_in_block = obs->left_in_block;
      do
	{
	  unsigned int byte = (work & 0x7f);
	  work >>= 7;
	  if (work != 0)
	    /* More bytes to follow.  */
	    byte |= 0x80;

	  *(current_pointer++) = byte;
	  left_in_block--;
	  size++;
	}
      while (work != 0);
    }
  obs->current_pointer = current_pointer;
  obs->left_in_block = left_in_block;
  obs->total_size += size;
}


/* Write a HOST_WIDE_INT value WORK to OBS.  */

void
streamer_write_hwi_stream (struct lto_output_stream *obs, HOST_WIDE_INT work)
{
  if (obs->left_in_block == 0)
    lto_append_block (obs);
  char *current_pointer = obs->current_pointer;
  unsigned int left_in_block = obs->left_in_block;
  unsigned int size = 0;
  bool more;
  do
    {
      unsigned int byte = (work & 0x7f);
      /* If the lower 7-bits are sign-extended 0 or -1 we are finished.  */
      work >>= 6;
      more = !(work == 0 || work == -1);
      if (more)
	{
	  /* More bits to follow.  */
	  work >>= 1;
	  byte |= 0x80;
	}

      *(current_pointer++) = byte;
      left_in_block--;
      size++;
    }
  while (more && left_in_block > 0);
  if (more)
    {
      obs->left_in_block = 0;
      lto_append_block (obs);
      current_pointer = obs->current_pointer;
      left_in_block = obs->left_in_block;
      do
	{
	  unsigned int byte = (work & 0x7f);
	  work >>= 6;
	  more = !(work == 0 || work == -1);
	  if (more)
	    {
	      work >>= 1;
	      byte |= 0x80;
	    }

	  *(current_pointer++) = byte;
	  left_in_block--;
	  size++;
	}
      while (more);
    }
  obs->current_pointer = current_pointer;
  obs->left_in_block = left_in_block;
  obs->total_size += size;
}

/* Write a GCOV counter value WORK to OBS.  */

void
streamer_write_gcov_count_stream (struct lto_output_stream *obs, gcov_type work)
{
  gcc_assert (work >= 0);
  gcc_assert ((HOST_WIDE_INT) work == work);
  streamer_write_hwi_stream (obs, work);
}
