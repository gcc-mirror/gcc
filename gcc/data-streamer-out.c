/* Routines for saving various data types to a file stream.  This deals
   with various data types like strings, integers, enums, etc.

   Copyright 2011 Free Software Foundation, Inc.
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
#include "data-streamer.h"

/* Return index used to reference STRING of LEN characters in the string table
   in OB.  The string might or might not include a trailing '\0'.
   Then put the index onto the INDEX_STREAM.  
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

unsigned
lto_string_index (struct output_block *ob, const char *s, unsigned int len,
		  bool persistent)
{
  struct string_slot **slot;
  struct string_slot s_slot;

  s_slot.s = s;
  s_slot.len = len;
  s_slot.slot_num = 0;

  slot = (struct string_slot **) htab_find_slot (ob->string_hash_table,
						 &s_slot, INSERT);
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
      lto_output_uleb128_stream (string_stream, len);
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
lto_output_string_with_length (struct output_block *ob,
			       struct lto_output_stream *index_stream,
			       const char *s, unsigned int len, bool persistent)
{
  if (s)
    lto_output_uleb128_stream (index_stream,
			       lto_string_index (ob, s, len, persistent));
  else
    lto_output_1_stream (index_stream, 0);
}


/* Output the '\0' terminated STRING to the string
   table in OB.  Then put the index onto the INDEX_STREAM.
   When PERSISTENT is set, the string S is supposed to not change during
   duration of the OB and thus OB can keep pointer into it.  */

void
lto_output_string (struct output_block *ob,
	           struct lto_output_stream *index_stream,
	           const char *string, bool persistent)
{
  if (string)
    lto_output_string_with_length (ob, index_stream, string,
				   strlen (string) + 1,
				   persistent);
  else
    lto_output_1_stream (index_stream, 0);
}


/* Write a zero to the output stream.  */

void
output_zero (struct output_block *ob)
{
  lto_output_1_stream (ob->main_stream, 0);
}


/* Output an unsigned LEB128 quantity to OB->main_stream.  */

void
output_uleb128 (struct output_block *ob, unsigned HOST_WIDE_INT work)
{
  lto_output_uleb128_stream (ob->main_stream, work);
}


/* Output a signed LEB128 quantity to OB->main_stream.  */

void
output_sleb128 (struct output_block *ob, HOST_WIDE_INT work)
{
  lto_output_sleb128_stream (ob->main_stream, work);
}


/* Output an unsigned LEB128 quantity to OBS.  */

void
lto_output_uleb128_stream (struct lto_output_stream *obs,
			   unsigned HOST_WIDE_INT work)
{
  do
    {
      unsigned int byte = (work & 0x7f);
      work >>= 7;
      if (work != 0)
	/* More bytes to follow.  */
	byte |= 0x80;

      lto_output_1_stream (obs, byte);
    }
  while (work != 0);
}


/* Identical to output_uleb128_stream above except using unsigned
   HOST_WIDEST_INT type.  For efficiency on host where unsigned HOST_WIDEST_INT
   is not native, we only use this if we know that HOST_WIDE_INT is not wide
   enough.  */

void
lto_output_widest_uint_uleb128_stream (struct lto_output_stream *obs,
				       unsigned HOST_WIDEST_INT work)
{
  do
    {
      unsigned int byte = (work & 0x7f);
      work >>= 7;
      if (work != 0)
	/* More bytes to follow.  */
	byte |= 0x80;

      lto_output_1_stream (obs, byte);
    }
  while (work != 0);
}


/* Output a signed LEB128 quantity.  */

void
lto_output_sleb128_stream (struct lto_output_stream *obs, HOST_WIDE_INT work)
{
  int more, byte;

  do
    {
      byte = (work & 0x7f);
      /* arithmetic shift */
      work >>= 7;
      more = !((work == 0 && (byte & 0x40) == 0)
	       || (work == -1 && (byte & 0x40) != 0));
      if (more)
	byte |= 0x80;

      lto_output_1_stream (obs, byte);
    }
  while (more);
}
