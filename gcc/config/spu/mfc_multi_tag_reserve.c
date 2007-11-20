/* Copyright (C) 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include <spu_mfcio.h>
extern vector unsigned int __mfc_tag_table;

/* Reserve a sequential group of tags for exclusive use.  The number of
   tags to be reserved is specified by the <number_of_tags> parameter.
   This routine returns the first tag ID for a sequential list of
   available tags and marks them as reserved. The reserved group
   of tags is in the range starting from the returned tag through
   the returned tag + <number_of_tags>-1.

   If the number of tags requested exceeds the number of available
   sequential tags, then MFC_DMA_TAG_INVALID is returned indicating
   that the request could not be serviced.  */

unsigned int
__mfc_multi_tag_reserve (unsigned int number_of_tags)
{
  vector unsigned int table_copy;
  vector unsigned int one = (vector unsigned int)
        { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF };
  vector unsigned int count_busy, is_valid;
  vector unsigned int count_total;
  vector unsigned int count_avail = (vector unsigned int) { 0, 0, 0, 0 };
  vector unsigned int index = (vector unsigned int) { 0, 0, 0, 0 };

  table_copy = __mfc_tag_table;


  /* count_busy: number of consecutive busy tags
     count_avail: number of consecutive free tags
     table_copy: temporary copy of the tag table
     count_total: sum of count_busy and count_avail
     index: index of the current working tag  */
  do
    {
      table_copy = spu_sl (table_copy, count_avail);

      count_busy = spu_cntlz (table_copy);
      table_copy = spu_sl (table_copy, count_busy);
      count_avail = spu_cntlz (spu_xor(table_copy, -1));
      count_total = spu_add (count_busy, count_avail);
      index = spu_add (index, count_total);
    }
  while (spu_extract (count_avail, 0) < number_of_tags
	 && spu_extract (table_copy, 0) != 0);

  index = spu_sub (index, count_avail);

  /* is_valid is set to 0xFFFFFFFF if table_copy == 0, 0 otherwise.  */
  is_valid = spu_cmpeq (table_copy, 0);
  index = spu_sel (index, is_valid, is_valid);

  /* Now I need to actually mark the tags as used.  */
  table_copy = spu_sl (one, number_of_tags);
  table_copy = spu_rl (table_copy, -number_of_tags - spu_extract (index, 0));
  table_copy = spu_sel (table_copy, __mfc_tag_table, table_copy);
  __mfc_tag_table = spu_sel (table_copy, __mfc_tag_table, is_valid);

  return spu_extract (index, 0);
}

