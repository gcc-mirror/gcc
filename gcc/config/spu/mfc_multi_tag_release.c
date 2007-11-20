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

/* Release a sequential group of tags from exclusive use. The sequential
   group of tags is the range starting from <first_tag> through
   <first_tag>+<number_of_tags>-1. Upon sucessful release, MFC_DMA_TAG_VALID
   is returned and the tags become available for future reservation.

   If the specified tags were not previously reserved, no action is
   taken and MFC_DMA_TAG_INVALID is returned.  */

unsigned int
__mfc_multi_tag_release (unsigned int first_tag, unsigned int number_of_tags)
{
  vector unsigned int table_copy, tmp, tmp1;
  vector unsigned int one = (vector unsigned int)
        { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF };
  vector unsigned int is_invalid;
  unsigned int last_tag;
  vector unsigned int has_been_reserved;

  last_tag = first_tag + number_of_tags;

  table_copy = spu_sl (one, number_of_tags);
  table_copy = spu_rl (table_copy, -last_tag);
  table_copy = spu_xor (table_copy, -1);

  /* Make sure the tags are in range and valid.  */
  tmp = spu_cmpgt (spu_promote(last_tag, 0), 32);
  tmp1 = spu_cmpgt (spu_promote(number_of_tags, 0), 32);
  is_invalid =  spu_cmpgt (spu_promote(first_tag, 0), 31);

  /* All bits are set to 1 if invalid, 0 if valid.  */
  is_invalid = spu_or (tmp, is_invalid);
  is_invalid = spu_or (tmp1, is_invalid);

  /* check whether these tags have been reserved */
  tmp = spu_rlmask (one, (int)-number_of_tags);
  tmp1 = spu_sl (__mfc_tag_table, first_tag);
  has_been_reserved = spu_cmpgt(tmp1, tmp);

  is_invalid = spu_or (has_been_reserved, is_invalid);

  table_copy = spu_sel (__mfc_tag_table, table_copy, table_copy);
  __mfc_tag_table = spu_sel (table_copy, __mfc_tag_table, is_invalid);

  return spu_extract (is_invalid, 0);
}

