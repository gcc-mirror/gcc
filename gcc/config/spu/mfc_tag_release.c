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

/* Release the specified DMA tag from exclusive use.  Once released, the
   tag is available for future reservation.  Upon sucessful release,
   MFC_DMA_TAG_VALID is returned.  If the specified tag is not in the
   range 0 to 31, or had not been reserved, no action is taken and
   MFC_DMA_TAG_INVALID is returned.  */

unsigned int
__mfc_tag_release (unsigned int tag)
{
  vector unsigned int is_invalid;
  vector unsigned int mask = (vector unsigned int)
	{ 0x80000000, 0x80000000, 0x80000000, 0x80000000 };
  vector signed int zero = (vector signed int) { 0, 0, 0, 0 };

  vector signed int has_been_reserved;

  /* Check if the tag is out of range.  */
  is_invalid = spu_cmpgt (spu_promote (tag, 0), 31);

  /* Check whether the tag has been reserved, set to all 1 if has not
     been reserved, 0 otherwise.  */
  has_been_reserved = (vector signed int) spu_rl (__mfc_tag_table, tag);
  has_been_reserved = (vector signed int) spu_cmpgt (zero, has_been_reserved);

  /* Set invalid.  */
  is_invalid = spu_or ((vector unsigned int) has_been_reserved, is_invalid);

  mask = spu_rlmask (mask, (int)(-tag));
  __mfc_tag_table = spu_or (__mfc_tag_table, mask);

  return spu_extract(is_invalid, 0);
}

