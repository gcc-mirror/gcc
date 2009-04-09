/* Copyright (C) 2007, 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* { dg-do run } */

#include <spu_mfcio.h>
#include <stdlib.h>

/* This test directly accesses the internal table used
   by the MFC tag manager.  */
extern vector unsigned int __mfc_tag_table;


/* This tag tests invalid tag release.  Invalid tag release does
   nothing to the tag table.  */
void
test_tag_release01 (void)
{
  unsigned int copy;
  copy = spu_extract (__mfc_tag_table, 0);

  mfc_tag_release (35);
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();
}

/* More invalid release tests.  */
void
test_tag_release_invalid (void)
{
  unsigned int copy;
  copy = spu_extract (__mfc_tag_table, 0);

  if (mfc_tag_release (32) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();

  if (mfc_tag_release (17) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();
}

/* Invalid multiple-tag release tests.  */
void
test_tag_group_release_invalid (void)
{
  unsigned int copy;
  copy = spu_extract (__mfc_tag_table, 0);

  if (mfc_multi_tag_release (32, 10) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();

  if (mfc_multi_tag_release (28, 10) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();

  if (mfc_multi_tag_release (17, 10) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();

  if (mfc_multi_tag_release (32, 10) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();
}

/* The tag table should be in a pristine mode to run this test.  */
void
test_tag_reserve01 (void)
{
  unsigned int correct_table[32] =
    {
		  0x80000000, 0xC0000000, 0xE0000000,
      0xF0000000, 0xF8000000, 0xFC000000, 0xFE000000,
      0xFF000000, 0xFF800000, 0xFFC00000, 0xFFE00000,
      0xFFF00000, 0xFFF80000, 0xFFFC0000, 0xFFFE0000,
      0xFFFF0000, 0xFFFF8000, 0xFFFFC000, 0xFFFFE000,
      0xFFFFF000, 0xFFFFF800, 0xFFFFFC00, 0xFFFFFE00,
      0xFFFFFF00, 0xFFFFFF80, 0xFFFFFFC0, 0xFFFFFFE0,
      0xFFFFFFF0, 0xFFFFFFF8, 0xFFFFFFFC, 0xFFFFFFFE,
      0xFFFFFFFF
    };

  unsigned int tag;
  unsigned int i;

  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve ();
      if (tag != i)
	abort ();
    }

  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve ();
      if (tag != MFC_TAG_INVALID)
	abort ();
    }

  for (i = 0; i < 32; i++)
    {
      mfc_tag_release (i);
      if (spu_extract (__mfc_tag_table, 0) != correct_table[i])
	abort ();
    }
}

/* The tag table should be in a pristine mode to run this test.  */
void
test_tag_reserve02 (void)
{
  unsigned int correct_table[32] =
    {
      0x80000000, 0xC0000000, 0xA0000000, 0xF0000000,
      0xA8000000, 0xFC000000, 0xAA000000, 0xFF000000,
      0xAA800000, 0xFFC00000, 0xAAA00000, 0xFFF00000,
      0xAAA80000, 0xFFFC0000, 0xAAAA0000, 0xFFFF0000,
      0xAAAA8000, 0xFFFFC000, 0xAAAAA000, 0xFFFFF000,
      0xAAAAA800, 0xFFFFFC00, 0xAAAAAA00, 0xFFFFFF00,
      0xAAAAAA80, 0xFFFFFFC0, 0xAAAAAAA0, 0xFFFFFFF0,
      0xAAAAAAA8, 0xFFFFFFFC, 0xAAAAAAAA, 0xFFFFFFFF
    };

  unsigned int correct_table2[32] =
    {
      0x80000000, 0xEAAAAAAA, 0xA0000000, 0xFAAAAAAA,
      0xA8000000, 0xFEAAAAAA, 0xAA000000, 0xFFAAAAAA,
      0xAA800000, 0xFFEAAAAA, 0xAAA00000, 0xFFFAAAAA,
      0xAAA80000, 0xFFFEAAAA, 0xAAAA0000, 0xFFFFAAAA,
      0xAAAA8000, 0xFFFFEAAA, 0xAAAAA000, 0xFFFFFAAA,
      0xAAAAA800, 0xFFFFFEAA, 0xAAAAAA00, 0xFFFFFFAA,
      0xAAAAAA80, 0xFFFFFFEA, 0xAAAAAAA0, 0xFFFFFFFA,
      0xAAAAAAA8, 0xFFFFFFFE, 0xAAAAAAAA, 0xFFFFFFFF
    };

  unsigned int tag;
  unsigned int i;

  /* Reserve all 32 tags.  */
  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve();
      if (tag != i)
	abort ();
    }

  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve();
      if (tag != MFC_TAG_INVALID)
	abort ();
    }

  /* Release only 16 tags with a stride of 2.  */
  for (i = 0; i < 32; i += 2)
    {
      mfc_tag_release (i);
      if (spu_extract (__mfc_tag_table, 0) != correct_table[i])
	abort ();
    }

  /* Release the other 16 tags with a stride of 2.  */
  for (i = 1; i < 32; i += 2)
    {
      mfc_tag_release (i);
      if (spu_extract (__mfc_tag_table, 0) != correct_table2[i])
	abort ();
    }
}

/* The tag table should be in a pristine mode to run this test.  */
void
test_tag_reserve03 (void)
{
  unsigned int tag;
  unsigned int i;

  /* Reserve all 32 tags.  */
  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve ();
      if (tag != i)
	abort ();
    }

  for (i = 0; i < 32; i++)
    {
      tag = mfc_tag_reserve ();
      if (tag != MFC_TAG_INVALID)
	abort ();
    }

  /* Release only 16 tags with a stride of 2.  */
  for (i = 0; i < 32; i += 2)
    mfc_tag_release (i);

  /* Now let's re-reserve those tags.  */
  for (i = 0; i < 32; i += 2)
    {
      tag = mfc_tag_reserve ();
      if (tag != i)
	abort ();
    }

  /* Release all tags.  */
  for (i = 0; i < 32; i++)
    mfc_tag_release (i);

  if (spu_extract (__mfc_tag_table,0) != 0xFFFFFFFF)
    abort ();
}


void
test_tag_group_reserve (void)
{
  unsigned int tag;
  unsigned int i;
  unsigned int copy;

  /* Reserve all tags.  */
  for (i = 0; i < 32; i++)
    mfc_tag_reserve();

  /* Release the first 4. */
  for (i = 0; i < 4; i++)
    mfc_tag_release (i);

  /* Release tag 5 to 7.  */
  for (i = 5; i < 8; i++)
    mfc_tag_release (i);

  /* Release tag 9 to 19.  */
  for (i = 9; i < 20; i++)
    mfc_tag_release (i);

  /* Tag table should be 0xF77FF000.  */
  if (spu_extract (__mfc_tag_table, 0) != 0xF77FF000)
    abort ();


  /* Verify invalid release is detected.  */
  copy = spu_extract (__mfc_tag_table, 0);
  if (mfc_multi_tag_release (1, 5) != MFC_TAG_INVALID)
    abort ();
  if (copy != spu_extract (__mfc_tag_table, 0))
    abort ();


  /* Reserve multiple tags.  */
  tag = mfc_multi_tag_reserve (5);
  if (tag != 9)
    abort ();

  /* Tag table should be 0xF703F000.  */
  if (spu_extract (__mfc_tag_table, 0) != 0xF703F000)
    abort ();


  /* Release 5 tags in the group.  */
  mfc_multi_tag_release (tag, 5);

  /* Tag table should be 0xF77FF000.  */
  if (spu_extract (__mfc_tag_table, 0) != 0xF77FF000)
    abort ();


  /* This call should not do anything.  */
  mfc_multi_tag_release (32, 5);

  /* Tag table should be 0xF77FF000.  */
  if (spu_extract (__mfc_tag_table, 0) != 0xF77FF000)
    abort ();
}


int
main (void)
{
  test_tag_release01 ();
  test_tag_release_invalid ();
  test_tag_group_release_invalid ();

  test_tag_reserve01 ();
  test_tag_reserve02 ();
  test_tag_reserve03 ();

  test_tag_group_reserve ();

  return 0;
}

