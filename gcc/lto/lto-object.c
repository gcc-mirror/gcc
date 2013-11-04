/* LTO routines to use object files.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Google.

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
#include "gimple.h"
#include "diagnostic-core.h"
#include "lto.h"
#include "tm.h"
#include "lto-streamer.h"
#include "simple-object.h"

/* Segment name for LTO sections.  This is only used for Mach-O.
   FIXME: This needs to be kept in sync with darwin.c.  */

#define LTO_SEGMENT_NAME "__GNU_LTO"

/* An LTO file wrapped around an simple_object.  */

struct lto_simple_object
{
  /* The base information.  */
  lto_file base;

  /* The system file descriptor.  */
  int fd;

  /* The simple_object if we are reading the file.  */
  simple_object_read *sobj_r;

  /* The simple_object if we are writing the file.  */
  simple_object_write *sobj_w;

  /* The currently active section.  */
  simple_object_write_section *section;
};

/* Saved simple_object attributes.  FIXME: Once set, this is never
   cleared.  */

static simple_object_attributes *saved_attributes;

/* Initialize FILE, an LTO file object for FILENAME.  */

static void
lto_file_init (lto_file *file, const char *filename, off_t offset)
{
  file->filename = filename;
  file->offset = offset;
}

/* Open the file FILENAME.  It WRITABLE is true, the file is opened
   for write and, if necessary, created.  Otherwise, the file is
   opened for reading.  Returns the opened file.  */

lto_file *
lto_obj_file_open (const char *filename, bool writable)
{
  const char *offset_p;
  long loffset;
  int consumed;
  char *fname;
  off_t offset;
  struct lto_simple_object *lo;
  const char *errmsg;
  int err;

  offset_p = strrchr (filename, '@');
  if (offset_p != NULL
      && offset_p != filename
      && sscanf (offset_p, "@%li%n", &loffset, &consumed) >= 1
      && strlen (offset_p) == (unsigned int) consumed)
    {
      fname = XNEWVEC (char, offset_p - filename + 1);
      memcpy (fname, filename, offset_p - filename);
      fname[offset_p - filename] = '\0';
      offset = (off_t) loffset;
    }
  else
    {
      fname = xstrdup (filename);
      offset = 0;
    }

  lo = XCNEW (struct lto_simple_object);
  lto_file_init ((lto_file *) lo, fname, offset);

  lo->fd = open (fname,
		 (writable
		  ? O_WRONLY | O_CREAT | O_BINARY
		  : O_RDONLY | O_BINARY),
		 0666);
  if (lo->fd == -1)
    {
      error ("open %s failed: %s", fname, xstrerror (errno));
      goto fail;
    }

  if (!writable)
    {
      simple_object_attributes *attrs;

      lo->sobj_r = simple_object_start_read (lo->fd, offset, LTO_SEGMENT_NAME,
					     &errmsg, &err);
      if (lo->sobj_r == NULL)
	goto fail_errmsg;

      attrs = simple_object_fetch_attributes (lo->sobj_r, &errmsg, &err);
      if (attrs == NULL)
	goto fail_errmsg;

      if (saved_attributes == NULL)
	saved_attributes = attrs;
      else
	{
	  errmsg = simple_object_attributes_merge (saved_attributes, attrs,
						   &err);
	  if (errmsg != NULL)
	    {
	      free (attrs);
	      goto fail_errmsg;
	    }
	}
    }
  else
    {
      gcc_assert (saved_attributes != NULL);
      lo->sobj_w = simple_object_start_write (saved_attributes,
					      LTO_SEGMENT_NAME,
					      &errmsg, &err);
      if (lo->sobj_w == NULL)
	goto fail_errmsg;
    }

  return &lo->base;

 fail_errmsg:
  if (err == 0)
    error ("%s: %s", fname, errmsg);
  else
    error ("%s: %s: %s", fname, errmsg, xstrerror (err));
					 
 fail:
  if (lo->fd != -1)
    lto_obj_file_close ((lto_file *) lo);
  free (lo);
  return NULL;
}


/* Close FILE.  If FILE was opened for writing, it is written out
   now.  */

void
lto_obj_file_close (lto_file *file)
{
  struct lto_simple_object *lo = (struct lto_simple_object *) file;

  if (lo->sobj_r != NULL)
    simple_object_release_read (lo->sobj_r);
  else if (lo->sobj_w != NULL)
    {
      const char *errmsg;
      int err;

      gcc_assert (lo->base.offset == 0);

      errmsg = simple_object_write_to_file (lo->sobj_w, lo->fd, &err);
      if (errmsg != NULL)
	{
	  if (err == 0)
	    fatal_error ("%s", errmsg);
	  else
	    fatal_error ("%s: %s", errmsg, xstrerror (err));
	}

      simple_object_release_write (lo->sobj_w);
    }

  if (lo->fd != -1)
    {
      if (close (lo->fd) < 0)
	fatal_error ("close: %s", xstrerror (errno));
    }
}

/* This is passed to lto_obj_add_section.  */

struct lto_obj_add_section_data
{
  /* The hash table of sections.  */
  htab_t section_hash_table;
  /* The offset of this file.  */
  off_t base_offset;
  /* List in linker order */
  struct lto_section_list *list;
};

/* This is called for each section in the file.  */

static int
lto_obj_add_section (void *data, const char *name, off_t offset,
		     off_t length)
{
  struct lto_obj_add_section_data *loasd =
    (struct lto_obj_add_section_data *) data;
  htab_t section_hash_table = (htab_t) loasd->section_hash_table;
  char *new_name;
  struct lto_section_slot s_slot;
  void **slot;
  struct lto_section_list *list = loasd->list;

  if (strncmp (name, LTO_SECTION_NAME_PREFIX,
	       strlen (LTO_SECTION_NAME_PREFIX)) != 0)
    return 1;

  new_name = xstrdup (name);
  s_slot.name = new_name;
  slot = htab_find_slot (section_hash_table, &s_slot, INSERT);
  if (*slot == NULL)
    {
      struct lto_section_slot *new_slot = XCNEW (struct lto_section_slot);

      new_slot->name = new_name;
      new_slot->start = loasd->base_offset + offset;
      new_slot->len = length;
      *slot = new_slot;

      if (list != NULL)
        {
          if (!list->first)
            list->first = new_slot;
          if (list->last)
            list->last->next = new_slot;
          list->last = new_slot;
        }
    }
  else
    {
      error ("two or more sections for %s", new_name);
      return 0;
    }

  return 1;
}

/* Build a hash table whose key is the section name and whose data is
   the start and size of each section in the .o file.  */

htab_t
lto_obj_build_section_table (lto_file *lto_file, struct lto_section_list *list)
{
  struct lto_simple_object *lo = (struct lto_simple_object *) lto_file;
  htab_t section_hash_table;
  struct lto_obj_add_section_data loasd;
  const char *errmsg;
  int err;

  section_hash_table = lto_obj_create_section_hash_table ();

  gcc_assert (lo->sobj_r != NULL && lo->sobj_w == NULL);
  loasd.section_hash_table = section_hash_table;
  loasd.base_offset = lo->base.offset;
  loasd.list = list;
  errmsg = simple_object_find_sections (lo->sobj_r, lto_obj_add_section,
					&loasd, &err);
  if (errmsg != NULL)
    {
      if (err == 0)
	error ("%s", errmsg);
      else
	error ("%s: %s", errmsg, xstrerror (err));
      htab_delete (section_hash_table);
      return NULL;
    }

  return section_hash_table;
}

/* The current output file.  */

static lto_file *current_out_file;

/* Set the current output file.  Return the old one.  */

lto_file *
lto_set_current_out_file (lto_file *file)
{
  lto_file *old_file;

  old_file = current_out_file;
  current_out_file = file;
  return old_file;
}

/* Return the current output file.  */

lto_file *
lto_get_current_out_file (void)
{
  return current_out_file;
}

/* Begin writing a new section named NAME in the current output
   file.  */

void
lto_obj_begin_section (const char *name)
{
  struct lto_simple_object *lo;
  int align;
  const char *errmsg;
  int err;

  lo = (struct lto_simple_object *) current_out_file;
  gcc_assert (lo != NULL
	      && lo->sobj_r == NULL
	      && lo->sobj_w != NULL
	      && lo->section == NULL);

  align = exact_log2 (POINTER_SIZE / BITS_PER_UNIT);
  lo->section = simple_object_write_create_section (lo->sobj_w, name, align,
						    &errmsg, &err);
  if (lo->section == NULL)
    {
      if (err == 0)
	fatal_error ("%s", errmsg);
      else
	fatal_error ("%s: %s", errmsg, xstrerror (errno));
    }
}

/* Add data to a section.  BLOCK is a pointer to memory containing
   DATA.  */

void
lto_obj_append_data (const void *data, size_t len, void *block)
{
  struct lto_simple_object *lo;
  const char *errmsg;
  int err;

  lo = (struct lto_simple_object *) current_out_file;
  gcc_assert (lo != NULL && lo->section != NULL);

  errmsg = simple_object_write_add_data (lo->sobj_w, lo->section, data, len,
					 1, &err);
  if (errmsg != NULL)
    {
      if (err == 0)
	fatal_error ("%s", errmsg);
      else
	fatal_error ("%s: %s", errmsg, xstrerror (errno));
    }

  free (block);
}

/* Stop writing to the current output section.  */

void
lto_obj_end_section (void)
{
  struct lto_simple_object *lo;

  lo = (struct lto_simple_object *) current_out_file;
  gcc_assert (lo != NULL && lo->section != NULL);
  lo->section = NULL;
}
