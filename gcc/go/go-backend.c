/* go-backend.c -- Go frontend interface to gcc backend.
   Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.

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
#include "simple-object.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "intl.h"
#include "output.h"
#include "target.h"
#include "common/common-target.h"

#include "go-c.h"

/* The segment name we pass to simple_object_start_read to find Go
   export data.  */

#ifndef GO_EXPORT_SEGMENT_NAME
#define GO_EXPORT_SEGMENT_NAME "__GNU_GO"
#endif

/* The section name we use when reading and writing export data.  */

#ifndef GO_EXPORT_SECTION_NAME
#define GO_EXPORT_SECTION_NAME ".go_export"
#endif

/* This file holds all the cases where the Go frontend needs
   information from gcc's backend.  */

/* Return the alignment in bytes of a struct field of type T.  */

unsigned int
go_field_alignment (tree t)
{
  unsigned int v;

  v = TYPE_ALIGN (t);

#ifdef BIGGEST_FIELD_ALIGNMENT
  if (v > BIGGEST_FIELD_ALIGNMENT)
    v = BIGGEST_FIELD_ALIGNMENT;
#endif

#ifdef ADJUST_FIELD_ALIGN
  {
    tree field ATTRIBUTE_UNUSED;
    field = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL, t);
    v = ADJUST_FIELD_ALIGN (field, v);
  }
#endif

  return v / BITS_PER_UNIT;
}

/* Return the size and alignment of a trampoline.  */

void
go_trampoline_info (unsigned int *size, unsigned int *alignment)
{
  *size = TRAMPOLINE_SIZE;
  *alignment = TRAMPOLINE_ALIGNMENT;
}

/* This is called by the Go frontend proper if the unsafe package was
   imported.  When that happens we can not do type-based alias
   analysis.  */

void
go_imported_unsafe (void)
{
  flag_strict_aliasing = false;

  /* This is a real hack.  init_varasm_once has already grabbed an
     alias set, which we don't want when we aren't doing strict
     aliasing.  We reinitialize to make it do it again.  This should
     be OK in practice since we haven't really done anything yet.  */
  init_varasm_once ();

  /* Let the backend know that the options have changed.  */
  targetm.override_options_after_change ();
}

/* This is called by the Go frontend proper to add data to the
   section containing Go export data.  */

void
go_write_export_data (const char *bytes, unsigned int size)
{
  static section* sec;

  if (sec == NULL)
    {
      gcc_assert (targetm_common.have_named_sections);
      sec = get_section (GO_EXPORT_SECTION_NAME, SECTION_DEBUG, NULL);
    }

  switch_to_section (sec);
  assemble_string (bytes, size);
}

/* The go_read_export_data function is called by the Go frontend
   proper to read Go export data from an object file.  FD is a file
   descriptor open for reading.  OFFSET is the offset within the file
   where the object file starts; this will be 0 except when reading an
   archive.  On success this returns NULL and sets *PBUF to a buffer
   allocated using malloc, of size *PLEN, holding the export data.  If
   the data is not found, this returns NULL and sets *PBUF to NULL and
   *PLEN to 0.  If some error occurs, this returns an error message
   and sets *PERR to an errno value or 0 if there is no relevant
   errno.  */

const char *
go_read_export_data (int fd, off_t offset, char **pbuf, size_t *plen,
		     int *perr)
{
  simple_object_read *sobj;
  const char *errmsg;
  off_t sec_offset;
  off_t sec_length;
  int found;
  char *buf;
  ssize_t c;

  *pbuf = NULL;
  *plen = 0;

  sobj = simple_object_start_read (fd, offset, GO_EXPORT_SEGMENT_NAME,
				   &errmsg, perr);
  if (sobj == NULL)
    {
      /* If we get an error here, just pretend that we didn't find any
	 export data.  This is the right thing to do if the error is
	 that the file was not recognized as an object file.  This
	 will ignore file I/O errors, but it's not too big a deal
	 because we will wind up giving some other error later.  */
      return NULL;
    }

  found = simple_object_find_section (sobj, GO_EXPORT_SECTION_NAME,
				      &sec_offset, &sec_length,
				      &errmsg, perr);
  simple_object_release_read (sobj);
  if (!found)
    return errmsg;

  if (lseek (fd, offset + sec_offset, SEEK_SET) < 0)
    {
      *perr = errno;
      return _("lseek failed while reading export data");
    }

  buf = XNEWVEC (char, sec_length);
  if (buf == NULL)
    {
      *perr = errno;
      return _("memory allocation failed while reading export data");
    }

  c = read (fd, buf, sec_length);
  if (c < 0)
    {
      *perr = errno;
      free (buf);
      return _("read failed while reading export data");
    }

  if (c < sec_length)
    {
      free (buf);
      return _("short read while reading export data");
    }

  *pbuf = buf;
  *plen = sec_length;

  return NULL;
}
