/* AIX cross support for collect2.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.

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
#include "collect2-aix.h"

#ifdef CROSS_AIX_SUPPORT

/* Read SIZE bytes starting at DATA as a big-endian value.  */

static inline bfd_vma
read_value (char *data, unsigned int size)
{
  bfd_vma value;
  unsigned int i;

  value = 0;
  for (i = 0; i < size; i++)
    {
      value <<= 8;
      value += (unsigned char) data[i];
    }
  return value;
}

/* FIELD is a char array.  Read the contents as a big-endian integer.  */
#define READ_FIELD(FIELD) \
  read_value (FIELD, sizeof (FIELD))

/* OBJECT is a char pointer to an in-file object of type struct TYPE.
   Return the address of field FIELD.  */
#define OBJECT_FIELD(OBJECT, TYPE, FIELD) \
  (OBJECT) + offsetof (struct TYPE, FIELD)

/* Return the size of FIELD, which is a field of struct TYPE.  */
#define FIELD_SIZE(TYPE, FIELD) \
  sizeof (((struct TYPE *) (0))->FIELD)

/* OBJECT is a char pointer to an in-file object of type struct TYPE.
   Read the value of field FIELD as a big-endian integer.  */
#define READ_OBJECT(OBJECT, TYPE, FIELD) \
  read_value (OBJECT_FIELD (OBJECT, TYPE, FIELD), FIELD_SIZE (TYPE, FIELD))

/* Copy FIELD from an external structure of type TYPE at address FROM
   to an internal structure pointed to by TO.  */
#define COPY_FIELD(TO, FROM, TYPE, FIELD) \
  ((TO)->FIELD = READ_OBJECT (FROM, TYPE, FIELD))

/* Return true if STRING is less than SIZE bytes long.  EXTRA_TERMINATOR
   is another character (besides '\0') that acts as a terminator,
   or '\0' if none.  */

static bool
string_within_bounds_p (const char *string, size_t size, char extra_terminator)
{
  const char *p;

  for (p = string; p < string + size; p++)
    if (*p == '\0' || *p == extra_terminator)
      return true;
  return false;
}

/* STRING is a pointer to a char array.  Try to read its value as an
   ASCII-encoded integer.  On success, return true and store the result
   in TARGET.  */
#define PARSE_INTEGER(TARGET, STRING) \
  (string_within_bounds_p (&(STRING)[0], sizeof (STRING), ' ') \
   && ((TARGET) = strtoul (STRING, NULL, 0), true))

/* Check that LDFILE's current object has SIZE bytes starting at OFFSET.  */

static inline bool
within_object_p (LDFILE *ldfile, size_t offset, size_t size)
{
  return offset <= ldfile->object_size && offset + size <= ldfile->object_size;
}

/* Try to read the file header for an XCOFF object at OFFSET bytes into
   LDFILE.  The object is expected to be OBJECT_SIZE bytes in size.
   If the object is a member of an archive, NEXT_MEMBER is the offset
   of the next member, otherwise it is -1.

   Return true on success, recording the object information in LDFILE.  */

static bool
read_xcoff_object (LDFILE *ldfile, size_t offset, size_t object_size,
		   off_t next_member)
{
  struct internal_filehdr *internal;
  char *external;
  void *map;
  size_t page_size;

  /* First try to map the file into memory.  */
  page_size = getpagesize ();
  ldfile->page_offset = offset & (page_size - 1);
  map = mmap (NULL, object_size + ldfile->page_offset, PROT_READ,
              MAP_SHARED, ldfile->fd, offset - ldfile->page_offset);
  if (map == MAP_FAILED)
    return false;

  /* Record the success.  */
  ldfile->object = (char *) map + ldfile->page_offset;
  ldfile->object_size = object_size;
  ldfile->next_member = next_member;

  /* Read the magic value to determine the type of file.  */
  if (!within_object_p (ldfile, 0, F_MAGIC_SIZE))
    return false;

  internal = &ldfile->filehdr;
  external = ldfile->object;
  internal->f_magic = read_value (external, F_MAGIC_SIZE);
  if (internal->f_magic == U802TOCMAGIC)
    {
      if (!within_object_p (ldfile, 0, sizeof (struct external_filehdr_32)))
	return false;

      COPY_FIELD (internal, external, external_filehdr_32, f_nscns);
      COPY_FIELD (internal, external, external_filehdr_32, f_timdat);
      COPY_FIELD (internal, external, external_filehdr_32, f_symptr);
      COPY_FIELD (internal, external, external_filehdr_32, f_nsyms);
      COPY_FIELD (internal, external, external_filehdr_32, f_opthdr);
      COPY_FIELD (internal, external, external_filehdr_32, f_flags);
      return true;
    }
  else if (internal->f_magic == U803XTOCMAGIC
	   || internal->f_magic == U64_TOCMAGIC)
    {
      if (!within_object_p (ldfile, 0, sizeof (struct external_filehdr_64)))
	return false;

      COPY_FIELD (internal, external, external_filehdr_64, f_nscns);
      COPY_FIELD (internal, external, external_filehdr_64, f_timdat);
      COPY_FIELD (internal, external, external_filehdr_64, f_symptr);
      COPY_FIELD (internal, external, external_filehdr_64, f_nsyms);
      COPY_FIELD (internal, external, external_filehdr_64, f_opthdr);
      COPY_FIELD (internal, external, external_filehdr_64, f_flags);
      return true;
    }
  return false;
}

/* Try to read an archive member at OFFSET bytes into LDFILE.
   Return true on success, recording the member and object
   information in LDFILE.  */

static bool
read_archive_member (LDFILE *ldfile, size_t offset)
{
  struct external_big_ar_member member;
  size_t namlen;
  size_t size;
  off_t next_member;

  if (lseek (ldfile->fd, offset, SEEK_SET) >= 0
      && read (ldfile->fd, &member, sizeof (member)) == sizeof (member)
      && PARSE_INTEGER (namlen, member.ar_namlen)
      /* Stop once we reach the member table entry, which has a name
	 of length 0.  */
      && namlen > 0
      && PARSE_INTEGER (size, member.ar_size)
      && PARSE_INTEGER (next_member, member.ar_nextoff))
    {
      /* The archive is followed by an even-padded name, then by
	 a magic string of length SXCOFFARFMAG.  The object itself
	 starts after that. */
      offset += sizeof (member) + namlen + SXCOFFARFMAG;
      offset += offset & 1;
      return read_xcoff_object (ldfile, offset, size, next_member);
    }
  return false;
}

/* Try to treat LDFILE as a non-empty big archive.  Return true
   on success, storing the member and object information for
   the first member in LDFILE.  */

static bool
read_big_archive (LDFILE *ldfile)
{
  struct external_big_ar_filehdr filehdr;
  size_t offset;

  return (lseek (ldfile->fd, 0L, SEEK_SET) == 0
	  && read (ldfile->fd, &filehdr, sizeof (filehdr)) == sizeof (filehdr)
	  && memcmp (filehdr.fl_magic, FL_MAGIC_BIG_AR, FL_MAGIC_SIZE) == 0
	  && PARSE_INTEGER (offset, filehdr.fl_firstmemoff)
	  && read_archive_member (ldfile, offset));
}

/* LDFILE is a zero-initialized structure.  Try to open FILENAME,
   returning true on success.  */

static bool
open_file (LDFILE *ldfile, const char *filename)
{
  struct stat st;

  ldfile->fd = open (filename, O_RDONLY);
  if (ldfile->fd < 0)
    return false;

  if (read_big_archive (ldfile))
    return true;

  if (fstat (ldfile->fd, &st) < 0)
    return false;

  return read_xcoff_object (ldfile, 0, st.st_size, -1);
}

/* Release the memory associated with the current object, if one has
   been mapped.  */

static void
free_object (LDFILE *ldfile)
{
  if (ldfile->object)
    munmap (ldfile->object - ldfile->page_offset,
	    ldfile->object_size + ldfile->page_offset);
}

/* Free LDFILE and all resources associated with it.  */

static void
free_ldfile (LDFILE *ldfile)
{
  if (ldfile->fd >= 0)
    close (ldfile->fd);
  XDELETE (ldfile);
}

/* Implement the API-defined ldopen function.  */

LDFILE *
ldopen (char *filename, LDFILE *ldfile)
{
  if (ldfile == NULL)
    {
      ldfile = XCNEW (LDFILE);
      if (!open_file (ldfile, filename))
	{
	  free_object (ldfile);
	  free_ldfile (ldfile);
	  return NULL;
	}
    }
  return ldfile;
}

/* Implement the API-defined ldtbread function.  */

int
ldtbread (LDFILE *ldfile, long index, SYMENT *internal)
{
  size_t offset, name_length;
  char *external;

  /* Make sure that the symbol index is valid.  */
  if (index < 0 || index >= HEADER (ldfile).f_nsyms)
    return FAILURE;

  /* Work out the offset of the symbol table entry.  */
  offset = HEADER (ldfile).f_symptr + index * sizeof (struct external_syment);
  if (!within_object_p (ldfile, offset, sizeof (struct external_syment)))
    return FAILURE;

  /* Read all the fields.  The format differs between 32-bit and
     64-bit files.  */
  external = ldfile->object + offset;
  if (HEADER (ldfile).f_magic == U802TOCMAGIC)
    {
      /* Copy the n_zeroes/n_offset interpretation.  */
      internal->n_zeroes = READ_OBJECT (external, external_syment,
					u.xcoff32.u.u.n_zeroes);
      internal->n_offset = READ_OBJECT (external, external_syment,
					u.xcoff32.u.u.n_offset);

      /* Copy the n_name interpretation.  The internal version has room
	 for a null terminator.  */
      name_length = FIELD_SIZE (external_syment, u.xcoff32.u.n_name);
      memcpy (internal->n_name,
	      external + offsetof (struct external_syment, u.xcoff32.u.n_name),
	      name_length);
      internal->n_name[name_length] = 0;

      internal->n_value = READ_OBJECT (external, external_syment,
				       u.xcoff32.n_value);
    }
  else
    {
      internal->n_zeroes = 0;
      internal->n_offset = READ_OBJECT (external, external_syment,
					u.xcoff64.n_offset);
      internal->n_value = READ_OBJECT (external, external_syment,
				       u.xcoff64.n_value);
    }
  COPY_FIELD (internal, external, external_syment, n_scnum);
  COPY_FIELD (internal, external, external_syment, n_type);
  COPY_FIELD (internal, external, external_syment, n_sclass);
  COPY_FIELD (internal, external, external_syment, n_numaux);
  return SUCCESS;
}

/* Implement the API-defined ldgetname function.  */

char *
ldgetname (LDFILE *ldfile, SYMENT *symbol)
{
  char *name;
  size_t offset;

  /* If the zeroes field is nonzero, the name is in the symbol table
     entry itself.  */
  if (symbol->n_zeroes != 0)
    return symbol->n_name;

  /* Otherwise, the symbol table entry contains an offset into the
     string table, which starts after the end of the symbol table.  */
  offset = (HEADER (ldfile).f_symptr
	    + HEADER (ldfile).f_nsyms * sizeof (struct external_syment)
	    + symbol->n_offset);
  if (offset >= ldfile->object_size)
    return NULL;

  /* Make sure that the name is entirely contained within the object.  */
  name = ldfile->object + offset;
  if (!string_within_bounds_p (name, ldfile->object_size - offset, '\0'))
    return NULL;

  return name;
}

/* Implement the API-defined ldclose function.  */

int
ldclose (LDFILE *ldfile)
{
  free_object (ldfile);
  if (ldfile->next_member >= 0
      && read_archive_member (ldfile, ldfile->next_member))
    return FAILURE;

  free_ldfile (ldfile);
  return SUCCESS;
}

#endif
