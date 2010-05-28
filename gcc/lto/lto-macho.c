/* LTO routines for Mach-O object files.
   Copyright 2010 Free Software Foundation, Inc.
   Contributed by Steven Bosscher.

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
#include "toplev.h"
#include "lto.h"
#include "tm.h"
#include "libiberty.h"
#include "lto-streamer.h"
#include "lto/lto-endian.h"
#include "lto/lto-macho.h"

/* Rather than implementing a libmacho to match libelf, or attempting to
   integrate libbfd into GCC, this file is a self-contained (and very
   minimal) Mach-O format object file reader/writer.  The generated files
   will contain a Mach-O header, a number of Mach-O load commands an
   section headers, the  section data itself, and a trailing string table
   for section names.  */

/* This needs to be kept in sync with darwin.c.  Better yet, lto-macho.c
   and lto-macho.h should be moved to config/, and likewise for lto-coff.*
   and lto-elf.*.  */

/* Segment name for LTO sections.  */
#define LTO_SEGMENT_NAME "__GNU_LTO"

/* Section name for LTO section names section.  */
#define LTO_NAMES_SECTION "__section_names"

/* Handle opening elf files on hosts, such as Windows, that may use 
   text file handling that will break binary access.  */
#ifndef O_BINARY
# define O_BINARY 0
#endif

/* Cached object file header.  We use a header_64 for this, since all
   the fields we need are in there, in the same position as header_32.  */
mach_o_header_64 cached_mach_o_header;
uint32_t cached_mach_o_magic;

/* The current output file.  */
static lto_file *current_out_file;


/* Is this a 32-bits or 64-bits Mach-O object file?  */
static int
mach_o_word_size (void)
{
  gcc_assert (cached_mach_o_magic != 0);
  return (cached_mach_o_magic == MACH_O_MH_MAGIC_64
	  || cached_mach_o_magic == MACH_O_MH_CIGAM_64) ? 64 : 32;
}

/* Sets the current output file to FILE.  Returns the old output file or
   NULL.  */

lto_file *
lto_set_current_out_file (lto_file *file)
{
  lto_file *old_file = current_out_file;
  current_out_file = file;
  return old_file;
}


/* Returns the current output file.  */

lto_file *
lto_get_current_out_file (void)
{
  return current_out_file;
}

/* Mach-O section structure constructor.  */

static lto_mach_o_section
mach_o_new_section (lto_mach_o_file *mach_o_file, const char *name)
{
  lto_mach_o_section ptr;

  /* FIXME We could allocate these things on an obstack.  */
  ptr = XCNEW (struct lto_mach_o_section_d);
  if (name)
    {
      if (strncmp (name, LTO_SECTION_NAME_PREFIX,
		   strlen(LTO_SECTION_NAME_PREFIX)) != 0)
	sorry ("not implemented: Mach-O writer for non-LTO sections");
      ptr->name = xstrdup (name);
    }

  VEC_safe_push (lto_mach_o_section, heap, mach_o_file->section_vec, ptr);

  return ptr;
}

/* Mach-O section data block structure constructor.  */

static lto_mach_o_data
mach_o_new_data (lto_mach_o_section sec)
{
  lto_mach_o_data ptr, *chain_ptr_ptr;

  /* FIXME We could allocate these things on an obstack.  */
  ptr = XCNEW (struct lto_mach_o_data_d);

  chain_ptr_ptr = &sec->data_chain;
  while (*chain_ptr_ptr)
    chain_ptr_ptr = &(*chain_ptr_ptr)->next;
  *chain_ptr_ptr = ptr;

  return ptr;
}

/* Initialize FILE, an LTO file object for FILENAME.  Offset is the
   offset into FILE where the object is located (e.g. in an archive).  */

static void
lto_file_init (lto_file *file, const char *filename, off_t offset)
{
  file->filename = filename;
  file->offset = offset;
}

/* Returns a hash code for P.  */

static hashval_t
hash_name (const void *p)
{
  const struct lto_section_slot *s = (const struct lto_section_slot *) p;
  return (hashval_t) htab_hash_string (s->name);
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_name (const void *p1, const void *p2)
{
  const struct lto_section_slot *s1 =
    (const struct lto_section_slot *) p1;
  const struct lto_section_slot *s2 =
    (const struct lto_section_slot *) p2;

  return strcmp (s1->name, s2->name) == 0;
}

/* Build a hash table whose key is the section names and whose data is
   the start and size of each section in the .o file.  */

htab_t
lto_obj_build_section_table (lto_file *lto_file) 
{
  lto_mach_o_file *mach_o_file = (lto_mach_o_file *)lto_file;
  lto_mach_o_section sec;
  htab_t section_hash_table;
  off_t strtab_offs;
  ssize_t strtab_size;
  char *strtab = NULL;
  int i;

  section_hash_table = htab_create (37, hash_name, eq_name, free);

  /* Seek the string table.  */
  /* FIXME The segment name should be in darwin.h, but can we include it
     here in this file?  */
  for (i = 0;
       VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    {
      if (strncmp (sec->u.section.segname, "__GNU_LTO", 16) != 0)
	continue;
      if (strncmp (sec->u.section.sectname, "__section_names", 16) == 0)
        break;
    }
  if (! sec)
    {
      error ("invalid Mach-O LTO object file: no __section_names section found");
      goto done;
    }
  mach_o_file->section_names_section = sec;

  if (mach_o_word_size () == 64)
    {
      strtab_offs = (off_t) get_uint32 (&sec->u.section_64.offset[0]);
      strtab_size = (size_t) get_uint64 (&sec->u.section_64.size[0]);
    }
  else
    {
      strtab_offs = (off_t) get_uint32 (&sec->u.section_32.offset[0]);
      strtab_size = (size_t) get_uint32 (&sec->u.section_32.size[0]);
    }

  /* Seek to start of string table.  */
  if (strtab_offs != lseek (mach_o_file->fd,
			    mach_o_file->base.offset + strtab_offs,
			    SEEK_SET))
    {
      error ("altered or invalid Mach-O object file");
      goto done;
    }

  strtab = XNEWVEC (char, strtab_size);
  if (read (mach_o_file->fd, strtab, strtab_size) != strtab_size)
    {
      error ("invalid Mach-O LTO object file __section_names section");
      goto done;
    }

  /* Scan sections looking at names.  */
  for (i = 0;
       VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    {
      struct lto_section_slot s_slot;
      void **slot;
      char *new_name;
      unsigned long stringoffset;
      char name[17];

      /* Ignore non-LTO sections.  Also ignore the __section_names section
	 which does not need renaming.  */
      if (strncmp (sec->u.section.segname, "__GNU_LTO", 16) != 0)
	continue;
      if (sec == mach_o_file->section_names_section)
        continue;

      /* Try to extract the offset of the real name for this section from
	 __section_names.  */
      memcpy (&name[0], sec->u.section.sectname, 16);
      name[16] = '\0';
      if (name[0] != '_' || name[1] != '_'
	  || sscanf (&name[2], "%08lX", &stringoffset) != 1
	  || strtab_size < (ssize_t) stringoffset)
	{
	  error ("invalid Mach-O LTO section name string: %s", name);
	  continue;
	}

      new_name = XNEWVEC (char, strlen (strtab + stringoffset) + 1);
      strcpy (new_name, strtab + stringoffset);
      s_slot.name = new_name;
      slot = htab_find_slot (section_hash_table, &s_slot, INSERT);
      if (*slot == NULL)
	{
	  struct lto_section_slot *new_slot = XNEW (struct lto_section_slot);

	  new_slot->name = new_name;
	  if (mach_o_word_size() == 64)
	    {
	      new_slot->start =
		(intptr_t) get_uint32 (&sec->u.section_64.offset[0]);
	      new_slot->len =
		(size_t) get_uint64 (&sec->u.section_64.size[0]);
	    }
	  else
	    {
	      new_slot->start =
		(intptr_t) get_uint32 (&sec->u.section_32.offset[0]);
	      new_slot->len =
		(size_t) get_uint32 (&sec->u.section_32.size[0]);
	    }

	  *slot = new_slot;
	}
      else
	{
	  error ("two or more sections for %s:", new_name);
	  goto done;
	}
    }

 done:
  if (strtab)
    free (strtab);
  return section_hash_table;
}


/* Begin a new Mach-O section named NAME in the current output file.  */

void
lto_obj_begin_section (const char *name)
{
  lto_mach_o_file *file;

  if (strncmp (name, LTO_SECTION_NAME_PREFIX,
	       strlen(LTO_SECTION_NAME_PREFIX)) != 0)
    sorry ("not implemented: Mach-O writer for non-LTO sections");

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_mach_o_file *) lto_get_current_out_file (),
  gcc_assert (file && file->writable && !file->scn);

  /* Create a new section.  */
  file->scn = mach_o_new_section (file, name);
  if (!file->scn)
    fatal_error ("could not create a new Mach-O section: %m");
}


/* Append DATA of length LEN to the current output section.  BASE is a pointer
   to the output page containing DATA.  It is freed once the output file has
   been written.  */

void
lto_obj_append_data (const void *data, size_t len, void *block)
{
  lto_mach_o_file *file;
  lto_mach_o_data mach_o_data;
  struct lto_char_ptr_base *base = (struct lto_char_ptr_base *) block;

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_mach_o_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  mach_o_data = mach_o_new_data (file->scn);
  if (!mach_o_data)
    fatal_error ("could not append data to Mach-O section: %m");

  mach_o_data->d_buf = CONST_CAST (void *, data);
  mach_o_data->d_size = len;

  /* Chain all data blocks (from all sections) on one singly-linked
     list for freeing en masse after the file is closed.  */
  base->ptr = (char *)file->data;
  file->data = base;
}


/* End the current output section.  This just does some assertion checking
   and sets the current output file's scn member to NULL.  */

void
lto_obj_end_section (void)
{
  lto_mach_o_file *file;

  /* Grab the current output file and validate some basic assertions.  */
  file = (lto_mach_o_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  file->scn = NULL;
}


/* Read a Mach-O header from MACH_O_FILE and validate it.
   The file descriptor in MACH_O_FILE points at the start of the file.
   If cached_mach_o_header is uninitialized, caches the results.
   On succes, returns true and moves file pointer to the start of the
   load commands.  On failure, returns false.  */

static bool
validate_mach_o_header (lto_mach_o_file *mach_o_file)
{
  ssize_t i, n;
  unsigned char magic[4];
  uint32_t cputype;
  off_t startpos;

  /* Known header magics for validation, as an array.  */
  static const unsigned int mach_o_known_formats[] = {
    MACH_O_MH_MAGIC,
    MACH_O_MH_CIGAM,
    MACH_O_MH_MAGIC_64,
    MACH_O_MH_CIGAM_64,
  };
#define MACH_O_NUM_KNOWN_FORMATS \
  ((ssize_t) ARRAY_SIZE (mach_o_known_formats))

  startpos = lseek (mach_o_file->fd, 0, SEEK_CUR);
  if (read (mach_o_file->fd, &magic, sizeof (magic)) != 4
      || lseek (mach_o_file->fd, -4, SEEK_CUR) != startpos)
    {
      error ("cannot read file %s", mach_o_file->base.filename);
      return false;
    }

  for (i = 0; i < MACH_O_NUM_KNOWN_FORMATS; ++i)
    if (get_uint32 (&magic[0]) == mach_o_known_formats[i])
      break;
  if (i == MACH_O_NUM_KNOWN_FORMATS)
    goto not_for_target;

  /* Check the endian-ness.  */
  if (BYTES_BIG_ENDIAN && magic[0] != 0xfe)
    goto not_for_target;

  /* Set or check cached magic number.  */
  if (cached_mach_o_magic == 0)
    cached_mach_o_magic = get_uint32 (&magic[0]);
  else if (cached_mach_o_magic != get_uint32 (&magic[0]))
    goto not_for_target;
 
  n = mach_o_word_size () == 64
      ? sizeof (mach_o_header_64) : sizeof (mach_o_header_32);
  if (read (mach_o_file->fd, &mach_o_file->u.header, n) != n)
    goto not_for_target;

  /* Is this a supported CPU?  */
  /* ??? Would be nice to validate the exact target architecture.  */
  cputype = get_uint32 (&mach_o_file->u.header.cputype[0]);
  if (cputype == MACH_O_CPU_TYPE_I386
      || cputype == MACH_O_CPU_TYPE_POWERPC)
    {
      if (mach_o_word_size () != 32)
        goto not_for_target;
    }
  else if (cputype == MACH_O_CPU_TYPE_X86_64
	   || cputype == MACH_O_CPU_TYPE_POWERPC_64)
    {
      if (mach_o_word_size () != 64)
        goto not_for_target;
    }

  /* Is this an MH_OBJECT file?  */
  if (get_uint32 (&mach_o_file->u.header.filetype[0]) != MACH_O_MH_OBJECT)
    error ("Mach-O file %s is not an MH_OBJECT file",
	   mach_o_file->base.filename);

  /* Save the header for future use.  */
  memcpy (&cached_mach_o_header, &mach_o_file->u.header,
	  sizeof (cached_mach_o_header));

  return true;

 not_for_target:
  error ("file %s is not a Mach-O object file for target",
	 mach_o_file->base.filename);
  return false;
}


/* Read a Mach-O LC_SEGMENT command (32 bits) from MACH_O_FILE and
   validate it.
   The file descriptor in MACH_O_FILE points at the start of the load
   command.  On sucess, returns true and advances the file pointer
   past the end of the load command.  On failure, returns false.  */

static bool
validate_mach_o_segment_command_32 (lto_mach_o_file *mach_o_file)
{
  mach_o_segment_command_32 seg_cmd_32;
  unsigned int i;
  ssize_t n;
  off_t startpos;

  /* Fields we're interested in.  */
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t nsects;

  startpos = lseek (mach_o_file->fd, 0, SEEK_CUR);

  n = sizeof (mach_o_segment_command_32);
  if (read (mach_o_file->fd, (void *) &seg_cmd_32, n) != n)
    goto fail;

  cmd = get_uint32 (&seg_cmd_32.cmd[0]);
  cmdsize = get_uint32 (&seg_cmd_32.cmdsize[0]);
  nsects = get_uint32 (&seg_cmd_32.nsects[0]);
  gcc_assert (cmd == MACH_O_LC_SEGMENT);

  /* Validate section table entries.  */
  for (i = 0; i < nsects; i++)
    {
      mach_o_section_32 sec_32;
      lto_mach_o_section ltosec;

      n = sizeof (mach_o_section_32);
      if (read (mach_o_file->fd, &sec_32, n) != n)
	goto fail;

      /* ??? Perform some checks.  */

      /* Looks ok, so record its details.  We don't read the 
         string table or set up names yet; we'll do that when
	 we build the hash table.  */
      ltosec = mach_o_new_section (mach_o_file, NULL);
      memcpy (&ltosec->u.section_32, &sec_32, sizeof (sec_32));
    }

  if (lseek (mach_o_file->fd, 0, SEEK_CUR) != startpos + cmdsize)
    goto fail;

  return true;

 fail:
  error ("could not read LC_SEGMENT command in Mach-O file %s",
	 mach_o_file->base.filename);
  return false;
}


/* Read a Mach-O LC_SEGMENT_64 command from MACH_O_FILE and validate it.
   The file descriptor in MACH_O_FILE points at the start of the load
   command.  On sucess, returns true and advances the file pointer
   past the end of the load command.  On failure, returns false.  */

static bool
validate_mach_o_segment_command_64 (lto_mach_o_file *mach_o_file)
{
  mach_o_segment_command_64 seg_cmd_64;
  unsigned int i;
  ssize_t n;
  off_t startpos;

  /* Fields we're interested in.  */
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t nsects;

  startpos = lseek (mach_o_file->fd, 0, SEEK_CUR);

  n = sizeof (mach_o_segment_command_64);
  if (read (mach_o_file->fd, (void *) &seg_cmd_64, n) != n)
    goto fail;

  cmd = get_uint32 (&seg_cmd_64.cmd[0]);
  cmdsize = get_uint32 (&seg_cmd_64.cmdsize[0]);
  nsects = get_uint32 (&seg_cmd_64.nsects[0]);
  gcc_assert (cmd == MACH_O_LC_SEGMENT_64);

  /* Validate section table entries.  */
  for (i = 0; i < nsects; i++)
    {
      mach_o_section_64 sec_64;
      lto_mach_o_section ltosec;

      n = sizeof (mach_o_section_64);
      if (read (mach_o_file->fd, &sec_64, n) != n)
	goto fail;

      /* ??? Perform some checks.  */

      /* Looks ok, so record its details.  We don't read the 
         string table or set up names yet; we'll do that when
	 we build the hash table.  */
      ltosec = mach_o_new_section (mach_o_file, NULL);
      memcpy (&ltosec->u.section_64, &sec_64, sizeof (sec_64));
    }

  if (lseek (mach_o_file->fd, 0, SEEK_CUR) != startpos + cmdsize)
    goto fail;

  return true;

 fail:
  error ("could not read LC_SEGMENT_64 command in Mach-O file %s",
	 mach_o_file->base.filename);
  return false;
}

/* Read a Mach-O load commands from MACH_O_FILE and validate it.
   The file descriptor in MACH_O_FILE points at the start of the load
   command.  On sucess, returns true and advances the file pointer
   past the end of the load command.  On failure, returns false.  */

static bool
validate_mach_o_load_command (lto_mach_o_file *mach_o_file)
{
  mach_o_load_command load_command;
  uint32_t cmd;
  uint32_t cmdsize;
  ssize_t n;

  n = sizeof (load_command);
  if (read (mach_o_file->fd, &load_command, n) != n)
    {
      error ("could not read load commands in Mach-O file %s",
	     mach_o_file->base.filename);
      return false;
    }
  lseek (mach_o_file->fd, -1 * (off_t) sizeof (load_command), SEEK_CUR);

  cmd = get_uint32 (&load_command.cmd[0]);
  cmdsize = get_uint32 (&load_command.cmdsize[0]);
  switch (cmd)
    {
    case MACH_O_LC_SEGMENT:
      return validate_mach_o_segment_command_32 (mach_o_file);
    case MACH_O_LC_SEGMENT_64:
      return validate_mach_o_segment_command_64 (mach_o_file);

    default:
      /* Just skip over it.  */
      lseek (mach_o_file->fd, cmdsize, SEEK_CUR);
      return true;
    }
}

/* Validate's MACH_O_FILE's executable header and, if cached_mach_o_header is
   uninitialized, caches the results.  Also records the section header string
   table's section index.  Returns true on success, false on failure.  */

static bool
validate_file (lto_mach_o_file *mach_o_file)
{
  uint32_t i, ncmds;

  /* Read and sanity check the raw header.  */
  if (! validate_mach_o_header (mach_o_file))
    return false;

  ncmds = get_uint32 (&mach_o_file->u.header.ncmds[0]);
  for (i = 0; i < ncmds; ++i)
    if (! validate_mach_o_load_command (mach_o_file))
      return false;

  return true;
}

/* Initialize MACH_O_FILE's executable header using cached data from previously
   read files.  */

static void
init_mach_o_header (lto_mach_o_file *mach_o_file)
{
  gcc_assert (cached_mach_o_magic != 0);
  memcpy (&mach_o_file->u.header,
	  &cached_mach_o_header,
	  sizeof (mach_o_file->u.header));
  put_uint32 (&mach_o_file->u.header.ncmds[0], 0);
  put_uint32 (&mach_o_file->u.header.sizeofcmds[0], 0);
}

/* Open Mach-O file FILENAME.  If WRITABLE is true, the file is opened for write
   and, if necessary, created.  Otherwise, the file is opened for reading.
   Returns the opened file.  */

lto_file *
lto_obj_file_open (const char *filename, bool writable)
{
  lto_mach_o_file *mach_o_file;
  lto_file *result = NULL;
  off_t offset;
  const char *offset_p;
  char *fname;
  struct stat statbuf;

  offset_p = strchr (filename, '@');
  if (!offset_p)
    {
      fname = xstrdup (filename);
      offset = 0;
    }
  else
    {
      /* The file started with '@' is a file containing command line
	 options.  Stop if it doesn't exist.  */
      if (offset_p == filename)
	fatal_error ("command line option file '%s' does not exist",
		     filename);

      fname = (char *) xmalloc (offset_p - filename + 1);
      memcpy (fname, filename, offset_p - filename);
      fname[offset_p - filename] = '\0';
      offset_p += 3; /* skip the @0x */
      offset = lto_parse_hex (offset_p);
    }

  /* Set up.  */
  mach_o_file = XCNEW (lto_mach_o_file);
  result = (lto_file *) mach_o_file;
  lto_file_init (result, fname, offset);
  mach_o_file->fd = -1;
  mach_o_file->writable = writable;

  /* Open the file.  */
  mach_o_file->fd = open (fname,
    O_BINARY | (writable ? O_WRONLY | O_CREAT | O_TRUNC : O_RDONLY), 0666);

  if (mach_o_file->fd == -1)
    {
      error ("could not open file %s", fname);
      goto fail;
    }

  if (stat (fname, &statbuf) < 0)
    {
      error ("could not stat file %s", fname);
      goto fail;
    }

  mach_o_file->file_size = statbuf.st_size;

  /* If the object is in an archive, get it out.  */
  if (offset != 0)
    {
      char ar_tail[12];
      int size;

      /* Surely not?  */
      gcc_assert (!writable);

      /* Seek to offset, or error.  */
      if (lseek (mach_o_file->fd, offset, SEEK_SET) != (ssize_t) offset)
	{
	  error ("could not find archive member @0x%lx", (long) offset);
	  goto fail;
	}

      /* Now seek back 12 chars and read the tail of the AR header to
         find the length of the member file.  */
      if (lseek (mach_o_file->fd, -12, SEEK_CUR) < 0
	  || read (mach_o_file->fd, ar_tail, 12) != 12
	  || lseek (mach_o_file->fd, 0, SEEK_CUR) != (ssize_t) offset
	  || ar_tail[10] != '`' || ar_tail[11] != '\n')
	{
	  error ("could not find archive header @0x%lx", (long) offset);
	  goto fail;
	}

      ar_tail[11] = 0;
      if (sscanf (ar_tail, "%d", &size) != 1)
	{
	  error ("invalid archive header @0x%lx", (long) offset);
	  goto fail;
	}
      mach_o_file->file_size = size;
    }

  if (writable)
    {
      init_mach_o_header (mach_o_file);
    }
  else
    if (! validate_file (mach_o_file))
      goto fail;

  return result;

 fail:
  if (result)
    lto_obj_file_close (result);
  return NULL;
}


/* Write the data in MACH_O_FILE to a real Mach-O binary object.
   We write a header, a segment load command, and section data.  */

static bool
mach_o_write_object_file (lto_mach_o_file *mach_o_file)
{
  lto_mach_o_section sec, snsec;
  lto_mach_o_data snsec_data;
  ssize_t hdrsize, cmdsize, secsize;
  size_t num_sections, snsec_size, total_sec_size;
  unsigned int sec_offs, strtab_offs;
  int i;
  bool write_err = false;

  /* The number of sections we will write is the number of sections added by
     the streamer, plus 1 for the section names section.  */
  num_sections = VEC_length (lto_mach_o_section, mach_o_file->section_vec) + 1;

  /* Calculate the size of the basic data structures on disk.  */
  if (mach_o_word_size () == 64)
    {
      hdrsize = sizeof (mach_o_header_64);
      secsize = sizeof (mach_o_section_64);
      cmdsize = sizeof (mach_o_segment_command_64) + num_sections * secsize;
    }
  else
    {
      hdrsize = sizeof (mach_o_header_32);
      secsize = sizeof (mach_o_section_32);
      cmdsize = sizeof (mach_o_segment_command_32) + num_sections * secsize;
    }
 
  /* Allocate the section names section.  */
  snsec_size = 0;
  for (i = 0;
       VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    snsec_size += strlen (sec->name) + 1;
  snsec = mach_o_new_section (mach_o_file, NULL);
  snsec->name = LTO_NAMES_SECTION;
  snsec_data = mach_o_new_data (snsec);
  snsec_data->d_buf = XCNEWVEC (char, snsec_size);
  snsec_data->d_size = snsec_size;

  /* Position all the sections, and fill out their headers.  */
  sec_offs = hdrsize + cmdsize;
  strtab_offs = 0;
  total_sec_size = 0;
  for (i = 0;
       VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    {
      lto_mach_o_data data;
      size_t data_size;
      /* Put the section and segment names.  Add the section name to the
         section names section (unless, of course, this *is* the section
	 names section).  */
      if (sec == snsec)
	snprintf (sec->u.section.sectname, 16, "%s", LTO_NAMES_SECTION);
      else
	{
	  sprintf (sec->u.section.sectname, "__%08X", strtab_offs);
	  memcpy ((char *) snsec_data->d_buf + strtab_offs, sec->name, strlen (sec->name));
	}
      memcpy (&sec->u.section.segname[0],
	      LTO_SEGMENT_NAME, strlen (LTO_SEGMENT_NAME));

      /* Add layout and attributes.  */
      for (data = sec->data_chain, data_size = 0; data; data = data->next)
	data_size += data->d_size;
      if (mach_o_word_size () == 64)
	{
	  put_uint64 (&sec->u.section_64.addr[0], total_sec_size); 
	  put_uint64 (&sec->u.section_64.size[0], data_size); 
	  put_uint32 (&sec->u.section_64.offset[0], sec_offs); 
	  put_uint32 (&sec->u.section_64.flags[0], MACH_O_S_ATTR_DEBUG);
	}
      else
	{
	  put_uint32 (&sec->u.section_64.addr[0], total_sec_size); 
	  put_uint32 (&sec->u.section_32.size[0], data_size); 
	  put_uint32 (&sec->u.section_32.offset[0], sec_offs); 
	  put_uint32 (&sec->u.section_32.flags[0], MACH_O_S_ATTR_DEBUG);
	}

      sec_offs += data_size;
      total_sec_size += data_size;
      strtab_offs += strlen (sec->name) + 1;
    }

  /* We can write the data now.  As there's no way to indicate an error return
     from this hook, error handling is limited to not wasting our time doing
     any more writes in the event that any one fails.  */

  /* Write the header.  */
  put_uint32 (&mach_o_file->u.header.ncmds[0], 1);
  put_uint32 (&mach_o_file->u.header.sizeofcmds[0], cmdsize);
  write_err = (write (mach_o_file->fd,
		      &mach_o_file->u.header, hdrsize) != hdrsize);
  /* Write the segment load command.  */
  if (mach_o_word_size () == 64)
    {
      mach_o_segment_command_64 lc;
      ssize_t lc_size = sizeof (lc);
      memset (&lc, 0, lc_size);
      put_uint32 (&lc.cmd[0], MACH_O_LC_SEGMENT_64);
      put_uint32 (&lc.cmdsize[0], cmdsize);
      put_uint64 (&lc.fileoff[0], hdrsize + cmdsize);
      put_uint64 (&lc.filesize[0], total_sec_size);
      put_uint32 (&lc.nsects[0], num_sections);
      write_err = (write (mach_o_file->fd, &lc, lc_size) != lc_size);
    }
  else
    {
      mach_o_segment_command_32 lc;
      ssize_t lc_size = sizeof (lc);
      memset (&lc, 0, lc_size);
      put_uint32 (&lc.cmd[0], MACH_O_LC_SEGMENT);
      put_uint32 (&lc.cmdsize[0], cmdsize);
      put_uint32 (&lc.fileoff[0], hdrsize + cmdsize);
      put_uint32 (&lc.filesize[0], total_sec_size);
      put_uint32 (&lc.nsects[0], num_sections);
      write_err = (write (mach_o_file->fd, &lc, lc_size) != lc_size);
    }
  for (i = 0;
       !write_err
       && VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    write_err = (write (mach_o_file->fd,
			&sec->u.section, secsize) != secsize);

  gcc_assert (lseek (mach_o_file->fd, 0, SEEK_CUR) == hdrsize + cmdsize);

  /* Write the section data.  */
  for (i = 0;
       !write_err
       && VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    {
      lto_mach_o_data data;

      for (data = sec->data_chain; data; data = data->next)
	{
	  if (!write_err)
	    write_err = (write (mach_o_file->fd, data->d_buf, data->d_size)
			 != data->d_size);
	  else
	    break;
	}
    }

  return !write_err;
}

/* Close Mach-O file FILE and clean up any associated data structures.  If FILE
   was opened for writing, the file's Mach-O data is written at this time.  Any
   cached data buffers are freed.  */

void
lto_obj_file_close (lto_file *file)
{
  lto_mach_o_file *mach_o_file = (lto_mach_o_file *) file;
  struct lto_char_ptr_base *cur, *tmp;
  lto_mach_o_section sec;
  bool write_err = false;
  int i;

  /* If this file is open for writing, write a Mach-O object file.  */
  if (mach_o_file->writable)
    {
      if (! mach_o_write_object_file (mach_o_file))
        fatal_error ("cannot write Mach-O object file");
    }

  /* Close the file, we're done.  */
  if (mach_o_file->fd != -1)
    close (mach_o_file->fd);

  /* Free any data buffers.  */
  cur = mach_o_file->data;
  while (cur)
    {
      tmp = cur;
      cur = (struct lto_char_ptr_base *) cur->ptr;
      free (tmp);
    }

  /* Free any sections and their data chains.  */
  for (i = 0;
       VEC_iterate (lto_mach_o_section, mach_o_file->section_vec, i, sec);
       i++)
    {
      lto_mach_o_data curdata, nextdata;
      curdata = sec->data_chain;
      while (curdata)
	{
	  nextdata = curdata->next;
	  free (curdata);
	  curdata = nextdata;
	}
      free (sec);
    }
  VEC_free (lto_mach_o_section, heap, mach_o_file->section_vec);

  free (file);

  /* If there was an error, mention it.  */
  if (write_err)
    error ("I/O error writing Mach-O output file");
}

