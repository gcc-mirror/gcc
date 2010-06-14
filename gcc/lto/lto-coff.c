/* LTO routines for COFF object files.
   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Dave Korn.

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
#include "ggc.h"
#include "lto-streamer.h"
#include "lto/lto-coff.h"


/* Rather than implementing a libcoff to match libelf, or attempting to
   integrate libbfd into GCC, this file is a self-contained (and very
   minimal) COFF format object file reader/writer.  The generated files
   will contain a COFF header, a number of COFF section headers, the 
   section data itself, and a trailing string table for section names.  */

/* Handle opening elf files on hosts, such as Windows, that may use 
   text file handling that will break binary access.  */

#ifndef O_BINARY
#define O_BINARY 0
#endif

/* Known header magics for validation, as an array.  */

static const unsigned int coff_machine_array[] = COFF_KNOWN_MACHINES;

/* Number of valid entries (no sentinel) in array.  */

#define NUM_COFF_KNOWN_MACHINES	\
	(sizeof (coff_machine_array) / sizeof (coff_machine_array[0]))

/* Cached object file header.  */

static Coff_header cached_coff_hdr;

/* Flag to indicate if we have read and cached any header yet.  */

static bool cached_coff_hdr_valid = false;

/* The current output file.  */

static lto_file *current_out_file;


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


/* COFF section structure constructor.  */

static lto_coff_section *
coff_newsection (lto_coff_file *file, const char *name, size_t type)
{
  lto_coff_section *ptr, **chain_ptr_ptr;

  ptr = XCNEW (lto_coff_section);
  ptr->name = name;
  ptr->type = type;

  chain_ptr_ptr = &file->section_chain;
  while (*chain_ptr_ptr)
    chain_ptr_ptr = &(*chain_ptr_ptr)->next;
  *chain_ptr_ptr = ptr;

  return ptr;
}


/* COFF section data block structure constructor.  */

static lto_coff_data *
coff_newdata (lto_coff_section *sec)
{
  lto_coff_data *ptr, **chain_ptr_ptr;

  ptr = XCNEW (lto_coff_data);

  chain_ptr_ptr = &sec->data_chain;
  while (*chain_ptr_ptr)
    chain_ptr_ptr = &(*chain_ptr_ptr)->next;
  *chain_ptr_ptr = ptr;

  return ptr;
}


/* Initialize FILE, an LTO file object for FILENAME.  */

static void
lto_file_init (lto_file *file, const char *filename, off_t offset)
{
  file->filename = filename;
  file->offset = offset;
}

/* Return an error string after an error, or a predetermined one
   if ERRCODE is not -1.  */

static const char *
coff_errmsg (int errcode)
{
  return strerror (errcode == -1 ? errno : errcode);
}

/* Returns a hash code for P.  */

static hashval_t
hash_name (const void *p)
{
  const struct lto_section_slot *ds = (const struct lto_section_slot *) p;
  return (hashval_t) htab_hash_string (ds->name);
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
  lto_coff_file *coff_file = (lto_coff_file *)lto_file;
  lto_coff_section *sec;
  htab_t section_hash_table;
  ssize_t strtab_size;
  char *strtab;

  section_hash_table = htab_create (37, hash_name, eq_name, free);

  /* Seek to start of string table.  */
  if (coff_file->strtab_offs != lseek (coff_file->fd,
		coff_file->base.offset + coff_file->strtab_offs, SEEK_SET))
    {
      error ("altered or invalid COFF object file");
      return section_hash_table;
    }

  strtab_size = coff_file->file_size - coff_file->strtab_offs;
  strtab = XNEWVEC (char, strtab_size);
  if (read (coff_file->fd, strtab, strtab_size) != strtab_size)
    {
      error ("invalid COFF object file string table");
      return section_hash_table;
    }

  /* Scan sections looking at names.  */
  COFF_FOR_ALL_SECTIONS(coff_file, sec)
    {
      struct lto_section_slot s_slot;
      void **slot;
      char *new_name;
      int stringoffset;
      char *name = (char *) &sec->coffsec.Name[0];

      /* Skip dummy string section if by any chance we see it.  */
      if (sec->type == 1)
	continue;

      if (name[0] == '/')
	{
	  if (1 != sscanf (&name[1], "%d", &stringoffset)
		|| stringoffset < 0 || stringoffset >= strtab_size)
	    {
	      error ("invalid COFF section name string");
	      continue;
	    }
	  name = strtab + stringoffset;
	}
      else
	{
	  /* If we cared about the VirtualSize field, we couldn't
	     crudely trash it like this to guarantee nul-termination
	     of the Name field.  But we don't, so we do.  */
	  name[8] = 0;
	}
      if (strncmp (name, LTO_SECTION_NAME_PREFIX,
			strlen (LTO_SECTION_NAME_PREFIX)) != 0)
	  continue;

      new_name = XNEWVEC (char, strlen (name) + 1);
      strcpy (new_name, name);
      s_slot.name = new_name;
      slot = htab_find_slot (section_hash_table, &s_slot, INSERT);
      if (*slot == NULL)
	{
	  struct lto_section_slot *new_slot = XNEW (struct lto_section_slot);

	  new_slot->name = new_name;
	  /* The offset into the file for this section.  */
	  new_slot->start = coff_file->base.offset
			+ COFF_GET(&sec->coffsec,PointerToRawData);
	  new_slot->len = COFF_GET(&sec->coffsec,SizeOfRawData);
	  *slot = new_slot;
	}
      else
	{
	  error ("two or more sections for %s:", new_name);
	  return NULL;
	}
    }

  free (strtab);
  return section_hash_table;
}


/* Begin a new COFF section named NAME with type TYPE in the current output
   file.  TYPE is an SHT_* macro from the libelf headers.  */

static void
lto_coff_begin_section_with_type (const char *name, size_t type)
{
  lto_coff_file *file;
  size_t sh_name;

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_coff_file *) lto_get_current_out_file (),
  gcc_assert (file);
  gcc_assert (!file->scn);

  /* Create a new section.  */
  file->scn = coff_newsection (file, name, type);
  if (!file->scn)
    fatal_error ("could not create a new COFF section: %s", coff_errmsg (-1));

  /* Add a string table entry and record the offset.  */
  gcc_assert (file->shstrtab_stream);
  sh_name = file->shstrtab_stream->total_size;
  lto_output_data_stream (file->shstrtab_stream, name, strlen (name) + 1);

  /* Initialize the section header.  */
  file->scn->strtab_offs = sh_name;
}


/* Begin a new COFF section named NAME in the current output file.  */

void
lto_obj_begin_section (const char *name)
{
  lto_coff_begin_section_with_type (name, 0);
}


/* Append DATA of length LEN to the current output section.  BASE is a pointer
   to the output page containing DATA.  It is freed once the output file has
   been written.  */

void
lto_obj_append_data (const void *data, size_t len, void *block)
{
  lto_coff_file *file;
  lto_coff_data *coff_data;
  struct lto_char_ptr_base *base = (struct lto_char_ptr_base *) block;

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_coff_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  coff_data = coff_newdata (file->scn);
  if (!coff_data)
    fatal_error ("could not append data to COFF section: %s", coff_errmsg (-1));

  coff_data->d_buf = CONST_CAST (void *, data);
  coff_data->d_size = len;

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
  lto_coff_file *file;

  /* Grab the current output file and validate some basic assertions.  */
  file = (lto_coff_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  file->scn = NULL;
}


/* Validate's COFF_FILE's executable header and, if cached_coff_hdr is
   uninitialized, caches the results.  Also records the section header string
   table's section index.  Returns true on success or false on failure.  */

static bool
validate_file (lto_coff_file *coff_file)
{
  size_t n, secnum;
  unsigned int numsections, secheaderssize, numsyms;
  off_t sectionsstart, symbolsstart, stringsstart;
  unsigned int mach, charact;

  /* Read and sanity check the raw header.  */
  n = read (coff_file->fd, &coff_file->coffhdr, sizeof (coff_file->coffhdr));
  if (n != sizeof (coff_file->coffhdr))
    {
      error ("not a COFF object file");
      return false;
    }

  mach = COFF_GET(&coff_file->coffhdr, Machine);
  for (n = 0; n < NUM_COFF_KNOWN_MACHINES; n++)
    if (mach == coff_machine_array[n])
      break;
  if (n == NUM_COFF_KNOWN_MACHINES)
    {
      error ("not a recognized COFF object file");
      return false;
    }

  charact = COFF_GET(&coff_file->coffhdr, Characteristics);
  if (COFF_NOT_CHARACTERISTICS & charact)
    {
      /* DLL, EXE or SYS file.  */
      error ("not a relocatable COFF object file");
      return false;
    }

  if (COFF_CHARACTERISTICS != (COFF_CHARACTERISTICS & charact))
    {
      /* ECOFF/XCOFF/PE+ support not implemented.  */
      error ("not a 32-bit COFF object file");
      return false;
    }

  /* It validated OK, so cached it if we don't already have one.  */
  if (!cached_coff_hdr_valid)
    {
      cached_coff_hdr_valid = true;
      memcpy (&cached_coff_hdr, &coff_file->coffhdr, sizeof (cached_coff_hdr));
    }

  if (mach != COFF_GET(&cached_coff_hdr, Machine))
    {
      error ("inconsistent file architecture detected");
      return false;
    }

  /* Read section headers and string table? */

  numsections = COFF_GET(&coff_file->coffhdr, NumberOfSections);
  secheaderssize = numsections * sizeof (Coff_section);
  sectionsstart = sizeof (Coff_header) + secheaderssize;
  symbolsstart = COFF_GET(&coff_file->coffhdr, PointerToSymbolTable);
  numsyms = COFF_GET(&coff_file->coffhdr, NumberOfSymbols);
  stringsstart = (symbolsstart + COFF_SYMBOL_SIZE * numsyms);

#define CVOFFSETTTED(x) (coff_file->base.offset + (x))

  if (numsections <= 0 || symbolsstart <= 0 || numsyms <= 0
	|| (CVOFFSETTTED(sectionsstart) >= coff_file->file_size)
	|| (CVOFFSETTTED(symbolsstart) >= coff_file->file_size)
	|| (CVOFFSETTTED(stringsstart) >= coff_file->file_size))
    {
      error ("not a valid COFF object file");
      return false;
    }

#undef CVOFFSETTTED

  /* Record start of string table.  */
  coff_file->strtab_offs = stringsstart;

  /* Validate section table entries.  */
  for (secnum = 0; secnum < numsections; secnum++)
    {
      Coff_section coffsec;
      lto_coff_section *ltosec;
      off_t size_raw, offs_raw, offs_relocs, offs_lines;
      off_t num_relocs, num_lines;

      n = read (coff_file->fd, &coffsec, sizeof (coffsec));
      if (n != sizeof (coffsec))
	{
	  error ("short/missing COFF section table");
	  return false;
	}

      size_raw = COFF_GET(&coffsec, SizeOfRawData);
      offs_raw = COFF_GET(&coffsec, PointerToRawData);
      offs_relocs = COFF_GET(&coffsec, PointerToRelocations);
      offs_lines = COFF_GET(&coffsec, PointerToLinenumbers);
      num_relocs = COFF_GET(&coffsec, NumberOfRelocations);
      num_lines = COFF_GET(&coffsec, NumberOfLinenumbers);

      if (size_raw < 0 || num_relocs < 0 || num_lines < 0
	|| (size_raw
	  && ((COFF_GET(&coffsec, Characteristics)
	      & IMAGE_SCN_CNT_UNINITIALIZED_DATA)
	    ? (offs_raw != 0)
	    : (offs_raw < sectionsstart || offs_raw >= coff_file->file_size)))
	|| (num_relocs
	  && (offs_relocs < sectionsstart
	    || offs_relocs >= coff_file->file_size))
	|| (num_lines
	  && (offs_lines < sectionsstart
	    || offs_lines >= coff_file->file_size)))
	{
	  error ("invalid COFF section table");
	  return false;
	}

      /* Looks ok, so record its details.  We don't read the 
         string table or set up names yet; we'll do that when
	 we build the hash table.  */
      ltosec = coff_newsection (coff_file, NULL, 0);
      memcpy (&ltosec->coffsec, &coffsec, sizeof (ltosec->coffsec));
    }

  return true;
}

/* Initialize COFF_FILE's executable header using cached data from previously
   read files.  */

static void
init_coffhdr (lto_coff_file *coff_file)
{
  gcc_assert (cached_coff_hdr_valid);
  memset (&coff_file->coffhdr, 0, sizeof (coff_file->coffhdr));
  COFF_PUT(&coff_file->coffhdr, Machine, COFF_GET(&cached_coff_hdr, Machine));
  COFF_PUT(&coff_file->coffhdr, Characteristics, COFF_GET(&cached_coff_hdr, Characteristics));
}

/* Open COFF file FILENAME.  If WRITABLE is true, the file is opened for write
   and, if necessary, created.  Otherwise, the file is opened for reading.
   Returns the opened file.  */

lto_file *
lto_obj_file_open (const char *filename, bool writable)
{
  lto_coff_file *coff_file;
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
  coff_file = XCNEW (lto_coff_file);
  result = (lto_file *) coff_file;
  lto_file_init (result, fname, offset);
  coff_file->fd = -1;

  /* Open the file.  */
  coff_file->fd = open (fname,
    O_BINARY | (writable ? O_WRONLY | O_CREAT | O_TRUNC : O_RDONLY), 0666);

  if (coff_file->fd == -1)
    {
      error ("could not open file %s", fname);
      goto fail;
    }

  if (stat (fname, &statbuf) < 0)
    {
      error ("could not stat file %s", fname);
      goto fail;
    }

  coff_file->file_size = statbuf.st_size;

  if (offset != 0)
    {
      char ar_tail[12];
      int size;

      /* Surely not?  */
      gcc_assert (!writable);

      /* Seek to offset, or error.  */
      if (lseek (coff_file->fd, offset, SEEK_SET) != (ssize_t) offset)
	{
	  error ("could not find archive member @0x%lx", (long) offset);
	  goto fail;
	}

      /* Now seek back 12 chars and read the tail of the AR header to
         find the length of the member file.  */
      if (lseek (coff_file->fd, -12, SEEK_CUR) < 0
	  || read (coff_file->fd, ar_tail, 12) != 12
	  || lseek (coff_file->fd, 0, SEEK_CUR) != (ssize_t) offset
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
      coff_file->file_size = size;
    }

  if (writable)
    {
      init_coffhdr (coff_file);
      coff_file->shstrtab_stream = XCNEW (struct lto_output_stream);
    }
  else
    if (!validate_file (coff_file))
      goto fail;

  return result;

 fail:
  if (result)
    lto_obj_file_close (result);
  return NULL;
}


/* Close COFF file FILE and clean up any associated data structures.  If FILE
   was opened for writing, the file's COFF data is written at this time, and
   any cached data buffers are freed.  Return TRUE if there was an error.  */

static bool
coff_write_object_file (lto_coff_file *coff_file)
{
  lto_coff_section *cursec, *stringsec;
  lto_coff_data *data;
  size_t fileoffset, numsections, totalsecsize, numsyms, stringssize;
  bool write_err = false;
  int secnum;

  /* Infer whether this file was opened for reading or writing from the
     presence or absense of an initialised stream for the string table;
     do nothing if it was opened for reading.  */
  if (!coff_file->shstrtab_stream)
    return false;
  else
    {
      /* Write the COFF string table into a dummy new section that
	 we will not write a header for.  */
      lto_file *old_file = lto_set_current_out_file (&coff_file->base);
      /* This recursively feeds in the data to a new section.  */
      lto_coff_begin_section_with_type (".strtab", 1);
      lto_write_stream (coff_file->shstrtab_stream);
      lto_obj_end_section ();
      lto_set_current_out_file (old_file);
      free (coff_file->shstrtab_stream);
    }

  /* Layout the file.  Count sections (not dummy string section) and calculate
     data size for all of them.  */
  numsections = 0;
  totalsecsize = 0;
  stringssize = 0;
  stringsec = NULL;
  COFF_FOR_ALL_SECTIONS(coff_file, cursec)
    {
      lto_coff_data *data;
      size_t cursecsize;
      cursecsize = 0;
      COFF_FOR_ALL_DATA(cursec,data)
	cursecsize += data->d_size;
      if (cursec->type == 0)
	{
	  ++numsections;
	  totalsecsize += COFF_ALIGN(cursecsize);
#if COFF_ALIGNMENT > 1
	  cursec->pad_needed = COFF_ALIGN(cursecsize) - cursecsize;
#endif
	}
      else
        {
	  stringssize = cursecsize;
	  stringsec = cursec;
	}
      COFF_PUT(&cursec->coffsec, SizeOfRawData, cursecsize);
    }

  /* There is a file symbol and a section symbol per section,
     and each of these has a single auxiliary symbol following.  */
  numsyms = 2 * (1 + numsections);

  /* Great!  Now we have enough info to fill out the file header.  */
  COFF_PUT(&coff_file->coffhdr, NumberOfSections, numsections);
  COFF_PUT(&coff_file->coffhdr, NumberOfSymbols, numsyms);
  COFF_PUT(&coff_file->coffhdr, PointerToSymbolTable, sizeof (Coff_header)
		+ numsections * sizeof (Coff_section) + totalsecsize);
  /* The remaining members were initialised to zero or copied from
     a cached header, so we leave them alone here.  */

  /* Now position all the sections, and fill out their headers.  */
  fileoffset = sizeof (Coff_header) + numsections * sizeof (Coff_section);
  COFF_FOR_ALL_SECTIONS(coff_file, cursec)
    {
      /* Skip dummy string section.  */
      if (cursec->type == 1)
	continue;
      COFF_PUT(&cursec->coffsec, PointerToRawData, fileoffset);
      fileoffset += COFF_ALIGN (COFF_GET(&cursec->coffsec, SizeOfRawData));
      COFF_PUT(&cursec->coffsec, Characteristics, COFF_SECTION_CHARACTERISTICS);
      snprintf ((char *)&cursec->coffsec.Name[0], 8, "/%d", cursec->strtab_offs + 4);
    }

  /* We can write the data now.  As there's no way to indicate an error return
     from this hook, error handling is limited to not wasting our time doing
     any more writes in the event that any one fails.  */

  /* Write the COFF header.  */
  write_err = (write (coff_file->fd, &coff_file->coffhdr,
		sizeof (coff_file->coffhdr)) != sizeof (coff_file->coffhdr));

  /* Write the COFF section headers.  */
  COFF_FOR_ALL_SECTIONS(coff_file, cursec)
    if (cursec->type == 1)	/* Skip dummy string section.  */
	continue;
    else if (!write_err)
      write_err = (write (coff_file->fd, &cursec->coffsec,
		sizeof (cursec->coffsec)) != sizeof (cursec->coffsec));
    else
      break;

  /* Write the COFF sections.  */
  COFF_FOR_ALL_SECTIONS(coff_file, cursec)
    {
#if COFF_ALIGNMENT > 1
      static const char padzeros[COFF_ALIGNMENT] = { 0 };
#endif
      /* Skip dummy string section.  */
      if (cursec->type == 1)
	continue;
      COFF_FOR_ALL_DATA(cursec, data)
	if (!write_err)
	  write_err = (write (coff_file->fd, data->d_buf, data->d_size)
		!= data->d_size);
	else
	  break;
#if COFF_ALIGNMENT > 1
      if (!write_err && cursec->pad_needed)
	write_err = (write (coff_file->fd, padzeros, cursec->pad_needed)
		!= cursec->pad_needed);
#endif
    }

  /* Write the COFF symbol table.  */
  if (!write_err)
    {
      union
	{
	  Coff_symbol sym;
	  Coff_aux_sym_file file;
	  Coff_aux_sym_section sec;
	} symbols[2];
      memset (&symbols[0], 0, sizeof (symbols));
      strcpy ((char *) &symbols[0].sym.Name[0], ".file");
      COFF_PUT(&symbols[0].sym, SectionNumber, IMAGE_SYM_DEBUG);
      COFF_PUT(&symbols[0].sym, Type, IMAGE_SYM_TYPE);
      symbols[0].sym.StorageClass[0] = IMAGE_SYM_CLASS_FILE;
      symbols[0].sym.NumberOfAuxSymbols[0] = 1;
      snprintf ((char *)symbols[1].file.FileName,
		sizeof (symbols[1].file.FileName),
		"%s", lbasename (coff_file->base.filename));
      write_err = (write (coff_file->fd, &symbols[0], sizeof (symbols))
		!= (2 * COFF_SYMBOL_SIZE));

      /* Set up constant parts for section sym loop.  */
      memset (&symbols[0], 0, sizeof (symbols));
      COFF_PUT(&symbols[0].sym, Type, IMAGE_SYM_TYPE);
      symbols[0].sym.StorageClass[0] = IMAGE_SYM_CLASS_STATIC;
      symbols[0].sym.NumberOfAuxSymbols[0] = 1;

      secnum = 1;
      if (!write_err)
	COFF_FOR_ALL_SECTIONS(coff_file, cursec)
	  {
	    /* Skip dummy string section.  */
	    if (cursec->type == 1)
	      continue;
	    /* Reuse section name string for section symbol name.  */
	    COFF_PUT_NDXSZ(&symbols[0].sym, Name, 0, 0, 4);
	    COFF_PUT_NDXSZ(&symbols[0].sym, Name, cursec->strtab_offs + 4, 4, 4);
	    COFF_PUT(&symbols[0].sym, SectionNumber, secnum++);
	    COFF_PUT(&symbols[1].sec, Length,
			COFF_GET(&cursec->coffsec, SizeOfRawData));
	    if (!write_err)
	      write_err = (write (coff_file->fd, &symbols[0], sizeof (symbols))
			!= (2 * COFF_SYMBOL_SIZE));
	    else
	      break;
	  }
    }

  /* Write the COFF string table.  */
  if (!write_err)
    {
      unsigned char outlen[4];
      COFF_PUT4(outlen, stringssize + 4);
      if (!write_err)
	write_err = (write (coff_file->fd, outlen, 4) != 4);
      if (stringsec)
	COFF_FOR_ALL_DATA(stringsec, data)
	  if (!write_err)
	write_err = (write (coff_file->fd, data->d_buf, data->d_size)
			!= data->d_size);
	else
	  break;
    }

  return write_err;
}

/* Close COFF file FILE and clean up any associated data structures.  If FILE
   was opened for writing, the file's COFF data is written at this time, and
   any cached data buffers are freed.  */

void
lto_obj_file_close (lto_file *file)
{
  lto_coff_file *coff_file = (lto_coff_file *) file;
  struct lto_char_ptr_base *cur, *tmp;
  lto_coff_section *cursec, *nextsec;
  bool write_err = false;

  /* Write the COFF string table into a dummy new section that
     we will not write a header for.  */
  if (coff_file->shstrtab_stream)
    coff_write_object_file (coff_file);

  /* Close the file, we're done.  */
  if (coff_file->fd != -1)
    close (coff_file->fd);

  /* Free any data buffers.  */
  cur = coff_file->data;
  while (cur)
    {
      tmp = cur;
      cur = (struct lto_char_ptr_base *) cur->ptr;
      free (tmp);
    }

  /* Free any sections and their data chains.  */
  cursec = coff_file->section_chain;
  while (cursec)
    {
      lto_coff_data *curdata, *nextdata;
      nextsec = cursec->next;
      curdata = cursec->data_chain;
      while (curdata)
	{
	  nextdata = curdata->next;
	  free (curdata);
	  curdata = nextdata;
	}
      free (cursec);
      cursec = nextsec;
    }

  free (file);

  /* If there was an error, mention it.  */
  if (write_err)
    error ("I/O error writing COFF output file");
}

