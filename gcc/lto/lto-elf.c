/* LTO routines for ELF object files.
   Copyright 2009 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include <gelf.h>
#include "lto.h"
#include "tm.h"
#include "libiberty.h"
#include "ggc.h"
#include "lto-streamer.h"

/* Handle opening elf files on hosts, such as Windows, that may use 
   text file handling that will break binary access.  */

#ifndef O_BINARY
# define O_BINARY 0
#endif


/* Initialize FILE, an LTO file object for FILENAME.  */
static void
lto_file_init (lto_file *file, const char *filename, off_t offset)
{
  file->filename = filename;
  file->offset = offset;
}

/* An ELF file.  */
struct lto_elf_file 
{
  /* The base information.  */
  lto_file base;

  /* The system file descriptor for the file.  */
  int fd;

  /* The libelf descriptor for the file.  */
  Elf *elf;

  /* Section number of string table used for section names.  */
  size_t sec_strtab;

  /* Writable file members.  */

  /* The currently active section.  */
  Elf_Scn *scn;

  /* The output stream for section header names.  */
  struct lto_output_stream *shstrtab_stream;

  /* Linked list of data which must be freed *after* the file has been
     closed.  This is an annoying limitation of libelf.  */
  struct lto_char_ptr_base *data;
};
typedef struct lto_elf_file lto_elf_file;

/* Stores executable header attributes which must be shared by all ELF files.
   This is used for validating input files and populating output files.  */
static struct {
  bool initialized;
  /* 32 or 64 bits?  */
  size_t bits;
  unsigned char elf_ident[EI_NIDENT];
  Elf64_Half elf_machine;
} cached_file_attrs;


/* Return the section header for SECTION.  The return value is never
   NULL.  Call lto_elf_free_shdr to release the memory allocated.  */

static Elf64_Shdr *
lto_elf_get_shdr (Elf_Scn *section)
{
  Elf64_Shdr *shdr;

  switch (cached_file_attrs.bits)
    {
    case 32:
      {
	Elf32_Shdr *shdr32;

	/* Read the 32-bit section header.  */
	shdr32 = elf32_getshdr (section);
	if (!shdr32)
	  fatal_error ("could not read section header: %s", elf_errmsg (0));

	/* Transform it into a 64-bit section header.  */
	shdr = XNEW (Elf64_Shdr);
	shdr->sh_name = shdr32->sh_name;
	shdr->sh_type = shdr32->sh_type;
	shdr->sh_flags = shdr32->sh_flags;
	shdr->sh_addr = shdr32->sh_addr;
	shdr->sh_offset = shdr32->sh_offset;
	shdr->sh_size = shdr32->sh_size;
	shdr->sh_link = shdr32->sh_link;
	shdr->sh_info = shdr32->sh_info;
	shdr->sh_addralign = shdr32->sh_addralign;
	shdr->sh_entsize  = shdr32->sh_entsize;
	break;
      }
      break;

    case 64:
      shdr = elf64_getshdr (section);
      if (!shdr)
	fatal_error ("could not read section header: %s", elf_errmsg (0));
      break;

    default:
      gcc_unreachable ();
    }

  return shdr;
}

/* Free SHDR, previously allocated by lto_elf_get_shdr.  */
static void
lto_elf_free_shdr (Elf64_Shdr *shdr)
{
  if (cached_file_attrs.bits != 64)
    free (shdr);
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
lto_elf_build_section_table (lto_file *lto_file) 
{
  lto_elf_file *elf_file = (lto_elf_file *)lto_file;
  htab_t section_hash_table;
  Elf_Scn *section;
  size_t base_offset;

  section_hash_table = htab_create (37, hash_name, eq_name, free);

  base_offset = elf_getbase (elf_file->elf);
  for (section = elf_getscn (elf_file->elf, 0);
       section;
       section = elf_nextscn (elf_file->elf, section)) 
    {
      Elf64_Shdr *shdr;
      const char *name;
      size_t offset;
      char *new_name;
      void **slot;
      struct lto_section_slot s_slot;

      /* Get the name of this section.  */
      shdr = lto_elf_get_shdr (section);
      offset = shdr->sh_name;
      name = elf_strptr (elf_file->elf, 
			 elf_file->sec_strtab,
			 offset);

      /* Only put lto stuff into the symtab.  */
      if (strncmp (name, LTO_SECTION_NAME_PREFIX, 
		   strlen (LTO_SECTION_NAME_PREFIX)) != 0)
	{
	  lto_elf_free_shdr (shdr);
	  continue;
	}

      new_name = XNEWVEC (char, strlen (name) + 1);
      strcpy (new_name, name);
      s_slot.name = new_name;
      slot = htab_find_slot (section_hash_table, &s_slot, INSERT);
      if (*slot == NULL)
	{
	  struct lto_section_slot *new_slot = XNEW (struct lto_section_slot);

	  new_slot->name = new_name;
	  /* The offset into the file for this section.  */
	  new_slot->start = base_offset + shdr->sh_offset;
	  new_slot->len = shdr->sh_size;
	  *slot = new_slot;
	}
      else
	{
	  error ("two or more sections for %s:", new_name);
	  return NULL;
	}

      lto_elf_free_shdr (shdr);
    }

  return section_hash_table;
}


/* Initialize the section header of section SCN.  SH_NAME is the section name
   as an index into the section header string table.  SH_TYPE is the section
   type, an SHT_* macro from libelf headers.  */

#define DEFINE_INIT_SHDR(BITS)					      \
static void							      \
init_shdr##BITS (Elf_Scn *scn, size_t sh_name, size_t sh_type)	      \
{								      \
  Elf##BITS##_Shdr *shdr;					      \
								      \
  shdr = elf##BITS##_getshdr (scn);				      \
  if (!shdr)							      \
    {								      \
      if (BITS == 32)						      \
	fatal_error ("elf32_getshdr() failed: %s", elf_errmsg (-1));  \
      else							      \
	fatal_error ("elf64_getshdr() failed: %s", elf_errmsg (-1));  \
    }								      \
								      \
  shdr->sh_name = sh_name;					      \
  shdr->sh_type = sh_type;					      \
  shdr->sh_addralign = POINTER_SIZE / BITS_PER_UNIT;		      \
  shdr->sh_flags = 0;						      \
  shdr->sh_entsize = 0;						      \
}

DEFINE_INIT_SHDR (32)
DEFINE_INIT_SHDR (64)

static bool first_data_block;

/* Begin a new ELF section named NAME with type TYPE in the current output
   file.  TYPE is an SHT_* macro from the libelf headers.  */

static void
lto_elf_begin_section_with_type (const char *name, size_t type)
{
  lto_elf_file *file;
  Elf_Scn *scn;
  size_t sh_name;

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_elf_file *) lto_get_current_out_file (),
  gcc_assert (file);
  gcc_assert (file->elf);
  gcc_assert (!file->scn);

  /* Create a new section.  */
  scn = elf_newscn (file->elf);
  if (!scn)
    fatal_error ("could not create a new ELF section: %s", elf_errmsg (-1));
  file->scn = scn;

  /* Add a string table entry and record the offset.  */
  gcc_assert (file->shstrtab_stream);
  sh_name = file->shstrtab_stream->total_size;
  lto_output_data_stream (file->shstrtab_stream, name, strlen (name) + 1);

  /* Initialize the section header.  */
  switch (cached_file_attrs.bits)
    {
    case 32:
      init_shdr32 (scn, sh_name, type);
      break;

    case 64:
      init_shdr64 (scn, sh_name, type);
      break;

    default:
      gcc_unreachable ();
    }

  first_data_block = true;
}


/* Begin a new ELF section named NAME in the current output file.  */

void
lto_elf_begin_section (const char *name)
{
  lto_elf_begin_section_with_type (name, SHT_PROGBITS);
}


/* Append DATA of length LEN to the current output section.  BASE is a pointer
   to the output page containing DATA.  It is freed once the output file has
   been written.  */

void
lto_elf_append_data (const void *data, size_t len, void *block)
{
  lto_elf_file *file;
  Elf_Data *elf_data;
  struct lto_char_ptr_base *base = (struct lto_char_ptr_base *) block;

  /* Grab the current output file and do some basic assertion checking.  */
  file = (lto_elf_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  elf_data = elf_newdata (file->scn);
  if (!elf_data)
    fatal_error ("could not append data to ELF section: %s", elf_errmsg (-1));

  if (first_data_block)
    {
      elf_data->d_align = POINTER_SIZE / BITS_PER_UNIT;
      first_data_block = false;
    }
  else
    elf_data->d_align = 1;
  elf_data->d_buf = CONST_CAST (void *, data);
  elf_data->d_off = 0LL;
  elf_data->d_size = len;
  elf_data->d_type = ELF_T_BYTE;
  elf_data->d_version = EV_CURRENT;

  base->ptr = (char *)file->data;
  file->data = base;
}


/* End the current output section.  This just does some assertion checking
   and sets the current output file's scn member to NULL.  */

void
lto_elf_end_section (void)
{
  lto_elf_file *file;

  /* Grab the current output file and validate some basic assertions.  */
  file = (lto_elf_file *) lto_get_current_out_file ();
  gcc_assert (file);
  gcc_assert (file->scn);

  file->scn = NULL;
}


/* Validate's ELF_FILE's executable header and, if cached_file_attrs is
   uninitialized, caches the architecture.  */

#define DEFINE_VALIDATE_EHDR(BITS)				\
static bool							\
validate_ehdr##BITS (lto_elf_file *elf_file)			\
{								\
  Elf##BITS##_Ehdr *elf_header;					\
								\
  elf_header = elf##BITS##_getehdr (elf_file->elf);		\
  if (!elf_header)						\
    {								\
      error ("could not read ELF header: %s", elf_errmsg (0));	\
      return false;						\
    }								\
								\
  if (elf_header->e_type != ET_REL)				\
    {								\
      error ("not a relocatable ELF object file");		\
      return false;						\
    }								\
								\
  if (!cached_file_attrs.initialized)				\
    cached_file_attrs.elf_machine = elf_header->e_machine;	\
								\
  if (cached_file_attrs.elf_machine != elf_header->e_machine)	\
    {								\
      error ("inconsistent file architecture detected");	\
      return false;						\
    }								\
								\
  return true;							\
}

DEFINE_VALIDATE_EHDR (32)
DEFINE_VALIDATE_EHDR (64)


/* Validate's ELF_FILE's executable header and, if cached_file_attrs is
   uninitialized, caches the results.  Also records the section header string
   table's section index.  Returns true on success or false on failure.  */

static bool
validate_file (lto_elf_file *elf_file)
{
  const char *elf_ident;

  /* Some aspects of the libelf API are dependent on whether the
     object file is a 32-bit or 64-bit file.  Determine which kind of
     file this is now.  */
  elf_ident = elf_getident (elf_file->elf, NULL);
  if (!elf_ident)
    {
      error ("could not read ELF identification information: %s",
	      elf_errmsg (0));
      return false;
	     
    }

  if (!cached_file_attrs.initialized)
    {
      switch (elf_ident[EI_CLASS])
	{
	case ELFCLASS32:
	  cached_file_attrs.bits = 32;
	  break;

	case ELFCLASS64:
	  cached_file_attrs.bits = 64;
	  break;

	default:
	  error ("unsupported ELF file class");
	  return false;
	}

      memcpy (cached_file_attrs.elf_ident, elf_ident,
	      sizeof cached_file_attrs.elf_ident);
    }

  if (memcmp (elf_ident, cached_file_attrs.elf_ident,
	      sizeof cached_file_attrs.elf_ident))
    return false;

  /* Check that the input file is a relocatable object file with the correct
     architecture.  */
  switch (cached_file_attrs.bits)
    {
    case 32:
      if (!validate_ehdr32 (elf_file))
	return false;
      break;

    case 64:
      if (!validate_ehdr64 (elf_file))
	return false;
      break;

    default:
      gcc_unreachable ();
    }

  /* Read the string table used for section header names.  */
  if (elf_getshdrstrndx (elf_file->elf, &elf_file->sec_strtab) == -1)
    {
      error ("could not locate ELF string table: %s", elf_errmsg (0));
      return false;
    }

  cached_file_attrs.initialized = true;
  return true;
}


/* Helper functions used by init_ehdr.  Initialize ELF_FILE's executable
   header using cached data from previously read files.  */

#define DEFINE_INIT_EHDR(BITS)					      \
static void							      \
init_ehdr##BITS (lto_elf_file *elf_file)			      \
{								      \
  Elf##BITS##_Ehdr *ehdr;					      \
								      \
  gcc_assert (cached_file_attrs.bits);				      \
								      \
  ehdr = elf##BITS##_newehdr (elf_file->elf);			      \
  if (!ehdr)							      \
    {								      \
      if (BITS == 32)						      \
	fatal_error ("elf32_newehdr() failed: %s", elf_errmsg (-1));  \
      else							      \
	fatal_error ("elf64_newehdr() failed: %s", elf_errmsg (-1));  \
    }								      \
								      \
  memcpy (ehdr->e_ident, cached_file_attrs.elf_ident,		      \
	  sizeof cached_file_attrs.elf_ident);			      \
  ehdr->e_type = ET_REL;					      \
  ehdr->e_version = EV_CURRENT;					      \
  ehdr->e_machine = cached_file_attrs.elf_machine;		      \
}

DEFINE_INIT_EHDR (32)
DEFINE_INIT_EHDR (64)


/* Initialize ELF_FILE's executable header using cached data from previously
   read files.  */

static void
init_ehdr (lto_elf_file *elf_file)
{
  switch (cached_file_attrs.bits)
    {
    case 32:
      init_ehdr32 (elf_file);
      break;

    case 64:
      init_ehdr64 (elf_file);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Open ELF file FILENAME.  If WRITABLE is true, the file is opened for write
   and, if necessary, created.  Otherwise, the file is opened for reading.
   Returns the opened file.  */

lto_file *
lto_elf_file_open (const char *filename, bool writable)
{
  lto_elf_file *elf_file;
  lto_file *result = NULL;
  off_t offset;
  off_t header_offset;
  const char *offset_p;
  char *fname;

  offset_p = strchr (filename, '@');
  if (!offset_p)
    {
      fname = xstrdup (filename);
      offset = 0;
      header_offset = 0;
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
      /* elf_rand expects the offset to point to the ar header, not the
         object itself. Subtract the size of the ar header (60 bytes).
         We don't uses sizeof (struct ar_hd) to avoid including ar.h */
      header_offset = offset - 60;
    }

  /* Set up.  */
  elf_file = XCNEW (lto_elf_file);
  result = (lto_file *) elf_file;
  lto_file_init (result, fname, offset);
  elf_file->fd = -1;

  /* Open the file.  */
  elf_file->fd = open (fname, writable ? O_WRONLY|O_CREAT|O_BINARY 
				       : O_RDONLY|O_BINARY, 0666);
  if (elf_file->fd == -1)
    {
      error ("could not open file %s", fname);
      goto fail;
    }

  /* Initialize the ELF library.  */
  if (elf_version (EV_CURRENT) == EV_NONE)
    {
      error ("ELF library is older than that used when building GCC");
      goto fail;
    }

  /* Open the ELF file descriptor.  */
  elf_file->elf = elf_begin (elf_file->fd, writable ? ELF_C_WRITE : ELF_C_READ,
			     NULL);
  if (!elf_file->elf)
    {
      error ("could not open ELF file: %s", elf_errmsg (0));
      goto fail;
    }

  if (offset != 0)
    {
      Elf *e;
      off_t t = elf_rand (elf_file->elf, header_offset);
      if (t != header_offset)
        {
          error ("could not seek in archive");
          goto fail;
        }

      e = elf_begin (elf_file->fd, ELF_C_READ, elf_file->elf);
      if (e == NULL)
        {
          error("could not find archive member");
          goto fail;
        }
      elf_end (elf_file->elf);
      elf_file->elf = e;
    }

  if (writable)
    {
      init_ehdr (elf_file);
      elf_file->shstrtab_stream = XCNEW (struct lto_output_stream);
      /* Output an empty string to the section header table.  This becomes the
	 name of the initial NULL section.  */
      lto_output_1_stream (elf_file->shstrtab_stream, '\0');
    }
  else
    if (!validate_file (elf_file))
      goto fail;

  return result;

 fail:
  if (result)
    lto_elf_file_close (result);
  return NULL;
}


/* Close ELF file FILE and clean up any associated data structures.  If FILE
   was opened for writing, the file's ELF data is written at this time, and
   any cached data buffers are freed.  */

void
lto_elf_file_close (lto_file *file)
{
  lto_elf_file *elf_file = (lto_elf_file *) file;
  struct lto_char_ptr_base *cur, *tmp;

  /* Write the ELF section header string table.  */
  if (elf_file->shstrtab_stream)
    {
      size_t strtab;
      GElf_Ehdr *ehdr_p, ehdr_buf;
      lto_file *old_file = lto_set_current_out_file (file);

      lto_elf_begin_section_with_type (".shstrtab", SHT_STRTAB);
      ehdr_p = gelf_getehdr (elf_file->elf, &ehdr_buf);
      if (ehdr_p == NULL)
	fatal_error ("gelf_getehdr() failed: %s", elf_errmsg (-1));
      strtab = elf_ndxscn (elf_file->scn);
      if (strtab < SHN_LORESERVE)
	ehdr_p->e_shstrndx = strtab;
      else
	{
	  GElf_Shdr *shdr_p, shdr_buf;
	  Elf_Scn *scn_p = elf_getscn (elf_file->elf, 0);
	  if (scn_p == NULL)
	    fatal_error ("elf_getscn() failed: %s", elf_errmsg (-1));
	  shdr_p = gelf_getshdr (scn_p, &shdr_buf);
	  if (shdr_p == NULL)
	    fatal_error ("gelf_getshdr() failed: %s", elf_errmsg (-1));
	  shdr_p->sh_link = strtab;
	  if (gelf_update_shdr (scn_p, shdr_p) == 0)
	    fatal_error ("gelf_update_shdr() failed: %s", elf_errmsg (-1));
	  ehdr_p->e_shstrndx = SHN_XINDEX;
	}
      if (gelf_update_ehdr (elf_file->elf, ehdr_p) == 0)
	fatal_error ("gelf_update_ehdr() failed: %s", elf_errmsg (-1));
      lto_write_stream (elf_file->shstrtab_stream);
      lto_elf_end_section ();

      lto_set_current_out_file (old_file);
      free (elf_file->shstrtab_stream);

      if (elf_update (elf_file->elf, ELF_C_WRITE) < 0)
	fatal_error ("elf_update() failed: %s", elf_errmsg (-1));
    }

  if (elf_file->elf)
    elf_end (elf_file->elf);
  if (elf_file->fd != -1)
    close (elf_file->fd);

  /* Free any ELF data buffers.  */
  cur = elf_file->data;
  while (cur)
    {
      tmp = cur;
      cur = (struct lto_char_ptr_base *) cur->ptr;
      free (tmp);
    }

  free (file);
}


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
