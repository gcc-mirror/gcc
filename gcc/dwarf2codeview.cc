/* Generate CodeView debugging info from the GCC DWARF.
   Copyright (C) 2023 Free Software Foundation, Inc.

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

/* See gas/codeview.h in binutils for more about the constants and structs
   listed below.  References to Microsoft files refer to Microsoft's PDB
   repository: https://github.com/microsoft/microsoft-pdb.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "output.h"
#include "errors.h"
#include "md5.h"
#include "function.h"
#include "version.h"
#include "tree.h"
#include "langhooks.h"
#include "dwarf2out.h"
#include "dwarf2codeview.h"

#ifdef CODEVIEW_DEBUGGING_INFO

#define CV_SIGNATURE_C13	4

#define DEBUG_S_STRINGTABLE     0xf3
#define DEBUG_S_FILECHKSMS      0xf4

#define CHKSUM_TYPE_MD5		1

#define HASH_SIZE 16

struct codeview_string
{
  codeview_string *next;
  uint32_t offset;
  char *string;
};

struct string_hasher : free_ptr_hash <struct codeview_string>
{
  typedef const char *compare_type;

  static hashval_t hash (const codeview_string *x)
  {
    return htab_hash_string (x->string);
  }

  static bool equal (const codeview_string *x, const char *y)
  {
    return !strcmp (x->string, y);
  }

  static void mark_empty (codeview_string *x)
  {
    if (x->string)
      {
	free (x->string);
	x->string = NULL;
      }
  }

  static void remove (codeview_string *&x)
  {
    free (x->string);
  }
};

struct codeview_source_file
{
  codeview_source_file *next;
  unsigned int file_num;
  uint32_t string_offset;
  char *filename;
  uint8_t hash[HASH_SIZE];
};

static codeview_source_file *files, *last_file;
static unsigned int num_files;
static uint32_t string_offset = 1;
static hash_table<string_hasher> *strings_htab;
static codeview_string *strings, *last_string;

/* Adds string to the string table, returning its offset.  If already present,
   this returns the offset of the existing string.  */

static uint32_t
add_string (const char *string)
{
  codeview_string **slot;
  codeview_string *s;
  size_t len;

  if (!strings_htab)
    strings_htab = new hash_table<string_hasher> (10);

  slot = strings_htab->find_slot_with_hash (string, htab_hash_string (string),
					    INSERT);

  if (*slot)
    return (*slot)->offset;

  s = (codeview_string *) xmalloc (sizeof (codeview_string));
  len = strlen (string);

  s->next = NULL;

  s->offset = string_offset;
  string_offset += len + 1;

  s->string = xstrdup (string);

  if (last_string)
    last_string->next = s;
  else
    strings = s;

  last_string = s;

  *slot = s;

  return s->offset;
}

/* A new source file has been encountered - record the details and calculate
   its hash.  */

void
codeview_start_source_file (const char *filename)
{
  codeview_source_file *sf;
  char *path;
  uint32_t string_offset;
  FILE *f;

  path = lrealpath (filename);
  string_offset = add_string (path);
  free (path);

  sf = files;
  while (sf)
    {
      if (sf->string_offset == string_offset)
	return;

      sf = sf->next;
    }

  sf = (codeview_source_file *) xmalloc (sizeof (codeview_source_file));
  sf->next = NULL;
  sf->file_num = num_files;
  sf->string_offset = string_offset;
  sf->filename = xstrdup (filename);

  f = fopen (filename, "r");
  if (!f)
    internal_error ("could not open %s for reading", filename);

  if (md5_stream (f, sf->hash))
    {
      fclose (f);
      internal_error ("md5_stream failed");
    }

  fclose (f);

  if (last_file)
    last_file->next = sf;
  else
    files = sf;

  last_file = sf;
  num_files++;
}

/* Write out the strings table into the .debug$S section.  The linker will
   parse this, and handle the deduplication and hashing for all the object
   files.  */

static void
write_strings_table (void)
{
  codeview_string *string;

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_STRINGTABLE);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_strings_end - %LLcv_strings_start\n");

  asm_fprintf (asm_out_file, "%LLcv_strings_start:\n");

  /* The first entry is always an empty string.  */
  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  string = strings;
  while (string)
    {
      ASM_OUTPUT_ASCII (asm_out_file, string->string,
			strlen (string->string) + 1);

      string = string->next;
    }

  delete strings_htab;

  asm_fprintf (asm_out_file, "%LLcv_strings_end:\n");

  ASM_OUTPUT_ALIGN (asm_out_file, 2);
}

/* Write out the file checksums data into the .debug$S section.  */

static void
write_source_files (void)
{
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_FILECHKSMS);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%LLcv_filechksms_end - %LLcv_filechksms_start\n");

  asm_fprintf (asm_out_file, "%LLcv_filechksms_start:\n");

  while (files)
    {
      codeview_source_file *next = files->next;

      /* This is struct file_checksum in binutils, or filedata in Microsoft's
	 dumpsym7.cpp:

	struct file_checksum
	{
	  uint32_t file_id;
	  uint8_t checksum_length;
	  uint8_t checksum_type;
	} ATTRIBUTE_PACKED;

	followed then by the bytes of the hash, padded to the next 4 bytes.
	file_id here is actually the offset in the strings table.  */

      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, files->string_offset);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, HASH_SIZE);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, CHKSUM_TYPE_MD5);
      putc ('\n', asm_out_file);

      for (unsigned int i = 0; i < HASH_SIZE; i++)
	{
	  fputs (integer_asm_op (1, false), asm_out_file);
	  fprint_whex (asm_out_file, files->hash[i]);
	  putc ('\n', asm_out_file);
	}

      ASM_OUTPUT_ALIGN (asm_out_file, 2);

      free (files->filename);
      free (files);

      files = next;
    }

  asm_fprintf (asm_out_file, "%LLcv_filechksms_end:\n");
}

/* Finish CodeView debug info emission.  */

void
codeview_debug_finish (void)
{
  targetm.asm_out.named_section (".debug$S", SECTION_DEBUG, NULL);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, CV_SIGNATURE_C13);
  putc ('\n', asm_out_file);

  write_strings_table ();
  write_source_files ();
}

#endif
