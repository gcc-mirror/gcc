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

#define DEBUG_S_SYMBOLS		0xf1
#define DEBUG_S_LINES		0xf2
#define DEBUG_S_STRINGTABLE     0xf3
#define DEBUG_S_FILECHKSMS      0xf4

#define CHKSUM_TYPE_MD5		1

#define S_COMPILE3		0x113c

#define CV_CFL_80386		0x03
#define CV_CFL_X64		0xD0

#define CV_CFL_C		0x00
#define CV_CFL_CXX		0x01

#define LINE_LABEL	"Lcvline"
#define END_FUNC_LABEL	"Lcvendfunc"
#define SYMBOL_START_LABEL	"Lcvsymstart"
#define SYMBOL_END_LABEL	"Lcvsymend"

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

struct codeview_line
{
  codeview_line *next;
  unsigned int line_no;
  unsigned int label_num;
};

struct codeview_line_block
{
  codeview_line_block *next;
  uint32_t file_id;
  unsigned int num_lines;
  codeview_line *lines, *last_line;
};

struct codeview_function
{
  codeview_function *next;
  function *func;
  unsigned int end_label;
  codeview_line_block *blocks, *last_block;
};

static unsigned int line_label_num;
static unsigned int func_label_num;
static unsigned int sym_label_num;
static codeview_source_file *files, *last_file;
static unsigned int num_files;
static uint32_t string_offset = 1;
static hash_table<string_hasher> *strings_htab;
static codeview_string *strings, *last_string;
static codeview_function *funcs, *last_func;
static const char* last_filename;
static uint32_t last_file_id;

/* Record new line number against the current function.  */

void
codeview_source_line (unsigned int line_no, const char *filename)
{
  codeview_line *l;
  uint32_t file_id = last_file_id;
  unsigned int label_num = ++line_label_num;

  targetm.asm_out.internal_label (asm_out_file, LINE_LABEL, label_num);

  if (!last_func || last_func->func != cfun)
    {
      codeview_function *f = (codeview_function *)
				xmalloc (sizeof (codeview_function));

      f->next = NULL;
      f->func = cfun;
      f->end_label = 0;
      f->blocks = f->last_block = NULL;

      if (!funcs)
	funcs = f;
      else
	last_func->next = f;

      last_func = f;
    }

  if (filename != last_filename)
    {
      codeview_source_file *sf = files;

      while (sf)
	{
	  if (!strcmp (sf->filename, filename))
	    {
	      /* 0x18 is the size of the checksum entry for each file.
		 0x6 bytes for the header, plus 0x10 bytes for the hash,
		 then padded to a multiple of 4.  */

	      file_id = sf->file_num * 0x18;
	      last_filename = filename;
	      last_file_id = file_id;
	      break;
	    }

	  sf = sf->next;
	}
    }

  if (!last_func->last_block || last_func->last_block->file_id != file_id)
    {
      codeview_line_block *b;

      b = (codeview_line_block *) xmalloc (sizeof (codeview_line_block));

      b->next = NULL;
      b->file_id = file_id;
      b->num_lines = 0;
      b->lines = b->last_line = NULL;

      if (!last_func->blocks)
	last_func->blocks = b;
      else
	last_func->last_block->next = b;

      last_func->last_block = b;
    }

  if (last_func->last_block->last_line
    && last_func->last_block->last_line->line_no == line_no)
    return;

  l = (codeview_line *) xmalloc (sizeof (codeview_line));

  l->next = NULL;
  l->line_no = line_no;
  l->label_num = label_num;

  if (!last_func->last_block->lines)
    last_func->last_block->lines = l;
  else
    last_func->last_block->last_line->next = l;

  last_func->last_block->last_line = l;
  last_func->last_block->num_lines++;
}

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

/* Write out the line number information for each function into the
   .debug$S section.  */

static void
write_line_numbers (void)
{
  unsigned int func_num = 0;

  while (funcs)
    {
      codeview_function *next = funcs->next;
      unsigned int first_label_num;

      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, DEBUG_S_LINES);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (4, false), asm_out_file);
      asm_fprintf (asm_out_file, "%LLcv_lines%u_end - %LLcv_lines%u_start\n",
		   func_num, func_num);

      asm_fprintf (asm_out_file, "%LLcv_lines%u_start:\n", func_num);

      /* Output the header (struct cv_lines_header in binutils or
	 CV_DebugSLinesHeader_t in Microsoft's cvinfo.h):

	struct cv_lines_header
	{
	  uint32_t offset;
	  uint16_t section;
	  uint16_t flags;
	  uint32_t length;
	};
      */

      asm_fprintf (asm_out_file, "\t.secrel32\t%L" LINE_LABEL "%u\n",
		   funcs->blocks->lines->label_num);
      asm_fprintf (asm_out_file, "\t.secidx\t%L" LINE_LABEL "%u\n",
		   funcs->blocks->lines->label_num);

      /* flags */
      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, 0);
      putc ('\n', asm_out_file);

      first_label_num = funcs->blocks->lines->label_num;

      /* length */
      fputs (integer_asm_op (4, false), asm_out_file);
      asm_fprintf (asm_out_file,
		   "%L" END_FUNC_LABEL "%u - %L" LINE_LABEL "%u\n",
		   funcs->end_label, first_label_num);

      while (funcs->blocks)
	{
	  codeview_line_block *next = funcs->blocks->next;

	  /* Next comes the blocks, each block being a part of a function
	     within the same source file (struct cv_lines_block in binutils or
	     CV_DebugSLinesFileBlockHeader_t in Microsoft's cvinfo.h):

	    struct cv_lines_block
	    {
	      uint32_t file_id;
	      uint32_t num_lines;
	      uint32_t length;
	    };
	  */

	  /* file ID */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, funcs->blocks->file_id);
	  putc ('\n', asm_out_file);

	  /* number of lines */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, funcs->blocks->num_lines);
	  putc ('\n', asm_out_file);

	  /* length of code block: (num_lines * sizeof (struct cv_line)) +
	     sizeof (struct cv_lines_block) */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, (funcs->blocks->num_lines * 0x8) + 0xc);
	  putc ('\n', asm_out_file);

	  while (funcs->blocks->lines)
	    {
	      codeview_line *next = funcs->blocks->lines->next;

	      /* Finally comes the line number information (struct cv_line in
		 binutils or CV_Line_t in Microsoft's cvinfo.h):

		struct cv_line
		{
		  uint32_t offset;
		  uint32_t line_no;
		};

		Strictly speaking line_no is a bitfield: the bottom 24 bits
		are the line number, and the top bit means "is a statement".
	      */

	      fputs (integer_asm_op (4, false), asm_out_file);
	      asm_fprintf (asm_out_file,
			   "%L" LINE_LABEL "%u - %L" LINE_LABEL "%u\n",
			   funcs->blocks->lines->label_num, first_label_num);

	      fputs (integer_asm_op (4, false), asm_out_file);
	      fprint_whex (asm_out_file,
			   0x80000000
			   | (funcs->blocks->lines->line_no & 0xffffff));
	      putc ('\n', asm_out_file);

	      free (funcs->blocks->lines);

	      funcs->blocks->lines = next;
	    }

	  free (funcs->blocks);

	  funcs->blocks = next;
	}

      free (funcs);

      asm_fprintf (asm_out_file, "%LLcv_lines%u_end:\n", func_num);
      func_num++;

      funcs = next;
    }
}

/* Treat cold sections as separate functions, for the purposes of line
   numbers.  */

void
codeview_switch_text_section (void)
{
  codeview_function *f;

  if (last_func && last_func->end_label == 0)
    {
      unsigned int label_num = ++func_label_num;

      targetm.asm_out.internal_label (asm_out_file, END_FUNC_LABEL,
				      label_num);

      last_func->end_label = label_num;
    }

  f = (codeview_function *) xmalloc (sizeof (codeview_function));

  f->next = NULL;
  f->func = cfun;
  f->end_label = 0;
  f->blocks = f->last_block = NULL;

  if (!funcs)
    funcs = f;
  else
    last_func->next = f;

  last_func = f;
}

/* Mark the end of the current function.  */

void
codeview_end_epilogue (void)
{
  if (last_func && last_func->end_label == 0)
    {
      unsigned int label_num = ++func_label_num;

      targetm.asm_out.internal_label (asm_out_file, END_FUNC_LABEL,
				      label_num);

      last_func->end_label = label_num;
    }
}

/* Return the CodeView constant for the selected architecture.  */

static uint16_t
target_processor (void)
{
  if (TARGET_64BIT)
    return CV_CFL_X64;
  else
    return CV_CFL_80386;
}

/* Return the CodeView constant for the language being used.  */

static uint32_t
language_constant (void)
{
  const char *language_string = lang_hooks.name;

  if (startswith (language_string, "GNU C++"))
    return CV_CFL_CXX;
  else if (startswith (language_string, "GNU C"))
    return CV_CFL_C;

  return 0;
}

/* Write a S_COMPILE3 symbol, which records the details of the compiler
   being used.  */

static void
write_compile3_symbol (void)
{
  unsigned int label_num = ++sym_label_num;

  static const char compiler_name[] = "GCC ";

  /* This is struct COMPILESYM3 in binutils and Microsoft's cvinfo.h:

     struct COMPILESYM3
     {
       uint16_t length;
       uint16_t type;
       uint32_t flags;
       uint16_t machine;
       uint16_t frontend_major;
       uint16_t frontend_minor;
       uint16_t frontend_build;
       uint16_t frontend_qfe;
       uint16_t backend_major;
       uint16_t backend_minor;
       uint16_t backend_build;
       uint16_t backend_qfe;
     } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_COMPILE3);
  putc ('\n', asm_out_file);

  /* Microsoft has the flags as a bitfield, with the bottom 8 bits being the
     language constant, and the reset being MSVC-specific stuff.  */
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, language_constant ());
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, target_processor ());
  putc ('\n', asm_out_file);

  /* Write 8 uint16_ts for the frontend and backend versions.  As with GAS, we
     zero these, as it's easier to record the version in the compiler
     string.  */
  for (unsigned int i = 0; i < 8; i++)
    {
      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, 0);
      putc ('\n', asm_out_file);
    }

  ASM_OUTPUT_ASCII (asm_out_file, compiler_name, sizeof (compiler_name) - 1);
  ASM_OUTPUT_ASCII (asm_out_file, version_string, strlen (version_string) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write the CodeView symbols into the .debug$S section.  */

static void
write_codeview_symbols (void)
{
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_SYMBOLS);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_syms_end - %LLcv_syms_start\n");

  asm_fprintf (asm_out_file, "%LLcv_syms_start:\n");

  write_compile3_symbol ();

  asm_fprintf (asm_out_file, "%LLcv_syms_end:\n");
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
  write_line_numbers ();
  write_codeview_symbols ();
}

#endif
