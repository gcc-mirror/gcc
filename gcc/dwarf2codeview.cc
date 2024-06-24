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

#define S_LDATA32		0x110c
#define S_GDATA32		0x110d
#define S_COMPILE3		0x113c

#define CV_CFL_80386		0x03
#define CV_CFL_X64		0xD0

#define CV_CFL_C		0x00
#define CV_CFL_CXX		0x01

#define FIRST_TYPE		0x1000

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

struct codeview_symbol
{
  codeview_symbol *next;
  uint16_t kind;

  union
  {
    struct
    {
      uint32_t type;
      char *name;
      dw_die_ref die;
    } data_symbol;
  };
};

struct codeview_type
{
  dw_die_ref die;
  uint32_t num;
};

struct die_hasher : free_ptr_hash <codeview_type>
{
  typedef dw_die_ref compare_type;

  static hashval_t hash (const codeview_type *x)
  {
    return htab_hash_pointer (x->die);
  }

  static bool equal (const codeview_type *x, const dw_die_ref y)
  {
    return x->die == y;
  }
};

struct codeview_custom_type
{
  struct codeview_custom_type *next;
  uint32_t num;
  uint16_t kind;

  union
  {
    struct
    {
      uint32_t base_type;
      uint32_t attributes;
    } lf_pointer;
    struct
    {
      uint32_t base_type;
      uint16_t modifier;
    } lf_modifier;
  };
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
static codeview_symbol *sym, *last_sym;
static hash_table<die_hasher> *types_htab;
static codeview_custom_type *custom_types, *last_custom_type;

static uint32_t get_type_num (dw_die_ref type);

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

/* Write an S_GDATA32 symbol, representing a global variable, or an S_LDATA32
   symbol, for a static global variable.  */

static void
write_data_symbol (codeview_symbol *s)
{
  unsigned int label_num = ++sym_label_num;
  dw_attr_node *loc;
  dw_loc_descr_ref loc_ref;

  /* This is struct datasym in binutils:

      struct datasym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t type;
	uint32_t offset;
	uint16_t section;
	char name[];
      } ATTRIBUTE_PACKED;
  */

  /* Extract the DW_AT_location attribute from the DIE, and make sure it's in
     in a format we can parse.  */

  loc = get_AT (s->data_symbol.die, DW_AT_location);
  if (!loc)
    goto end;

  if (loc->dw_attr_val.val_class != dw_val_class_loc)
    goto end;

  loc_ref = loc->dw_attr_val.v.val_loc;
  if (!loc_ref || loc_ref->dw_loc_opc != DW_OP_addr)
    goto end;

  /* Output the S_GDATA32 / S_LDATA32 record.  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, s->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->data_symbol.type);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, s->data_symbol.name,
		    strlen (s->data_symbol.name) + 1);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

end:
  free (s->data_symbol.name);
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

  while (sym)
    {
      codeview_symbol *n = sym->next;

      switch (sym->kind)
	{
	case S_LDATA32:
	case S_GDATA32:
	  write_data_symbol (sym);
	  break;
	}

      free (sym);
      sym = n;
    }

  asm_fprintf (asm_out_file, "%LLcv_syms_end:\n");
}

/* Write an LF_POINTER type.  */

static void
write_lf_pointer (codeview_custom_type *t)
{
  /* This is lf_pointer in binutils and lfPointer in Microsoft's cvinfo.h:

    struct lf_pointer
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint32_t attributes;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_pointer.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_pointer.attributes);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* All CodeView type definitions have to be aligned to a four-byte boundary,
   so write some padding bytes if necessary.  These have to be specific values:
   f3, f2, f1.  */

static void
write_cv_padding (size_t padding)
{
  if (padding == 4 || padding == 0)
    return;

  if (padding == 3)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, 0xf3);
      putc ('\n', asm_out_file);
    }

  if (padding >= 2)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, 0xf2);
      putc ('\n', asm_out_file);
    }

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, 0xf1);
  putc ('\n', asm_out_file);
}

/* Write an LF_MODIFIER type, representing a const and/or volatile modification
   of another type.  */

static void
write_lf_modifier (codeview_custom_type *t)
{
  /* This is lf_modifier in binutils and lfModifier in Microsoft's cvinfo.h:

    struct lf_modifier
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint16_t modifier;
      uint16_t padding;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_modifier.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_modifier.modifier);
  putc ('\n', asm_out_file);

  write_cv_padding (2);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write the .debug$T section, which contains all of our custom type
   definitions.  */

static void
write_custom_types (void)
{
  targetm.asm_out.named_section (".debug$T", SECTION_DEBUG, NULL);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, CV_SIGNATURE_C13);
  putc ('\n', asm_out_file);

  while (custom_types)
    {
      codeview_custom_type *n = custom_types->next;

      switch (custom_types->kind)
	{
	case LF_POINTER:
	  write_lf_pointer (custom_types);
	  break;

	case LF_MODIFIER:
	  write_lf_modifier (custom_types);
	  break;
	}

      free (custom_types);
      custom_types = n;
    }
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

  if (custom_types)
    write_custom_types ();

  if (types_htab)
    delete types_htab;
}

/* Translate a DWARF base type (DW_TAG_base_type) into its CodeView
   equivalent.  */

static uint32_t
get_type_num_base_type (dw_die_ref type)
{
  unsigned int size = get_AT_unsigned (type, DW_AT_byte_size);

  switch (get_AT_unsigned (type, DW_AT_encoding))
    {
    case DW_ATE_signed_char:
      {
	const char *name = get_AT_string (type, DW_AT_name);

	if (size != 1)
	  return 0;

	if (name && !strcmp (name, "signed char"))
	  return T_CHAR;
	else
	  return T_RCHAR;
      }

    case DW_ATE_unsigned_char:
      if (size != 1)
	return 0;

      return T_UCHAR;

    case DW_ATE_signed:
      switch (size)
	{
	case 2:
	  return T_SHORT;

	case 4:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "int"))
	      return T_INT4;
	    else
	      return T_LONG;
	  }

	case 8:
	  return T_QUAD;

	default:
	  return 0;
	}

    case DW_ATE_unsigned:
      switch (size)
	{
	case 2:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "wchar_t"))
	      return T_WCHAR;
	    else
	      return T_USHORT;
	  }

	case 4:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "unsigned int"))
	      return T_UINT4;
	    else
	      return T_ULONG;
	  }

	case 8:
	  return T_UQUAD;

	default:
	  return 0;
	}

    case DW_ATE_UTF:
      switch (size)
	{
	case 1:
	  return T_CHAR8;

	case 2:
	  return T_CHAR16;

	case 4:
	  return T_CHAR32;

	default:
	  return 0;
	}

    case DW_ATE_float:
      switch (size)
	{
	case 4:
	  return T_REAL32;

	case 8:
	  return T_REAL64;

	case 12:
	  return T_REAL80;

	case 16:
	  return T_REAL128;

	default:
	  return 0;
	}

    case DW_ATE_boolean:
      if (size == 1)
	return T_BOOL08;
      else
	return 0;

    default:
      return 0;
    }
}

/* Add a new codeview_custom_type to our singly-linked custom_types list.  */

static void
add_custom_type (codeview_custom_type *ct)
{
  uint32_t num;

  if (last_custom_type)
    {
      num = last_custom_type->num + 1;
      last_custom_type->next = ct;
    }
  else
    {
      num = FIRST_TYPE;
      custom_types = ct;
    }

  last_custom_type = ct;

  ct->num = num;
}

/* Process a DW_TAG_pointer_type DIE.  If this is a pointer to a builtin
   type, return the predefined constant for this.  Otherwise, add a new
   LF_POINTER type and return its number.  */

static uint32_t
get_type_num_pointer_type (dw_die_ref type)
{
  uint32_t base_type_num, byte_size;
  dw_die_ref base_type;
  codeview_custom_type *ct;

  byte_size = get_AT_unsigned (type, DW_AT_byte_size);
  if (byte_size != 4 && byte_size != 8)
    return 0;

  base_type = get_AT_ref (type, DW_AT_type);

  /* If DW_AT_type is not set, this must be a void pointer.  */
  if (!base_type)
    return byte_size == 4 ? T_32PVOID : T_64PVOID;

  base_type_num = get_type_num (base_type);
  if (base_type_num == 0)
    return 0;

  /* Pointers to builtin types have predefined type numbers, with the top byte
     determining the pointer size - 0x0400 for a 32-bit pointer and 0x0600
     for 64-bit.  */
  if (base_type_num < FIRST_TYPE && !(base_type_num & 0xff00))
    {
      if (byte_size == 4)
	return CV_POINTER_32 | base_type_num;
      else
	return CV_POINTER_64 | base_type_num;
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_POINTER;
  ct->lf_pointer.base_type = base_type_num;

  if (byte_size == 4)
    ct->lf_pointer.attributes = CV_PTR_NEAR32;
  else
    ct->lf_pointer.attributes = CV_PTR_64;

  ct->lf_pointer.attributes |= byte_size << 13;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_const_type DIE, adding an LF_MODIFIER type and returning
   its number.  */

static uint32_t
get_type_num_const_type (dw_die_ref type)
{
  dw_die_ref base_type;
  uint32_t base_type_num;
  codeview_custom_type *ct;
  bool is_volatile = false;

  base_type = get_AT_ref (type, DW_AT_type);
  if (!base_type)
    return 0;

  /* Handle case when this is a const volatile type - we only need one
     LF_MODIFIER for this.  */
  if (dw_get_die_tag (base_type) == DW_TAG_volatile_type)
    {
      is_volatile = true;

      base_type = get_AT_ref (base_type, DW_AT_type);
      if (!base_type)
	return 0;
    }

  base_type_num = get_type_num (base_type);
  if (base_type_num == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MODIFIER;
  ct->lf_modifier.base_type = base_type_num;
  ct->lf_modifier.modifier = MOD_const;

  if (is_volatile)
    ct->lf_modifier.modifier |= MOD_volatile;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_volatile_type DIE, adding an LF_MODIFIER type and
   returning its number.  */

static uint32_t
get_type_num_volatile_type (dw_die_ref type)
{
  uint32_t base_type_num;
  codeview_custom_type *ct;

  base_type_num = get_type_num (get_AT_ref (type, DW_AT_type));
  if (base_type_num == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MODIFIER;
  ct->lf_modifier.base_type = base_type_num;
  ct->lf_modifier.modifier = MOD_volatile;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DIE representing a type definition, add a CodeView type if
   necessary, and return its number.  If it's something we can't handle, return
   0.  We keep a hash table so that we're not adding the same type multiple
   times - though if we do it's not disastrous, as ld will deduplicate
   everything for us.  */

static uint32_t
get_type_num (dw_die_ref type)
{
  codeview_type **slot, *t;

  if (!type)
    return 0;

  if (!types_htab)
    types_htab = new hash_table<die_hasher> (10);

  slot = types_htab->find_slot_with_hash (type, htab_hash_pointer (type),
					  INSERT);

  if (*slot)
    return (*slot)->num;

  t = (codeview_type *) xmalloc (sizeof (codeview_type));
  t->die = type;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_base_type:
      t->num = get_type_num_base_type (type);
      break;

    case DW_TAG_typedef:
      /* FIXME - signed longs typedef'd as "HRESULT" should get their
		 own type (T_HRESULT) */
      t->num = get_type_num (get_AT_ref (type, DW_AT_type));
      break;

    case DW_TAG_pointer_type:
      t->num = get_type_num_pointer_type (type);
      break;

    case DW_TAG_const_type:
      t->num = get_type_num_const_type (type);
      break;

    case DW_TAG_volatile_type:
      t->num = get_type_num_volatile_type (type);
      break;

    default:
      t->num = 0;
      break;
    }

  *slot = t;

  return t->num;
}

/* Process a DW_TAG_variable DIE, and add an S_GDATA32 or S_LDATA32 symbol for
   this.  */

static void
add_variable (dw_die_ref die)
{
  codeview_symbol *s;
  const char *name;

  name = get_AT_string (die, DW_AT_name);
  if (!name)
    return;

  s = (codeview_symbol *) xmalloc (sizeof (codeview_symbol));

  s->next = NULL;
  s->kind = get_AT (die, DW_AT_external) ? S_GDATA32 : S_LDATA32;
  s->data_symbol.type = get_type_num (get_AT_ref (die, DW_AT_type));
  s->data_symbol.name = xstrdup (name);
  s->data_symbol.die = die;

  if (last_sym)
    last_sym->next = s;
  else
    sym = s;

  last_sym = s;
}

/* Loop through the DIEs that have been output for our TU, and add CodeView
   symbols for them.  */

void
codeview_debug_early_finish (dw_die_ref die)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;

  do
    {
      if (dw_get_die_tag (c) == DW_TAG_variable)
	add_variable (c);

      c = dw_get_die_sib (c);
    }
  while (c != first_child);
}

#endif
