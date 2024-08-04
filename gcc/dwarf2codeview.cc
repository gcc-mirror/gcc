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
#include "rtl.h"

#ifdef CODEVIEW_DEBUGGING_INFO

#define CV_SIGNATURE_C13	4

#define DEBUG_S_SYMBOLS		0xf1
#define DEBUG_S_LINES		0xf2
#define DEBUG_S_STRINGTABLE     0xf3
#define DEBUG_S_FILECHKSMS      0xf4

#define CHKSUM_TYPE_MD5		1

#define CV_CFL_80386		0x03
#define CV_CFL_X64		0xD0

#define CV_CFL_C		0x00
#define CV_CFL_CXX		0x01

#define FIRST_TYPE		0x1000

#define LINE_LABEL	"Lcvline"
#define END_FUNC_LABEL	"Lcvendfunc"
#define SYMBOL_START_LABEL	"Lcvsymstart"
#define SYMBOL_END_LABEL	"Lcvsymend"

/* There's two bytes available for each type's size, but follow MSVC's lead in
   capping the LF_FIELDLIST size at fb00 (minus 8 bytes for the LF_INDEX
   pointing to the overflow entry).  */
#define MAX_FIELDLIST_SIZE	0xfaf8

#define HASH_SIZE 16

/* This is enum SYM_ENUM_e in Microsoft's cvinfo.h.  */

enum cv_sym_type {
  S_LDATA32 = 0x110c,
  S_GDATA32 = 0x110d,
  S_COMPILE3 = 0x113c,
  S_LPROC32_ID = 0x1146,
  S_GPROC32_ID = 0x1147,
  S_PROC_ID_END = 0x114f
};

/* This is enum LEAF_ENUM_e in Microsoft's cvinfo.h.  */

enum cv_leaf_type {
  LF_PAD1 = 0xf1,
  LF_PAD2 = 0xf2,
  LF_PAD3 = 0xf3,
  LF_MODIFIER = 0x1001,
  LF_POINTER = 0x1002,
  LF_PROCEDURE = 0x1008,
  LF_ARGLIST = 0x1201,
  LF_FIELDLIST = 0x1203,
  LF_BITFIELD = 0x1205,
  LF_INDEX = 0x1404,
  LF_ENUMERATE = 0x1502,
  LF_ARRAY = 0x1503,
  LF_CLASS = 0x1504,
  LF_STRUCTURE = 0x1505,
  LF_UNION = 0x1506,
  LF_ENUM = 0x1507,
  LF_MEMBER = 0x150d,
  LF_FUNC_ID = 0x1601,
  LF_CHAR = 0x8000,
  LF_SHORT = 0x8001,
  LF_USHORT = 0x8002,
  LF_LONG = 0x8003,
  LF_ULONG = 0x8004,
  LF_QUADWORD = 0x8009,
  LF_UQUADWORD = 0x800a
};

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
  enum cv_sym_type kind;

  union
  {
    struct
    {
      uint32_t type;
      char *name;
      dw_die_ref die;
    } data_symbol;
    struct
    {
      uint32_t parent;
      uint32_t end;
      uint32_t next;
      uint32_t type;
      uint8_t flags;
      char *name;
      dw_die_ref die;
    } function;
  };
};

struct codeview_type
{
  dw_die_ref die;
  uint32_t num;
  bool is_fwd_ref;
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

struct codeview_integer
{
  bool neg;
  uint64_t num;
};

struct codeview_subtype
{
  struct codeview_subtype *next;
  enum cv_leaf_type kind;

  union
  {
    struct
    {
      char *name;
      struct codeview_integer value;
    } lf_enumerate;
    struct
    {
      uint32_t type_num;
    } lf_index;
    struct
    {
      uint16_t attributes;
      uint32_t type;
      codeview_integer offset;
      char *name;
    } lf_member;
  };
};

struct codeview_custom_type
{
  struct codeview_custom_type *next;
  uint32_t num;
  enum cv_leaf_type kind;

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
    struct
    {
      size_t length;
      codeview_subtype *subtypes;
      codeview_subtype *last_subtype;
    } lf_fieldlist;
    struct
    {
      uint16_t count;
      uint16_t properties;
      uint32_t underlying_type;
      uint32_t fieldlist;
      char *name;
    } lf_enum;
    struct
    {
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint32_t derived_from;
      uint32_t vshape;
      codeview_integer length;
      char *name;
    } lf_structure;
    struct
    {
      uint32_t element_type;
      uint32_t index_type;
      codeview_integer length_in_bytes;
    } lf_array;
    struct
    {
      uint32_t base_type;
      uint8_t length;
      uint8_t position;
    } lf_bitfield;
    struct
    {
      uint32_t return_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
    } lf_procedure;
    struct
    {
      uint32_t num_entries;
      uint32_t *args;
    } lf_arglist;
    struct
    {
      uint32_t parent_scope;
      uint32_t function_type;
      char *name;
    } lf_func_id;
  };
};

struct codeview_deferred_type
{
  struct codeview_deferred_type *next;
  dw_die_ref type;
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
static codeview_deferred_type *deferred_types, *last_deferred_type;

static uint32_t get_type_num (dw_die_ref type, bool in_struct, bool no_fwd_ref);

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

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

end:
  free (s->data_symbol.name);
}

/* Write an S_GPROC32_ID symbol, representing a global function, or an
   S_LPROC32_ID symbol, for a static function.  */

static void
write_function (codeview_symbol *s)
{
  unsigned int label_num = ++sym_label_num;
  dw_attr_node *loc_low, *loc_high;
  const char *label_low, *label_high;
  rtx rtx_low, rtx_high;

  /* This is struct procsym in binutils and PROCSYM32 in Microsoft's cvinfo.h:

      struct procsym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t parent;
	uint32_t end;
	uint32_t next;
	uint32_t proc_len;
	uint32_t debug_start;
	uint32_t debug_end;
	uint32_t type;
	uint32_t offset;
	uint16_t section;
	uint8_t flags;
	char name[];
      } ATTRIBUTE_PACKED;
  */

  loc_low = get_AT (s->function.die, DW_AT_low_pc);
  if (!loc_low)
    goto end;

  if (loc_low->dw_attr_val.val_class != dw_val_class_lbl_id)
    goto end;

  label_low = loc_low->dw_attr_val.v.val_lbl_id;
  if (!label_low)
    goto end;

  rtx_low = gen_rtx_SYMBOL_REF (Pmode, label_low);

  loc_high = get_AT (s->function.die, DW_AT_high_pc);
  if (!loc_high)
    goto end;

  if (loc_high->dw_attr_val.val_class != dw_val_class_high_pc)
    goto end;

  label_high = loc_high->dw_attr_val.v.val_lbl_id;
  if (!label_high)
    goto end;

  rtx_high = gen_rtx_SYMBOL_REF (Pmode, label_high);

  /* Output the S_GPROC32_ID / S_LPROC32_ID record.  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, s->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.parent);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.end);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.next);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  output_addr_const (asm_out_file, rtx_high);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, rtx_low);
  putc ('\n', asm_out_file);

  /* FIXME - debug_start should be the end of the prologue, and debug_end
	     the beginning of the epilogue.  Do the whole function for
	     now.  */

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  output_addr_const (asm_out_file, rtx_high);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, rtx_low);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.type);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.flags);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, s->function.name,
		    strlen (s->function.name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

  /* Output the S_PROC_ID_END record.  */

  label_num = ++sym_label_num;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_PROC_ID_END);
  putc ('\n', asm_out_file);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

end:
  free (s->function.name);
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
	case S_LPROC32_ID:
	case S_GPROC32_ID:
	  write_function (sym);
	  break;
	default:
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
   LF_PAD3, LF_PAD2, LF_PAD1.  */

static void
write_cv_padding (size_t padding)
{
  if (padding == 4 || padding == 0)
    return;

  if (padding == 3)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, LF_PAD3);
      putc ('\n', asm_out_file);
    }

  if (padding >= 2)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, LF_PAD2);
      putc ('\n', asm_out_file);
    }

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, LF_PAD1);
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

/* Write a CodeView extensible integer.  If the value is non-negative and
   < 0x8000, the value gets written directly as an uint16_t.  Otherwise, we
   output two bytes for the integer type (LF_CHAR, LF_SHORT, ...), and the
   actual value follows.  Returns the total number of bytes written.  */

static size_t
write_cv_integer (codeview_integer *i)
{
  if (i->neg)
    {
      if (i->num <= 0x80)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_CHAR);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (1, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 3;
	}
      else if (i->num <= 0x8000)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_SHORT);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 4;
	}
      else if (i->num <= 0x80000000)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_LONG);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 6;
	}
      else
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_QUADWORD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (8, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 10;
	}
    }
  else
    {
      if (i->num <= 0x7fff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 2;
	}
      else if (i->num <= 0xffff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_USHORT);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 4;
	}
      else if (i->num <= 0xffffffff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_ULONG);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 6;
	}
      else
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_UQUADWORD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (8, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 10;
	}
    }
}

/* Return the extra size needed for an extensible integer.  */

static size_t
cv_integer_len (codeview_integer *i)
{
  if (i->neg)
    {
      if (i->num <= 0x80)
	return sizeof (int8_t);
      else if (i->num <= 0x8000)
	return sizeof (int16_t);
      else if (i->num <= 0x80000000)
	return sizeof (int32_t);
      else
	return sizeof (int64_t);
    }
  else
    {
      if (i->num <= 0x7fff)
	return 0;
      else if (i->num <= 0xffff)
	return sizeof (uint16_t);
      else if (i->num <= 0xffffffff)
	return sizeof (uint32_t);
      else
	return sizeof (uint64_t);
    }
}

/* Write an LF_FIELDLIST type, which is a container for various subtypes.  This
   has two uses: for the values in an enum, and for the member, operators etc.
   for a struct, class, or union.  */

static void
write_lf_fieldlist (codeview_custom_type *t)
{
  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  while (t->lf_fieldlist.subtypes)
    {
      codeview_subtype *v = t->lf_fieldlist.subtypes;
      codeview_subtype *next = v->next;
      size_t name_len, leaf_len;

      switch (v->kind)
	{
	case LF_ENUMERATE:
	  /* This is lf_enumerate in binutils and lfEnumerate in Microsoft's
	     cvinfo.h:

	    struct lf_enumerate
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint16_t value;
	      (then actual value if value >= 0x8000)
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_ENUMERATE);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, CV_ACCESS_PUBLIC);
	  putc ('\n', asm_out_file);

	  leaf_len = 4 + write_cv_integer (&v->lf_enumerate.value);

	  name_len = strlen (v->lf_enumerate.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_enumerate.name, name_len);

	  leaf_len += name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_enumerate.name);
	  break;

	case LF_MEMBER:
	  /* This is lf_member in binutils and lfMember in Microsoft's
	     cvinfo.h:

	    struct lf_member
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint32_t type;
	      uint16_t offset;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_MEMBER);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_member.attributes);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_member.type);
	  putc ('\n', asm_out_file);

	  leaf_len = 8 + write_cv_integer (&v->lf_member.offset);

	  if (v->lf_member.name)
	    {
	      name_len = strlen (v->lf_member.name) + 1;
	      ASM_OUTPUT_ASCII (asm_out_file, v->lf_member.name, name_len);
	    }
	  else
	    {
	      name_len = 1;
	      ASM_OUTPUT_ASCII (asm_out_file, "", name_len);
	    }

	  leaf_len += name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_member.name);
	  break;

	case LF_INDEX:
	  /* This is lf_index in binutils and lfIndex in Microsoft's cvinfo.h:

	    struct lf_index
	    {
	      uint16_t kind;
	      uint16_t padding;
	      uint32_t index;
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_INDEX);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, 0);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_index.type_num);
	  putc ('\n', asm_out_file);

	  break;

	default:
	  break;
	}

      t->lf_fieldlist.subtypes = next;
      free (v);
    }

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ENUM type.  */

static void
write_lf_enum (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_enum in binutils and lfEnum in Microsoft's cvinfo.h:

    struct lf_enum
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_elements;
      uint16_t properties;
      uint32_t underlying_type;
      uint32_t field_list;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.count);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.underlying_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.fieldlist);
  putc ('\n', asm_out_file);

  name_len = strlen (t->lf_enum.name) + 1;
  ASM_OUTPUT_ASCII (asm_out_file, t->lf_enum.name, name_len);

  leaf_len = 14 + name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_enum.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_STRUCTURE or LF_CLASS type (the two have the same structure).  */

static void
write_lf_structure (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_class in binutils and lfClass in Microsoft's cvinfo.h:

    struct lf_class
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint32_t derived_from;
      uint32_t vshape;
      uint16_t length;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.num_members);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.field_list);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.derived_from);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.vshape);
  putc ('\n', asm_out_file);

  leaf_len = 20 + write_cv_integer (&t->lf_structure.length);

  if (t->lf_structure.name)
    {
      name_len = strlen (t->lf_structure.name) + 1;
      ASM_OUTPUT_ASCII (asm_out_file, t->lf_structure.name, name_len);
    }
  else
    {
      static const char unnamed_struct[] = "<unnamed-tag>";

      name_len = sizeof (unnamed_struct);
      ASM_OUTPUT_ASCII (asm_out_file, unnamed_struct, name_len);
    }

  leaf_len += name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_structure.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_UNION type.  */

static void
write_lf_union (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_union in binutils and lfUnion in Microsoft's cvinfo.h:

    struct lf_union
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint16_t length;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.num_members);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.field_list);
  putc ('\n', asm_out_file);

  leaf_len = 12 + write_cv_integer (&t->lf_structure.length);

  if (t->lf_structure.name)
    {
      name_len = strlen (t->lf_structure.name) + 1;
      ASM_OUTPUT_ASCII (asm_out_file, t->lf_structure.name, name_len);
    }
  else
    {
      static const char unnamed_struct[] = "<unnamed-tag>";

      name_len = sizeof (unnamed_struct);
      ASM_OUTPUT_ASCII (asm_out_file, unnamed_struct, name_len);
    }

  leaf_len += name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_structure.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ARRAY type.  */

static void
write_lf_array (codeview_custom_type *t)
{
  size_t leaf_len;

  /* This is lf_array in binutils and lfArray in Microsoft's cvinfo.h:

    struct lf_array
    {
      uint16_t size;
      uint16_t kind;
      uint32_t element_type;
      uint32_t index_type;
      uint16_t length_in_bytes;
      char name[];
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
  fprint_whex (asm_out_file, t->lf_array.element_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_array.index_type);
  putc ('\n', asm_out_file);

  leaf_len = 13 + write_cv_integer (&t->lf_array.length_in_bytes);

  ASM_OUTPUT_ASCII (asm_out_file, "", 1);

  write_cv_padding (4 - (leaf_len % 4));

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_BITFIELD type.  */

static void
write_lf_bitfield (codeview_custom_type *t)
{
  /* This is lf_bitfield in binutils and lfBitfield in Microsoft's cvinfo.h:

    struct lf_bitfield
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint8_t length;
      uint8_t position;
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
  fprint_whex (asm_out_file, t->lf_bitfield.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_bitfield.length);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_bitfield.position);
  putc ('\n', asm_out_file);

  write_cv_padding (2);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_PROCEDURE type.  Function pointers are implemented as pointers
   to one of these.  */

static void
write_lf_procedure (codeview_custom_type *t)
{
  /* This is lf_procedure in binutils and lfProc in Microsoft's cvinfo.h:

    struct lf_procedure
    {
      uint16_t size;
      uint16_t kind;
      uint32_t return_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
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
  fprint_whex (asm_out_file, t->lf_procedure.return_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.calling_convention);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.attributes);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.num_parameters);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.arglist);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ARGLIST type.  This is just a list of other types.  LF_PROCEDURE
   entries point to one of these.  */

static void
write_lf_arglist (codeview_custom_type *t)
{
  /* This is lf_arglist in binutils and lfArgList in Microsoft's cvinfo.h:

    struct lf_arglist
    {
      uint16_t size;
      uint16_t kind;
      uint32_t num_entries;
      uint32_t args[];
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
  fprint_whex (asm_out_file, t->lf_arglist.num_entries);
  putc ('\n', asm_out_file);

  for (uint32_t i = 0; i < t->lf_arglist.num_entries; i++)
    {
      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_arglist.args[i]);
      putc ('\n', asm_out_file);
    }

  free (t->lf_arglist.args);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_FUNC_ID type, which marries together a function type with its
   name.  This will end up in the alternative types stream in the final PDB,
   but we can just stick it in the normal .debug$T section.  */

static void
write_lf_func_id (codeview_custom_type *t)
{
  size_t name_len;

  /* This is lf_func_id in binutils and lfFuncId in Microsoft's cvinfo.h:

    struct lf_func_id
    {
      uint16_t size;
      uint16_t kind;
      uint32_t parent_scope;
      uint32_t function_type;
      char name[];
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
  fprint_whex (asm_out_file, t->lf_func_id.parent_scope);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_func_id.function_type);
  putc ('\n', asm_out_file);

  name_len = strlen (t->lf_func_id.name) + 1;

  ASM_OUTPUT_ASCII (asm_out_file, t->lf_func_id.name, name_len);

  write_cv_padding (4 - (name_len % 4));

  free (t->lf_func_id.name);

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

	case LF_FIELDLIST:
	  write_lf_fieldlist (custom_types);
	  break;

	case LF_ENUM:
	  write_lf_enum (custom_types);
	  break;

	case LF_STRUCTURE:
	case LF_CLASS:
	  write_lf_structure (custom_types);
	  break;

	case LF_UNION:
	  write_lf_union (custom_types);
	  break;

	case LF_ARRAY:
	  write_lf_array (custom_types);
	  break;

	case LF_BITFIELD:
	  write_lf_bitfield (custom_types);
	  break;

	case LF_PROCEDURE:
	  write_lf_procedure (custom_types);
	  break;

	case LF_ARGLIST:
	  write_lf_arglist (custom_types);
	  break;

	case LF_FUNC_ID:
	  write_lf_func_id (custom_types);
	  break;

	default:
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
get_type_num_pointer_type (dw_die_ref type, bool in_struct)
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

  base_type_num = get_type_num (base_type, in_struct, false);
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

/* Process a DW_TAG_reference_type or DW_TAG_rvalue_reference_type DIE, add a
   new LF_POINTER type, and return its number.  */

static uint32_t
get_type_num_reference_type (dw_die_ref type, bool in_struct, bool rvref)
{
  uint32_t base_type_num, byte_size;
  dw_die_ref base_type;
  codeview_custom_type *ct;

  byte_size = get_AT_unsigned (type, DW_AT_byte_size);
  if (byte_size != 4 && byte_size != 8)
    return 0;

  base_type = get_AT_ref (type, DW_AT_type);

  base_type_num = get_type_num (base_type, in_struct, false);
  if (base_type_num == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_POINTER;
  ct->lf_pointer.base_type = base_type_num;
  ct->lf_pointer.attributes = rvref ? CV_PTR_MODE_RVREF : CV_PTR_MODE_LVREF;

  if (byte_size == 4)
    ct->lf_pointer.attributes |= CV_PTR_NEAR32;
  else
    ct->lf_pointer.attributes |= CV_PTR_64;

  ct->lf_pointer.attributes |= byte_size << 13;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_const_type DIE, adding an LF_MODIFIER type and returning
   its number.  */

static uint32_t
get_type_num_const_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type;
  uint32_t base_type_num;
  codeview_custom_type *ct;
  bool is_volatile = false;

  base_type = get_AT_ref (type, DW_AT_type);

  /* Handle case when this is a const volatile type - we only need one
     LF_MODIFIER for this.  */
  if (base_type && dw_get_die_tag (base_type) == DW_TAG_volatile_type)
    {
      is_volatile = true;

      base_type = get_AT_ref (base_type, DW_AT_type);
    }

  if (!base_type)
    {
      base_type_num = T_VOID;
    }
  else
    {
      base_type_num = get_type_num (base_type, in_struct, false);
      if (base_type_num == 0)
	return 0;
    }

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
get_type_num_volatile_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type;
  uint32_t base_type_num;
  codeview_custom_type *ct;

  base_type = get_AT_ref (type, DW_AT_type);

  if (base_type)
    {
      base_type_num = get_type_num (base_type, in_struct, false);
      if (base_type_num == 0)
	return 0;
    }
  else
    {
      base_type_num = T_VOID;
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MODIFIER;
  ct->lf_modifier.base_type = base_type_num;
  ct->lf_modifier.modifier = MOD_volatile;

  add_custom_type (ct);

  return ct->num;
}

/* Add a forward declaration for an enum.  This is legal from C++11 onwards.  */

static uint32_t
add_enum_forward_def (dw_die_ref type)
{
  codeview_custom_type *ct;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ENUM;

  ct->lf_enum.count = 0;
  ct->lf_enum.properties = CV_PROP_FWDREF;
  ct->lf_enum.underlying_type = get_type_num (get_AT_ref (type, DW_AT_type),
					      false, false);
  ct->lf_enum.fieldlist = 0;
  ct->lf_enum.name = xstrdup (get_AT_string (type, DW_AT_name));

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_enumeration_type DIE, adding an LF_FIELDLIST and an LF_ENUM
   type, returning the number of the latter.  */

static uint32_t
get_type_num_enumeration_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref first_child;
  codeview_custom_type *ct;
  uint16_t count = 0;
  uint32_t last_type;

  if (get_AT_flag (type, DW_AT_declaration))
    return add_enum_forward_def (type);

  /* First, add an LF_FIELDLIST for the enum's values.  We don't need to worry
     about deduplication here, as ld will take care of that for us.  If there's
     a lot of entries, add more LF_FIELDLISTs with LF_INDEXes pointing to
     the overflow lists.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FIELDLIST;
  ct->lf_fieldlist.length = 0;
  ct->lf_fieldlist.subtypes = NULL;
  ct->lf_fieldlist.last_subtype = NULL;

  if (first_child)
    {
      dw_die_ref c;

      c = first_child;
      do
	{
	  dw_attr_node *att;
	  codeview_subtype *el;
	  size_t el_len;

	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) != DW_TAG_enumerator)
	    continue;

	  att = get_AT (c, DW_AT_const_value);
	  if (!att)
	    continue;

	  el = (codeview_subtype *) xmalloc (sizeof (*el));
	  el->next = NULL;
	  el->kind = LF_ENUMERATE;

	  switch (AT_class (att))
	    {
	    case dw_val_class_unsigned_const:
	    case dw_val_class_unsigned_const_implicit:
	      el->lf_enumerate.value.neg = false;
	      el->lf_enumerate.value.num = att->dw_attr_val.v.val_unsigned;
	      break;

	    case dw_val_class_const:
	    case dw_val_class_const_implicit:
	      if (att->dw_attr_val.v.val_int < 0)
		{
		  el->lf_enumerate.value.neg = true;
		  el->lf_enumerate.value.num = -att->dw_attr_val.v.val_int;
		}
	      else
		{
		  el->lf_enumerate.value.neg = false;
		  el->lf_enumerate.value.num = att->dw_attr_val.v.val_int;
		}
	      break;

	    default:
	      free (el);
	      continue;
	    }

	  el->lf_enumerate.name = xstrdup (get_AT_string (c, DW_AT_name));

	  el_len = 7 + strlen (el->lf_enumerate.name);
	  el_len += cv_integer_len (&el->lf_enumerate.value);

	  if (el_len % 4)
	    el_len += 4 - (el_len % 4);

	  if (ct->lf_fieldlist.length + el_len > MAX_FIELDLIST_SIZE)
	    {
	      codeview_subtype *idx;
	      codeview_custom_type *ct2;

	      idx = (codeview_subtype *) xmalloc (sizeof (*idx));
	      idx->next = NULL;
	      idx->kind = LF_INDEX;
	      idx->lf_index.type_num = 0;

	      ct->lf_fieldlist.last_subtype->next = idx;
	      ct->lf_fieldlist.last_subtype = idx;

	      ct2 = (codeview_custom_type *)
		xmalloc (sizeof (codeview_custom_type));

	      ct2->next = ct;
	      ct2->kind = LF_FIELDLIST;
	      ct2->lf_fieldlist.length = 0;
	      ct2->lf_fieldlist.subtypes = NULL;
	      ct2->lf_fieldlist.last_subtype = NULL;

	      ct = ct2;
	    }

	  ct->lf_fieldlist.length += el_len;

	  if (ct->lf_fieldlist.last_subtype)
	    ct->lf_fieldlist.last_subtype->next = el;
	  else
	    ct->lf_fieldlist.subtypes = el;

	  ct->lf_fieldlist.last_subtype = el;
	  count++;
	}
      while (c != first_child);
    }

  while (ct)
    {
      codeview_custom_type *ct2;

      ct2 = ct->next;
      ct->next = NULL;

      if (ct->lf_fieldlist.last_subtype->kind == LF_INDEX)
	ct->lf_fieldlist.last_subtype->lf_index.type_num = last_type;

      add_custom_type (ct);
      last_type = ct->num;

      ct = ct2;
    }

  /* Now add an LF_ENUM, pointing to the LF_FIELDLIST we just added.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ENUM;
  ct->lf_enum.count = count;
  ct->lf_enum.properties = 0;
  ct->lf_enum.underlying_type = get_type_num (get_AT_ref (type, DW_AT_type),
					      in_struct, false);
  ct->lf_enum.fieldlist = last_type;
  ct->lf_enum.name = xstrdup (get_AT_string (type, DW_AT_name));

  add_custom_type (ct);

  return ct->num;
}

/* Add a DIE to our deferred_types list.  This happens when we have a struct
   with a pointer to a type that hasn't been defined yet, but which gets
   defined later on.  */

static void
add_deferred_type (dw_die_ref type)
{
  codeview_deferred_type *def;

  def = (codeview_deferred_type *) xmalloc (sizeof (codeview_deferred_type));

  def->next = NULL;
  def->type = type;

  if (!deferred_types)
    deferred_types = def;
  else
    last_deferred_type->next = def;

  last_deferred_type = def;
}

/* Flush the contents of our deferred_types list.  This happens after everything
   else has been written.  We call get_type_num to ensure that a type gets
   added to custom_types, if it hasn't been already.  */

static void
flush_deferred_types (void)
{
  while (deferred_types)
    {
      codeview_deferred_type *next;

      next = deferred_types->next;

      get_type_num (deferred_types->type, false, true);

      free (deferred_types);
      deferred_types = next;
    }

  last_deferred_type = NULL;
}

/* Add a forward definition for a struct, class, or union.  */

static uint32_t
add_struct_forward_def (dw_die_ref type)
{
  codeview_custom_type *ct;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_class_type:
      ct->kind = LF_CLASS;
      break;

    case DW_TAG_structure_type:
      ct->kind = LF_STRUCTURE;
      break;

    case DW_TAG_union_type:
      ct->kind = LF_UNION;
      break;

    default:
      break;
    }

  ct->lf_structure.num_members = 0;
  ct->lf_structure.properties = CV_PROP_FWDREF;
  ct->lf_structure.field_list = 0;
  ct->lf_structure.derived_from = 0;
  ct->lf_structure.vshape = 0;
  ct->lf_structure.length.neg = false;
  ct->lf_structure.length.num = 0;
  ct->lf_structure.name = xstrdup (get_AT_string (type, DW_AT_name));

  add_custom_type (ct);

  if (!get_AT_flag (type, DW_AT_declaration))
    add_deferred_type (type);

  return ct->num;
}

/* Add an LF_BITFIELD type, returning its number.  DWARF represents bitfields
   as members in a struct with a DW_AT_data_bit_offset attribute, whereas in
   CodeView they're a distinct type.  */

static uint32_t
create_bitfield (dw_die_ref c)
{
  codeview_custom_type *ct;
  uint32_t base_type;

  base_type = get_type_num (get_AT_ref (c, DW_AT_type), true, false);
  if (base_type == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_BITFIELD;
  ct->lf_bitfield.base_type = base_type;
  ct->lf_bitfield.length = get_AT_unsigned (c, DW_AT_bit_size);
  ct->lf_bitfield.position = get_AT_unsigned (c, DW_AT_data_bit_offset);

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_structure_type, DW_TAG_class_type, or DW_TAG_union_type
   DIE, add an LF_FIELDLIST and an LF_STRUCTURE / LF_CLASS / LF_UNION type,
   and return the number of the latter.  */

static uint32_t
get_type_num_struct (dw_die_ref type, bool in_struct, bool *is_fwd_ref)
{
  dw_die_ref first_child;
  codeview_custom_type *ct;
  uint16_t num_members = 0;
  uint32_t last_type;
  const char *name;

  if ((in_struct && get_AT_string (type, DW_AT_name))
      || get_AT_flag (type, DW_AT_declaration))
    {
      *is_fwd_ref = true;
      return add_struct_forward_def (type);
    }

  *is_fwd_ref = false;

  /* First, add an LF_FIELDLIST for the structure's members.  We don't need to
     worry about deduplication here, as ld will take care of that for us.
     If there's a lot of entries, add more LF_FIELDLISTs with LF_INDEXes
     pointing to the overflow lists.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FIELDLIST;
  ct->lf_fieldlist.length = 0;
  ct->lf_fieldlist.subtypes = NULL;
  ct->lf_fieldlist.last_subtype = NULL;

  if (first_child)
    {
      dw_die_ref c;

      c = first_child;
      do
	{
	  codeview_subtype *el;
	  size_t el_len;

	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) != DW_TAG_member)
	    continue;

	  el = (codeview_subtype *) xmalloc (sizeof (*el));
	  el->next = NULL;
	  el->kind = LF_MEMBER;

	  switch (get_AT_unsigned (c, DW_AT_accessibility))
	    {
	    case DW_ACCESS_private:
	      el->lf_member.attributes = CV_ACCESS_PRIVATE;
	      break;

	    case DW_ACCESS_protected:
	      el->lf_member.attributes = CV_ACCESS_PROTECTED;
	      break;

	    case DW_ACCESS_public:
	      el->lf_member.attributes = CV_ACCESS_PUBLIC;
	      break;

	    /* Members in a C++ struct or union are public by default, members
	      in a class are private.  */
	    default:
	      if (dw_get_die_tag (type) == DW_TAG_class_type)
		el->lf_member.attributes = CV_ACCESS_PRIVATE;
	      else
		el->lf_member.attributes = CV_ACCESS_PUBLIC;
	      break;
	    }

	  if (get_AT (c, DW_AT_data_bit_offset))
	    el->lf_member.type = create_bitfield (c);
	  else
	    el->lf_member.type = get_type_num (get_AT_ref (c, DW_AT_type),
					       true, false);

	  el->lf_member.offset.neg = false;
	  el->lf_member.offset.num = get_AT_unsigned (c,
						      DW_AT_data_member_location);

	  el_len = 11;
	  el_len += cv_integer_len (&el->lf_member.offset);

	  if (get_AT_string (c, DW_AT_name))
	    {
	      el->lf_member.name = xstrdup (get_AT_string (c, DW_AT_name));
	      el_len += strlen (el->lf_member.name);
	    }
	  else
	    {
	      el->lf_member.name = NULL;
	    }

	  if (el_len % 4)
	    el_len += 4 - (el_len % 4);

	  /* Add an LF_INDEX subtype if everything's too big for one
	     LF_FIELDLIST.  */

	  if (ct->lf_fieldlist.length + el_len > MAX_FIELDLIST_SIZE)
	    {
	      codeview_subtype *idx;
	      codeview_custom_type *ct2;

	      idx = (codeview_subtype *) xmalloc (sizeof (*idx));
	      idx->next = NULL;
	      idx->kind = LF_INDEX;
	      idx->lf_index.type_num = 0;

	      ct->lf_fieldlist.last_subtype->next = idx;
	      ct->lf_fieldlist.last_subtype = idx;

	      ct2 = (codeview_custom_type *)
		xmalloc (sizeof (codeview_custom_type));

	      ct2->next = ct;
	      ct2->kind = LF_FIELDLIST;
	      ct2->lf_fieldlist.length = 0;
	      ct2->lf_fieldlist.subtypes = NULL;
	      ct2->lf_fieldlist.last_subtype = NULL;

	      ct = ct2;
	    }

	  ct->lf_fieldlist.length += el_len;

	  if (ct->lf_fieldlist.last_subtype)
	    ct->lf_fieldlist.last_subtype->next = el;
	  else
	    ct->lf_fieldlist.subtypes = el;

	  ct->lf_fieldlist.last_subtype = el;
	  num_members++;
	}
      while (c != first_child);
    }

  while (ct)
    {
      codeview_custom_type *ct2;

      ct2 = ct->next;
      ct->next = NULL;

      if (ct->lf_fieldlist.last_subtype
	  && ct->lf_fieldlist.last_subtype->kind == LF_INDEX)
	{
	  ct->lf_fieldlist.last_subtype->lf_index.type_num = last_type;
	}

      add_custom_type (ct);
      last_type = ct->num;

      ct = ct2;
    }

  /* Now add an LF_STRUCTURE / LF_CLASS / LF_UNION, pointing to the
     LF_FIELDLIST we just added.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_class_type:
      ct->kind = LF_CLASS;
      break;

    case DW_TAG_structure_type:
      ct->kind = LF_STRUCTURE;
      break;

    case DW_TAG_union_type:
      ct->kind = LF_UNION;
      break;

    default:
      break;
    }

  ct->lf_structure.num_members = num_members;
  ct->lf_structure.properties = 0;
  ct->lf_structure.field_list = last_type;
  ct->lf_structure.derived_from = 0;
  ct->lf_structure.vshape = 0;
  ct->lf_structure.length.neg = false;
  ct->lf_structure.length.num = get_AT_unsigned (type, DW_AT_byte_size);

  name = get_AT_string (type, DW_AT_name);

  if (name)
    ct->lf_structure.name = xstrdup (name);
  else
    ct->lf_structure.name = NULL;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_subroutine_type DIE, adding an LF_ARGLIST and an
   LF_PROCEDURE type, and returning the number of the latter.  */

static uint32_t
get_type_num_subroutine_type (dw_die_ref type, bool in_struct)
{
  codeview_custom_type *ct;
  uint32_t return_type, arglist_type;
  uint16_t num_args;
  dw_die_ref first_child;

  /* Find the return type.  */

  if (get_AT_ref (type, DW_AT_type))
    {
      return_type = get_type_num (get_AT_ref (type, DW_AT_type), in_struct,
				  false);
      if (return_type == 0)
	return 0;
    }
  else
    {
      return_type = T_VOID;
    }

  /* Count the arguments.  */

  first_child = dw_get_die_child (type);
  num_args = 0;

  if (first_child)
    {
      dw_die_ref c;

      c = first_child;
      do
	{
	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) != DW_TAG_formal_parameter
	      && dw_get_die_tag (c) != DW_TAG_unspecified_parameters)
	    continue;

	  num_args++;
	}
      while (c != first_child);
    }

  /* Create an LF_ARGLIST for the arguments.  If this is a duplicate, ld
     will take care of this for us.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ARGLIST;
  ct->lf_arglist.num_entries = num_args;

  if (num_args > 0)
    {
      dw_die_ref c;
      uint32_t *argptr;

      ct->lf_arglist.args = (uint32_t *) xmalloc (sizeof (uint32_t) * num_args);
      argptr = ct->lf_arglist.args;

      c = first_child;
      do
	{
	  c = dw_get_die_sib (c);

	  switch (dw_get_die_tag (c))
	    {
	    case DW_TAG_formal_parameter:
	      *argptr = get_type_num (get_AT_ref (c, DW_AT_type), in_struct,
				      false);
	      argptr++;
	      break;

	    case DW_TAG_unspecified_parameters:
	      *argptr = 0;
	      argptr++;
	      break;

	    default:
	      break;
	    }
	}
      while (c != first_child);
    }
  else
    {
      ct->lf_arglist.args = NULL;
    }

  add_custom_type (ct);

  arglist_type = ct->num;

  /* Finally, create an LF_PROCEDURE.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_PROCEDURE;
  ct->lf_procedure.return_type = return_type;
  ct->lf_procedure.calling_convention = 0;
  ct->lf_procedure.attributes = 0;
  ct->lf_procedure.num_parameters = num_args;
  ct->lf_procedure.arglist = arglist_type;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_array_type DIE, adding an LF_ARRAY type and returning its
   number.  */

static uint32_t
get_type_num_array_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type, t, first_child, c, *dimension_arr;
  uint64_t size = 0;
  unsigned int dimensions, i;
  uint32_t element_type;

  base_type = get_AT_ref (type, DW_AT_type);
  if (!base_type)
    return 0;

  /* We need to know the size of our base type.  Loop through until we find
     it.  */
  t = base_type;
  while (t && size == 0)
    {
      switch (dw_get_die_tag (t))
	{
	case DW_TAG_const_type:
	case DW_TAG_volatile_type:
	case DW_TAG_typedef:
	case DW_TAG_enumeration_type:
	  t = get_AT_ref (t, DW_AT_type);
	  break;

	case DW_TAG_base_type:
	case DW_TAG_structure_type:
	case DW_TAG_class_type:
	case DW_TAG_union_type:
	case DW_TAG_pointer_type:
	case DW_TAG_reference_type:
	case DW_TAG_rvalue_reference_type:
	  size = get_AT_unsigned (t, DW_AT_byte_size);
	  break;

	default:
	  return 0;
	}
    }

  if (size == 0)
    return 0;

  first_child = dw_get_die_child (type);
  if (!first_child)
    return 0;

  element_type = get_type_num (base_type, in_struct, false);
  if (element_type == 0)
    return 0;

  /* Create an array of our DW_TAG_subrange_type children, in reverse order.
     We have to do this because unlike DWARF CodeView doesn't have
     multidimensional arrays, so instead we do arrays of arrays.  */

  dimensions = 0;
  c = first_child;
  do
    {
      c = dw_get_die_sib (c);
      if (dw_get_die_tag (c) != DW_TAG_subrange_type)
	continue;

      dimensions++;
    }
  while (c != first_child);

  if (dimensions == 0)
    return 0;

  dimension_arr = (dw_die_ref *) xmalloc (sizeof (dw_die_ref) * dimensions);

  c = first_child;
  i = 0;
  do
    {
      c = dw_get_die_sib (c);
      if (dw_get_die_tag (c) != DW_TAG_subrange_type)
	continue;

      dimension_arr[dimensions - i - 1] = c;
      i++;
    }
  while (c != first_child);

  /* Record an LF_ARRAY entry for each array dimension.  If this leads to
     duplicate types, ld will take care of it for us.  */

  for (i = 0; i < dimensions; i++)
    {
      codeview_custom_type *ct;
      dw_die_ref index;

      ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

      size *= get_AT_unsigned (dimension_arr[i], DW_AT_upper_bound) + 1;

      index = get_AT_ref (dimension_arr[i], DW_AT_type);

      ct->next = NULL;
      ct->kind = LF_ARRAY;
      ct->lf_array.element_type = element_type;
      ct->lf_array.index_type = get_type_num (index, in_struct, false);
      ct->lf_array.length_in_bytes.neg = false;
      ct->lf_array.length_in_bytes.num = size;

      add_custom_type (ct);

      element_type = ct->num;
    }

  free (dimension_arr);

  return element_type;
}

/* Process a DIE representing a type definition, add a CodeView type if
   necessary, and return its number.  If it's something we can't handle, return
   0.  We keep a hash table so that we're not adding the same type multiple
   times - though if we do it's not disastrous, as ld will deduplicate
   everything for us.  */

static uint32_t
get_type_num (dw_die_ref type, bool in_struct, bool no_fwd_ref)
{
  codeview_type **slot, *t;
  uint32_t num;
  bool is_fwd_ref;

  if (!type)
    return 0;

  if (!types_htab)
    types_htab = new hash_table<die_hasher> (10);

  slot = types_htab->find_slot_with_hash (type, htab_hash_pointer (type),
					  NO_INSERT);

  if (slot && *slot && (!no_fwd_ref || !(*slot)->is_fwd_ref))
    return (*slot)->num;

  is_fwd_ref = false;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_base_type:
      num = get_type_num_base_type (type);
      break;

    case DW_TAG_typedef:
      /* FIXME - signed longs typedef'd as "HRESULT" should get their
		 own type (T_HRESULT) */
      num = get_type_num (get_AT_ref (type, DW_AT_type), in_struct, false);
      break;

    case DW_TAG_pointer_type:
      num = get_type_num_pointer_type (type, in_struct);
      break;

    case DW_TAG_reference_type:
      num = get_type_num_reference_type (type, in_struct, false);
      break;

    case DW_TAG_rvalue_reference_type:
      num = get_type_num_reference_type (type, in_struct, true);
      break;

    case DW_TAG_const_type:
      num = get_type_num_const_type (type, in_struct);
      break;

    case DW_TAG_volatile_type:
      num = get_type_num_volatile_type (type, in_struct);
      break;

    case DW_TAG_enumeration_type:
      num = get_type_num_enumeration_type (type, in_struct);
      break;

    case DW_TAG_structure_type:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
      num = get_type_num_struct (type, in_struct, &is_fwd_ref);
      break;

    case DW_TAG_array_type:
      num = get_type_num_array_type (type, in_struct);
      break;

    case DW_TAG_subroutine_type:
      num = get_type_num_subroutine_type (type, in_struct);
      break;

    default:
      num = 0;
      break;
    }

  /* Check hash table again, and account for the fact that self-referential
     structs will have created a forward reference to themselves.  */

  slot = types_htab->find_slot_with_hash (type, htab_hash_pointer (type),
					  INSERT);

  if (*slot && (*slot)->is_fwd_ref && !is_fwd_ref)
    {
      (*slot)->num = num;
      (*slot)->is_fwd_ref = false;
      return num;
    }

  t = (codeview_type *) xmalloc (sizeof (codeview_type));
  t->die = type;
  t->num = num;
  t->is_fwd_ref = is_fwd_ref;

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
  s->data_symbol.type = get_type_num (get_AT_ref (die, DW_AT_type), false,
				      false);
  s->data_symbol.name = xstrdup (name);
  s->data_symbol.die = die;

  if (last_sym)
    last_sym->next = s;
  else
    sym = s;

  last_sym = s;
}

/* Process a DW_TAG_subprogram DIE, and add an S_GPROC32_ID or S_LPROC32_ID
   symbol for this.  */

static void
add_function (dw_die_ref die)
{
  codeview_custom_type *ct;
  const char *name = get_AT_string (die, DW_AT_name);
  uint32_t function_type, func_id_type;
  codeview_symbol *s;

  if (!name)
    return;

  /* Add an LF_FUNC_ID type for this function.  */

  function_type = get_type_num_subroutine_type (die, false);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FUNC_ID;
  ct->lf_func_id.parent_scope = 0;
  ct->lf_func_id.function_type = function_type;
  ct->lf_func_id.name = xstrdup (name);

  add_custom_type (ct);

  func_id_type = ct->num;

  /* Add an S_GPROC32_ID / S_LPROC32_ID symbol.  */

  s = (codeview_symbol *) xmalloc (sizeof (codeview_symbol));

  s->next = NULL;
  s->kind = get_AT (die, DW_AT_external) ? S_GPROC32_ID : S_LPROC32_ID;
  s->function.parent = 0;
  s->function.end = 0;
  s->function.next = 0;
  s->function.type = func_id_type;
  s->function.flags = 0;
  s->function.name = xstrdup (name);
  s->function.die = die;

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
      switch (dw_get_die_tag (c))
	{
	case DW_TAG_variable:
	  add_variable (c);
	  break;
	case DW_TAG_subprogram:
	  add_function (c);
	  break;
	default:
	  break;
	}

      c = dw_get_die_sib (c);
    }
  while (c != first_child);

  flush_deferred_types ();
}

#endif
