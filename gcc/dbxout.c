/* Output dbx-format symbol table information from GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Output dbx-format symbol table data.
   This consists of many symbol table entries, each of them
   a .stabs assembler pseudo-op with four operands:
   a "name" which is really a description of one symbol and its type,
   a "code", which is a symbol defined in stab.h whose name starts with N_,
   an unused operand always 0,
   and a "value" which is an address or an offset.
   The name is enclosed in doublequote characters.

   Each function, variable, typedef, and structure tag
   has a symbol table entry to define it.
   The beginning and end of each level of name scoping within
   a function are also marked by special symbol table entries.

   The "name" consists of the symbol name, a colon, a kind-of-symbol letter,
   and a data type number.  The data type number may be followed by
   "=" and a type definition; normally this will happen the first time
   the type number is mentioned.  The type definition may refer to
   other types by number, and those type numbers may be followed
   by "=" and nested definitions.

   This can make the "name" quite long.
   When a name is more than 80 characters, we split the .stabs pseudo-op
   into two .stabs pseudo-ops, both sharing the same "code" and "value".
   The first one is marked as continued with a double-backslash at the
   end of its "name".

   The kind-of-symbol letter distinguished function names from global
   variables from file-scope variables from parameters from auto
   variables in memory from typedef names from register variables.
   See `dbxout_symbol'.

   The "code" is mostly redundant with the kind-of-symbol letter
   that goes in the "name", but not entirely: for symbols located
   in static storage, the "code" says which segment the address is in,
   which controls how it is relocated.

   The "value" for a symbol in static storage
   is the core address of the symbol (actually, the assembler
   label for the symbol).  For a symbol located in a stack slot
   it is the stack offset; for one in a register, the register number.
   For a typedef symbol, it is zero.

   If DEBUG_SYMS_TEXT is defined, all debugging symbols must be
   output while in the text section.

   For more on data type definitions, see `dbxout_type'.  */

#include "config.h"
#include "system.h"

#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "regs.h"
#include "insn-config.h"
#include "reload.h"
#include "defaults.h"
#include "output.h" /* ASM_OUTPUT_SOURCE_LINE may refer to sdb functions.  */
#include "dbxout.h"
#include "toplev.h"
#include "tm_p.h"
#include "ggc.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

#ifndef ASM_STABS_OP
#define ASM_STABS_OP ".stabs"
#endif

#ifndef ASM_STABN_OP
#define ASM_STABN_OP ".stabn"
#endif

#ifndef DBX_TYPE_DECL_STABS_CODE
#define DBX_TYPE_DECL_STABS_CODE N_LSYM
#endif

#ifndef DBX_STATIC_CONST_VAR_CODE
#define DBX_STATIC_CONST_VAR_CODE N_FUN
#endif

#ifndef DBX_REGPARM_STABS_CODE
#define DBX_REGPARM_STABS_CODE N_RSYM
#endif

#ifndef DBX_REGPARM_STABS_LETTER
#define DBX_REGPARM_STABS_LETTER 'P'
#endif

/* This is used for parameters passed by invisible reference in a register.  */
#ifndef GDB_INV_REF_REGPARM_STABS_LETTER
#define GDB_INV_REF_REGPARM_STABS_LETTER 'a'
#endif

#ifndef DBX_MEMPARM_STABS_LETTER
#define DBX_MEMPARM_STABS_LETTER 'p'
#endif

#ifndef FILE_NAME_JOINER
#define FILE_NAME_JOINER "/"
#endif

/* Nonzero means if the type has methods, only output debugging
   information if methods are actually written to the asm file.  This
   optimization only works if the debugger can detect the special C++
   marker.  */

#define MINIMAL_DEBUG 1

#ifdef NO_DOLLAR_IN_LABEL
#ifdef NO_DOT_IN_LABEL
#undef MINIMAL_DEBUG
#define MINIMAL_DEBUG 0
#endif
#endif

/* Typical USG systems don't have stab.h, and they also have
   no use for DBX-format debugging info.  */

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)

static int flag_minimal_debug = MINIMAL_DEBUG;

/* Nonzero if we have actually used any of the GDB extensions
   to the debugging format.  The idea is that we use them for the
   first time only if there's a strong reason, but once we have done that,
   we use them whenever convenient.  */

static int have_used_extensions = 0;

/* Number for the next N_SOL filename stabs label.  The number 0 is reserved
   for the N_SO filename stabs label.  */

static int source_label_number = 1;

#ifdef DEBUG_SYMS_TEXT
#define FORCE_TEXT text_section ();
#else
#define FORCE_TEXT
#endif

/* If there is a system stab.h, use it.  Otherwise, use our own.  */
/* ??? This is supposed to describe the target's stab format, so using
   the host HAVE_STAB_H appears to be wrong.  For now, we use our own file
   when cross compiling.  */
#if defined (USG) || !defined (HAVE_STAB_H) || defined (CROSS_COMPILE)
#include "gstab.h" /* If doing DBX on sysV, use our own stab.h.  */
#else
#include <stab.h>

/* This is a GNU extension we need to reference in this file.  */
#ifndef N_CATCH
#define N_CATCH 0x54
#endif
#endif

#ifdef __GNU_STAB__
#define STAB_CODE_TYPE enum __stab_debug_code
#else
#define STAB_CODE_TYPE int
#endif

/* 1 if PARM is passed to this function in memory.  */

#define PARM_PASSED_IN_MEMORY(PARM) \
 (GET_CODE (DECL_INCOMING_RTL (PARM)) == MEM)

/* A C expression for the integer offset value of an automatic variable
   (N_LSYM) having address X (an RTX).  */
#ifndef DEBUGGER_AUTO_OFFSET
#define DEBUGGER_AUTO_OFFSET(X) \
  (GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0)
#endif

/* A C expression for the integer offset value of an argument (N_PSYM)
   having address X (an RTX).  The nominal offset is OFFSET.  */
#ifndef DEBUGGER_ARG_OFFSET
#define DEBUGGER_ARG_OFFSET(OFFSET, X) (OFFSET)
#endif

/* Stream for writing to assembler file.  */

static FILE *asmfile;

/* Last source file name mentioned in a NOTE insn.  */

static const char *lastfile;

/* Current working directory.  */

static const char *cwd;

enum typestatus {TYPE_UNSEEN, TYPE_XREF, TYPE_DEFINED};

/* Structure recording information about a C data type.
   The status element says whether we have yet output
   the definition of the type.  TYPE_XREF says we have
   output it as a cross-reference only.
   The file_number and type_number elements are used if DBX_USE_BINCL
   is defined.  */

struct typeinfo
{
  enum typestatus status;
#ifdef DBX_USE_BINCL
  int file_number;
  int type_number;
#endif
};

/* Vector recording information about C data types.
   When we first notice a data type (a tree node),
   we assign it a number using next_type_number.
   That is its index in this vector.  */

struct typeinfo *typevec;

/* Number of elements of space allocated in `typevec'.  */

static int typevec_len;

/* In dbx output, each type gets a unique number.
   This is the number for the next type output.
   The number, once assigned, is in the TYPE_SYMTAB_ADDRESS field.  */

static int next_type_number;

#ifdef DBX_USE_BINCL

/* When using N_BINCL in dbx output, each type number is actually a
   pair of the file number and the type number within the file.
   This is a stack of input files.  */

struct dbx_file
{
  struct dbx_file *next;
  int file_number;
  int next_type_number;
};

/* This is the top of the stack.  */

static struct dbx_file *current_file;

/* This is the next file number to use.  */

static int next_file_number;

#endif /* DBX_USE_BINCL */

/* These variables are for dbxout_symbol to communicate to
   dbxout_finish_symbol.
   current_sym_code is the symbol-type-code, a symbol N_... define in stab.h.
   current_sym_value and current_sym_addr are two ways to address the
   value to store in the symtab entry.
   current_sym_addr if nonzero represents the value as an rtx.
   If that is zero, current_sym_value is used.  This is used
   when the value is an offset (such as for auto variables,
   register variables and parms).  */

static STAB_CODE_TYPE current_sym_code;
static int current_sym_value;
static rtx current_sym_addr;

/* Number of chars of symbol-description generated so far for the
   current symbol.  Used by CHARS and CONTIN.  */

static int current_sym_nchars;

/* Report having output N chars of the current symbol-description.  */

#define CHARS(N) (current_sym_nchars += (N))

/* Break the current symbol-description, generating a continuation,
   if it has become long.  */

#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 80
#endif

#if DBX_CONTIN_LENGTH > 0
#define CONTIN  \
  do {if (current_sym_nchars > DBX_CONTIN_LENGTH) dbxout_continue ();} while (0)
#else
#define CONTIN
#endif

#if defined(ASM_OUTPUT_SECTION_NAME)
static void dbxout_function_end		PARAMS ((void));
#endif
static void dbxout_typedefs		PARAMS ((tree));
static void dbxout_type_index		PARAMS ((tree));
#if DBX_CONTIN_LENGTH > 0
static void dbxout_continue		PARAMS ((void));
#endif
static void dbxout_type_fields		PARAMS ((tree));
static void dbxout_type_method_1	PARAMS ((tree, const char *));
static void dbxout_type_methods		PARAMS ((tree));
static void dbxout_range_type		PARAMS ((tree));
static void dbxout_type			PARAMS ((tree, int, int));
static void print_int_cst_octal		PARAMS ((tree));
static void print_octal			PARAMS ((unsigned HOST_WIDE_INT, int));
static void dbxout_type_name		PARAMS ((tree));
static int dbxout_symbol_location	PARAMS ((tree, tree, const char *, rtx));
static void dbxout_symbol_name		PARAMS ((tree, const char *, int));
static void dbxout_prepare_symbol	PARAMS ((tree));
static void dbxout_finish_symbol	PARAMS ((tree));
static void dbxout_block		PARAMS ((tree, int, tree));
static void dbxout_really_begin_function PARAMS ((tree));

#if defined(ASM_OUTPUT_SECTION_NAME)
static void
dbxout_function_end ()
{
  static int scope_labelno = 0;
  char lscope_label_name[100];
  /* Convert Ltext into the appropriate format for local labels in case
     the system doesn't insert underscores in front of user generated
     labels.  */
  ASM_GENERATE_INTERNAL_LABEL (lscope_label_name, "Lscope", scope_labelno);
  ASM_OUTPUT_INTERNAL_LABEL (asmfile, "Lscope", scope_labelno);
  scope_labelno++;

  /* By convention, GCC will mark the end of a function with an N_FUN
     symbol and an empty string.  */
  fprintf (asmfile, "%s \"\",%d,0,0,", ASM_STABS_OP, N_FUN);
  assemble_name (asmfile, lscope_label_name);
  fputc ('-', asmfile);
  assemble_name (asmfile, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));
  fprintf (asmfile, "\n");
}
#endif /* ! NO_DBX_FUNCTION_END */

/* At the beginning of compilation, start writing the symbol table.
   Initialize `typevec' and output the standard data types of C.  */

void
dbxout_init (asm_file, input_file_name, syms)
     FILE *asm_file;
     const char *input_file_name;
     tree syms;
{
  char ltext_label_name[100];

  asmfile = asm_file;

  typevec_len = 100;
  typevec = (struct typeinfo *) xcalloc (typevec_len, sizeof typevec[0]);

  /* Convert Ltext into the appropriate format for local labels in case
     the system doesn't insert underscores in front of user generated
     labels.  */
  ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext", 0);

  /* Put the current working directory in an N_SO symbol.  */
#ifndef DBX_WORKING_DIRECTORY /* Only some versions of DBX want this,
				 but GDB always does.  */
  if (use_gnu_debug_info_extensions)
#endif
    {
      if (!cwd && (cwd = getpwd ()) && (!*cwd || cwd[strlen (cwd) - 1] != '/'))
	{
	  char *wdslash = xmalloc (strlen (cwd) + sizeof (FILE_NAME_JOINER));
	  sprintf (wdslash, "%s%s", cwd, FILE_NAME_JOINER);
	  cwd = wdslash;
	}
      if (cwd)
	{
#ifdef DBX_OUTPUT_MAIN_SOURCE_DIRECTORY
	  DBX_OUTPUT_MAIN_SOURCE_DIRECTORY (asmfile, cwd);
#else /* no DBX_OUTPUT_MAIN_SOURCE_DIRECTORY */
	  fprintf (asmfile, "%s ", ASM_STABS_OP);
	  output_quoted_string (asmfile, cwd);
	  fprintf (asmfile, ",%d,0,0,%s\n", N_SO, &ltext_label_name[1]);
#endif /* no DBX_OUTPUT_MAIN_SOURCE_DIRECTORY */
	}
    }

#ifdef DBX_OUTPUT_MAIN_SOURCE_FILENAME
  /* This should NOT be DBX_OUTPUT_SOURCE_FILENAME. That
     would give us an N_SOL, and we want an N_SO.  */
  DBX_OUTPUT_MAIN_SOURCE_FILENAME (asmfile, input_file_name);
#else /* no DBX_OUTPUT_MAIN_SOURCE_FILENAME */
  /* We include outputting `Ltext:' here,
     because that gives you a way to override it.  */
  /* Used to put `Ltext:' before the reference, but that loses on sun 4.  */
  fprintf (asmfile, "%s ", ASM_STABS_OP);
  output_quoted_string (asmfile, input_file_name);
  fprintf (asmfile, ",%d,0,0,%s\n", 
	   N_SO, &ltext_label_name[1]);
  text_section ();
  ASM_OUTPUT_INTERNAL_LABEL (asmfile, "Ltext", 0);
#endif /* no DBX_OUTPUT_MAIN_SOURCE_FILENAME */

  /* Possibly output something to inform GDB that this compilation was by
     GCC.  It's easier for GDB to parse it when after the N_SO's.  This
     is used in Solaris 2.  */
#ifdef ASM_IDENTIFY_GCC_AFTER_SOURCE
  ASM_IDENTIFY_GCC_AFTER_SOURCE (asmfile);
#endif

  lastfile = input_file_name;

  next_type_number = 1;

#ifdef DBX_USE_BINCL
  current_file = (struct dbx_file *) xmalloc (sizeof *current_file);
  current_file->next = NULL;
  current_file->file_number = 0;
  current_file->next_type_number = 1;
  next_file_number = 1;
#endif

  /* Make sure that types `int' and `char' have numbers 1 and 2.
     Definitions of other integer types will refer to those numbers.
     (Actually it should no longer matter what their numbers are.
     Also, if any types with tags have been defined, dbxout_symbol
     will output them first, so the numbers won't be 1 and 2.  That
     happens in C++.  So it's a good thing it should no longer matter).  */

#ifdef DBX_OUTPUT_STANDARD_TYPES
  DBX_OUTPUT_STANDARD_TYPES (syms);
#else
  dbxout_symbol (TYPE_NAME (integer_type_node), 0);
  dbxout_symbol (TYPE_NAME (char_type_node), 0);
#endif

  /* Get all permanent types that have typedef names,
     and output them all, except for those already output.  */

  dbxout_typedefs (syms);

  ggc_add_string_root ((char **) &lastfile, 1);
}

/* Output any typedef names for types described by TYPE_DECLs in SYMS,
   in the reverse order from that which is found in SYMS.  */

static void
dbxout_typedefs (syms)
     tree syms;
{
  if (syms)
    {
      dbxout_typedefs (TREE_CHAIN (syms));
      if (TREE_CODE (syms) == TYPE_DECL)
	{
	  tree type = TREE_TYPE (syms);
	  if (TYPE_NAME (type)
	      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	      && TYPE_SIZE (type) != NULL_TREE
	      && ! TREE_ASM_WRITTEN (TYPE_NAME (type)))
	    dbxout_symbol (TYPE_NAME (type), 0);
	}
    }
}

/* Change to reading from a new source file.  Generate a N_BINCL stab.  */

void
dbxout_start_new_source_file (filename)
     const char *filename ATTRIBUTE_UNUSED;
{
#ifdef DBX_USE_BINCL
  struct dbx_file *n = (struct dbx_file *) xmalloc (sizeof *n);

  n->next = current_file;
  n->file_number = next_file_number++;
  n->next_type_number = 1;
  current_file = n;
  fprintf (asmfile, "%s ", ASM_STABS_OP);
  output_quoted_string (asmfile, filename);
  fprintf (asmfile, ",%d,0,0,0\n", N_BINCL);
#endif
}

/* Revert to reading a previous source file.  Generate a N_EINCL stab.  */

void
dbxout_resume_previous_source_file ()
{
#ifdef DBX_USE_BINCL
  struct dbx_file *next;

  fprintf (asmfile, "%s %d,0,0,0\n", ASM_STABN_OP, N_EINCL);
  next = current_file->next;
  free (current_file);
  current_file = next;
#endif
}

/* Output debugging info to FILE to switch to sourcefile FILENAME.  */

void
dbxout_source_file (file, filename)
     FILE *file;
     const char *filename;
{
  char ltext_label_name[100];

  if (filename && (lastfile == 0 || strcmp (filename, lastfile)))
    {
#ifdef DBX_OUTPUT_SOURCE_FILENAME
      DBX_OUTPUT_SOURCE_FILENAME (file, filename);
#else
      ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext",
				   source_label_number);
      fprintf (file, "%s ", ASM_STABS_OP);
      output_quoted_string (file, filename);
      fprintf (file, ",%d,0,0,%s\n", N_SOL, &ltext_label_name[1]);
      if (current_function_decl != NULL_TREE
	  && DECL_SECTION_NAME (current_function_decl) != NULL_TREE)
	; /* Don't change section amid function.  */
      else
	text_section ();
      ASM_OUTPUT_INTERNAL_LABEL (file, "Ltext", source_label_number);
      source_label_number++;
#endif
      lastfile = filename;
    }
}

/* Output a line number symbol entry into output stream FILE, 
   for source file FILENAME and line number LINENO.  */

void
dbxout_source_line (file, filename, lineno)
     FILE *file;
     const char *filename;
     int lineno;
{
  dbxout_source_file (file, filename);

#ifdef ASM_OUTPUT_SOURCE_LINE
  ASM_OUTPUT_SOURCE_LINE (file, lineno);
#else
  fprintf (file, "\t%s %d,0,%d\n", ASM_STABD_OP, N_SLINE, lineno);
#endif
}

/* At the end of compilation, finish writing the symbol table.
   Unless you define DBX_OUTPUT_MAIN_SOURCE_FILE_END, the default is
   to do nothing.  */

void
dbxout_finish (file, filename)
     FILE *file ATTRIBUTE_UNUSED;
     const char *filename ATTRIBUTE_UNUSED;
{
#ifdef DBX_OUTPUT_MAIN_SOURCE_FILE_END
  DBX_OUTPUT_MAIN_SOURCE_FILE_END (file, filename);
#endif /* DBX_OUTPUT_MAIN_SOURCE_FILE_END */
}

/* Output the index of a type.  */

static void
dbxout_type_index (type)
     tree type;
{
#ifndef DBX_USE_BINCL
  fprintf (asmfile, "%d", TYPE_SYMTAB_ADDRESS (type));
  CHARS (3);
#else
  struct typeinfo *t = &typevec[TYPE_SYMTAB_ADDRESS (type)];
  fprintf (asmfile, "(%d,%d)", t->file_number, t->type_number);
  CHARS (7);
#endif
}

#if DBX_CONTIN_LENGTH > 0
/* Continue a symbol-description that gets too big.
   End one symbol table entry with a double-backslash
   and start a new one, eventually producing something like
   .stabs "start......\\",code,0,value
   .stabs "...rest",code,0,value   */

static void
dbxout_continue ()
{
#ifdef DBX_CONTIN_CHAR
  fprintf (asmfile, "%c", DBX_CONTIN_CHAR);
#else
  fprintf (asmfile, "\\\\");
#endif
  dbxout_finish_symbol (NULL_TREE);
  fprintf (asmfile, "%s \"", ASM_STABS_OP);
  current_sym_nchars = 0;
}
#endif /* DBX_CONTIN_LENGTH > 0 */

/* Subroutine of `dbxout_type'.  Output the type fields of TYPE.
   This must be a separate function because anonymous unions require
   recursive calls.  */

static void
dbxout_type_fields (type)
     tree type;
{
  tree tem;
  /* Output the name, type, position (in bits), size (in bits) of each
     field.  */
  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
    {
      /* Omit here local type decls until we know how to support them.  */
      if (TREE_CODE (tem) == TYPE_DECL)
	continue;
      /* Omit fields whose position or size are variable.  */
      else if (TREE_CODE (tem) == FIELD_DECL
	       && (TREE_CODE (DECL_FIELD_BITPOS (tem)) != INTEGER_CST
		   || TREE_CODE (DECL_SIZE (tem)) != INTEGER_CST))
	continue;
      /* Omit here the nameless fields that are used to skip bits.  */
      else if (DECL_IGNORED_P (tem))
	continue;
      else if (TREE_CODE (tem) != CONST_DECL)
	{
	  /* Continue the line if necessary,
	     but not before the first field.  */
	  if (tem != TYPE_FIELDS (type))
	    {
	      CONTIN;
	    }

	  if (use_gnu_debug_info_extensions
	      && flag_minimal_debug
	      && TREE_CODE (tem) == FIELD_DECL
	      && DECL_VIRTUAL_P (tem)
	      && DECL_ASSEMBLER_NAME (tem))
	    {
	      have_used_extensions = 1;
	      CHARS (3 + IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (tem)));
	      fputs (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (tem)), asmfile);
	      dbxout_type (DECL_FCONTEXT (tem), 0, 0);
	      fprintf (asmfile, ":");
	      dbxout_type (TREE_TYPE (tem), 0, 0);
	      fputc (',', asmfile);
	      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		       TREE_INT_CST_LOW (DECL_FIELD_BITPOS (tem)));
	      fputc (';', asmfile);
	      continue;
	    }

	  if (DECL_NAME (tem))
	    {
	      fprintf (asmfile, "%s:", IDENTIFIER_POINTER (DECL_NAME (tem)));
	      CHARS (2 + IDENTIFIER_LENGTH (DECL_NAME (tem)));
	    }
	  else
	    {
	      fprintf (asmfile, ":");
	      CHARS (2);
	    }

	  if (use_gnu_debug_info_extensions
	      && (TREE_PRIVATE (tem) || TREE_PROTECTED (tem)
		  || TREE_CODE (tem) != FIELD_DECL))
	    {
	      have_used_extensions = 1;
	      putc ('/', asmfile);
	      putc ((TREE_PRIVATE (tem) ? '0'
		     : TREE_PROTECTED (tem) ? '1' : '2'),
		    asmfile);
	      CHARS (2);
	    }

	  dbxout_type ((TREE_CODE (tem) == FIELD_DECL
			&& DECL_BIT_FIELD_TYPE (tem))
		       ? DECL_BIT_FIELD_TYPE (tem)
		       : TREE_TYPE (tem), 0, 0);

	  if (TREE_CODE (tem) == VAR_DECL)
	    {
	      if (TREE_STATIC (tem) && use_gnu_debug_info_extensions)
		{
		  const char *name =
		    IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (tem));
		  have_used_extensions = 1;
		  fprintf (asmfile, ":%s;", name);
		  CHARS (strlen (name));
		}
	      else
		{
		  /* If TEM is non-static, GDB won't understand it.  */
		  fprintf (asmfile, ",0,0;");
		}
	    }
	  else if (TREE_CODE (DECL_FIELD_BITPOS (tem)) == INTEGER_CST)
	    {
	      fputc (',', asmfile);
	      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		       TREE_INT_CST_LOW (DECL_FIELD_BITPOS (tem)));
	      fputc (',', asmfile);
	      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		       TREE_INT_CST_LOW (DECL_SIZE (tem)));
	      fputc (';', asmfile);
	    }
	  CHARS (23);
	}
    }
}

/* Subroutine of `dbxout_type_methods'.  Output debug info about the
   method described DECL.  DEBUG_NAME is an encoding of the method's
   type signature.  ??? We may be able to do without DEBUG_NAME altogether
   now.  */

static void
dbxout_type_method_1 (decl, debug_name)
     tree decl;
     const char *debug_name;
{
  char c1 = 'A', c2;

  if (TREE_CODE (TREE_TYPE (decl)) == FUNCTION_TYPE)
    c2 = '?';
  else /* it's a METHOD_TYPE.  */
    {
      tree firstarg = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (decl)));
      /* A for normal functions.
	 B for `const' member functions.
	 C for `volatile' member functions.
	 D for `const volatile' member functions.  */
      if (TYPE_READONLY (TREE_TYPE (firstarg)))
	c1 += 1;
      if (TYPE_VOLATILE (TREE_TYPE (firstarg)))
	c1 += 2;

      if (DECL_VINDEX (decl))
	c2 = '*';
      else
	c2 = '.';
    }

  fprintf (asmfile, ":%s;%c%c%c", debug_name,
	   TREE_PRIVATE (decl) ? '0' : TREE_PROTECTED (decl) ? '1' : '2', c1, c2);
  CHARS (IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (decl)) + 6
	 - (debug_name - IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
  if (DECL_VINDEX (decl))
    {
      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
	       TREE_INT_CST_LOW (DECL_VINDEX (decl)));
      fputc (';', asmfile);
      dbxout_type (DECL_CONTEXT (decl), 0, 0);
      fprintf (asmfile, ";");
      CHARS (8);
    }
}

/* Subroutine of `dbxout_type'.  Output debug info about the methods defined
   in TYPE.  */

static void
dbxout_type_methods (type)
     register tree type;
{
  /* C++: put out the method names and their parameter lists */
  tree methods = TYPE_METHODS (type);
  tree type_encoding;
  register tree fndecl;
  register tree last;
  char formatted_type_identifier_length[16];
  register int type_identifier_length;

  if (methods == NULL_TREE)
    return;

  type_encoding = DECL_NAME (TYPE_NAME (type));

#if 0
  /* C++: Template classes break some assumptions made by this code about
     the class names, constructor names, and encodings for assembler
     label names.  For now, disable output of dbx info for them.  */
  {
    const char *ptr = IDENTIFIER_POINTER (type_encoding);
    /* This should use index.  (mrs) */
    while (*ptr && *ptr != '<') ptr++;
    if (*ptr != 0)
      {
	static int warned;
	if (!warned)
	    warned = 1;
	return;
      }
  }
#endif

  type_identifier_length = IDENTIFIER_LENGTH (type_encoding);

  sprintf(formatted_type_identifier_length, "%d", type_identifier_length);

  if (TREE_CODE (methods) != TREE_VEC)
    fndecl = methods;
  else if (TREE_VEC_ELT (methods, 0) != NULL_TREE)
    fndecl = TREE_VEC_ELT (methods, 0);
  else
    fndecl = TREE_VEC_ELT (methods, 1);

  while (fndecl)
    {
      tree name = DECL_NAME (fndecl);
      int need_prefix = 1;

      /* Group together all the methods for the same operation.
	 These differ in the types of the arguments.  */
      for (last = NULL_TREE;
	   fndecl && (last == NULL_TREE || DECL_NAME (fndecl) == DECL_NAME (last));
	   fndecl = TREE_CHAIN (fndecl))
	/* Output the name of the field (after overloading), as
	   well as the name of the field before overloading, along
	   with its parameter list */
	{
	  /* This is the "mangled" name of the method.
	     It encodes the argument types.  */
	  const char *debug_name =
	    IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl));
	  int show_arg_types = 0;

	  CONTIN;

	  last = fndecl;

	  if (DECL_IGNORED_P (fndecl))
	    continue;

	  if (flag_minimal_debug)
	    {
	      char marker;

	      /* We can't optimize a method which uses an anonymous
                 class, because the debugger will not be able to
                 associate the arbitrary class name with the actual
                 class.  */
#ifndef NO_DOLLAR_IN_LABEL
	      marker = '$';
#else
	      marker = '.';
#endif
	      if (strchr (debug_name, marker))
		show_arg_types = 1;
	      /* Detect ordinary methods because their mangled names
		 start with the operation name.  */
	      else if (!strncmp (IDENTIFIER_POINTER (name), debug_name,
				 IDENTIFIER_LENGTH (name)))
		{
		  debug_name += IDENTIFIER_LENGTH (name);
		  if (debug_name[0] == '_' && debug_name[1] == '_')
		    {
		      const char *method_name = debug_name + 2;
		      const char *length_ptr =
			formatted_type_identifier_length;
		      /* Get past const and volatile qualifiers.  */
		      while (*method_name == 'C' || *method_name == 'V')
			method_name++;
		      /* Skip digits for length of type_encoding.  */
		      while (*method_name == *length_ptr && *length_ptr)
			  length_ptr++, method_name++;
		      if (! strncmp (method_name,
				     IDENTIFIER_POINTER (type_encoding),
				     type_identifier_length))
			method_name += type_identifier_length;
		      debug_name = method_name;
		    }
		}
	      /* Detect constructors by their style of name mangling.  */
	      else if (debug_name[0] == '_' && debug_name[1] == '_')
		{
		  const char *ctor_name = debug_name + 2;
		  const char *length_ptr = formatted_type_identifier_length;
		  while (*ctor_name == 'C' || *ctor_name == 'V')
		    ctor_name++;
		  /* Skip digits for length of type_encoding.  */
		  while (*ctor_name == *length_ptr && *length_ptr)
		      length_ptr++, ctor_name++;
		  if (!strncmp (IDENTIFIER_POINTER (type_encoding), ctor_name,
				type_identifier_length))
		    debug_name = ctor_name + type_identifier_length;
		}
	      /* The other alternative is a destructor.  */
	      else
		show_arg_types = 1;

	      /* Output the operation name just once, for the first method
		 that we output.  */
	      if (need_prefix)
		{
		  fprintf (asmfile, "%s::", IDENTIFIER_POINTER (name));
		  CHARS (IDENTIFIER_LENGTH (name) + 2);
		  need_prefix = 0;
		}
	    }

	  dbxout_type (TREE_TYPE (fndecl), 0, show_arg_types);

	  dbxout_type_method_1 (fndecl, debug_name);
	}
      if (!need_prefix)
	{
          putc (';', asmfile);
	  CHARS (1);
	}
    }
}

/* Emit a "range" type specification, which has the form:
   "r<index type>;<lower bound>;<upper bound>;".
   TYPE is an INTEGER_TYPE.  */

static void
dbxout_range_type (type)
     tree type;
{
  fprintf (asmfile, "r");
  if (TREE_TYPE (type))
    dbxout_type (TREE_TYPE (type), 0, 0);
  else if (TREE_CODE (type) != INTEGER_TYPE)
    dbxout_type (type, 0, 0); /* E.g. Pascal's ARRAY [BOOLEAN] of INTEGER */
  else
    {
      /* Traditionally, we made sure 'int' was type 1, and builtin types
	 were defined to be sub-ranges of int.  Unfortunately, this
	 does not allow us to distinguish true sub-ranges from integer
	 types.  So, instead we define integer (non-sub-range) types as
	 sub-ranges of themselves.  This matters for Chill.  If this isn't
	 a subrange type, then we want to define it in terms of itself.
	 However, in C, this may be an anonymous integer type, and we don't
	 want to emit debug info referring to it.  Just calling
	 dbxout_type_index won't work anyways, because the type hasn't been
	 defined yet.  We make this work for both cases by checked to see
	 whether this is a defined type, referring to it if it is, and using
	 'int' otherwise.  */
      if (TYPE_SYMTAB_ADDRESS (type) != 0)
	dbxout_type_index (type);
      else
	dbxout_type_index (integer_type_node);
    }
  if (TREE_CODE (TYPE_MIN_VALUE (type)) == INTEGER_CST)
    {
      fputc (';', asmfile);
      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
	       TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)));
    }
  else
    fprintf (asmfile, ";0");
  if (TYPE_MAX_VALUE (type) 
      && TREE_CODE (TYPE_MAX_VALUE (type)) == INTEGER_CST)
    {
      fputc (';', asmfile);
      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
	       TREE_INT_CST_LOW (TYPE_MAX_VALUE (type)));
      fputc (';', asmfile);
    }
  else
    fprintf (asmfile, ";-1;");
}

/* Output a reference to a type.  If the type has not yet been
   described in the dbx output, output its definition now.
   For a type already defined, just refer to its definition
   using the type number.

   If FULL is nonzero, and the type has been described only with
   a forward-reference, output the definition now.
   If FULL is zero in this case, just refer to the forward-reference
   using the number previously allocated.

   If SHOW_ARG_TYPES is nonzero, we output a description of the argument
   types for a METHOD_TYPE.  */

static void
dbxout_type (type, full, show_arg_types)
     tree type;
     int full;
     int show_arg_types;
{
  register tree tem;
  static int anonymous_type_number = 0;

  /* If there was an input error and we don't really have a type,
     avoid crashing and write something that is at least valid
     by assuming `int'.  */
  if (type == error_mark_node)
    type = integer_type_node;
  else
    {
      /* Try to find the "main variant" with the same name but not const
	 or volatile.  (Since stabs does not distinguish const and volatile,
	 there is no need to make them separate types.  But types with
	 different names are usefully distinguished.) */
	 
      for (tem = TYPE_MAIN_VARIANT (type); tem; tem = TYPE_NEXT_VARIANT (tem))
	if (!TYPE_READONLY (tem) && !TYPE_VOLATILE (tem)
	    && TYPE_NAME (tem) == TYPE_NAME (type))
	  {
	    type = tem;
	    break;
	  }
      if (TYPE_NAME (type)
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (type)))
	full = 0;
    }

  if (TYPE_SYMTAB_ADDRESS (type) == 0)
    {
      /* Type has no dbx number assigned.  Assign next available number.  */
      TYPE_SYMTAB_ADDRESS (type) = next_type_number++;

      /* Make sure type vector is long enough to record about this type.  */

      if (next_type_number == typevec_len)
	{
	  typevec
	    = (struct typeinfo *) xrealloc (typevec,
					    typevec_len * 2 * sizeof typevec[0]);
	  bzero ((char *) (typevec + typevec_len),
		 typevec_len * sizeof typevec[0]);
	  typevec_len *= 2;
	}

#ifdef DBX_USE_BINCL
      typevec[TYPE_SYMTAB_ADDRESS (type)].file_number
	= current_file->file_number;
      typevec[TYPE_SYMTAB_ADDRESS (type)].type_number
	= current_file->next_type_number++;
#endif
    }

  /* Output the number of this type, to refer to it.  */
  dbxout_type_index (type);

#ifdef DBX_TYPE_DEFINED
  if (DBX_TYPE_DEFINED (type))
    return;
#endif

  /* If this type's definition has been output or is now being output,
     that is all.  */

  switch (typevec[TYPE_SYMTAB_ADDRESS (type)].status)
    {
    case TYPE_UNSEEN:
      break;
    case TYPE_XREF:
      /* If we have already had a cross reference,
	 and either that's all we want or that's the best we could do,
	 don't repeat the cross reference.
	 Sun dbx crashes if we do.  */
      if (! full || TYPE_SIZE (type) == 0
	  /* No way in DBX fmt to describe a variable size.  */
	  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	return;
      break;
    case TYPE_DEFINED:
      return;
    }

#ifdef DBX_NO_XREFS
  /* For systems where dbx output does not allow the `=xsNAME:' syntax,
     leave the type-number completely undefined rather than output
     a cross-reference.  If we have already used GNU debug info extensions,
     then it is OK to output a cross reference.  This is necessary to get
     proper C++ debug output.  */
  if ((TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
       || TREE_CODE (type) == QUAL_UNION_TYPE
       || TREE_CODE (type) == ENUMERAL_TYPE)
      && ! use_gnu_debug_info_extensions)
    /* We must use the same test here as we use twice below when deciding
       whether to emit a cross-reference.  */
    if ((TYPE_NAME (type) != 0
	 && ! (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	       && DECL_IGNORED_P (TYPE_NAME (type)))
	 && !full)
	|| TYPE_SIZE (type) == 0
	/* No way in DBX fmt to describe a variable size.  */
	|| TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
      {
	typevec[TYPE_SYMTAB_ADDRESS (type)].status = TYPE_XREF;
	return;
      }
#endif

  /* Output a definition now.  */

  fprintf (asmfile, "=");
  CHARS (1);

  /* Mark it as defined, so that if it is self-referent
     we will not get into an infinite recursion of definitions.  */

  typevec[TYPE_SYMTAB_ADDRESS (type)].status = TYPE_DEFINED;

  if (TYPE_NAME (type) && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (TYPE_NAME (type)))
    { 
      dbxout_type (DECL_ORIGINAL_TYPE (TYPE_NAME (type)), 0, 0);
      return;
    }

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case LANG_TYPE:
      /* For a void type, just define it as itself; ie, "5=5".
	 This makes us consider it defined
	 without saying what it is.  The debugger will make it
	 a void type when the reference is seen, and nothing will
	 ever override that default.  */
      dbxout_type_index (type);
      break;

    case INTEGER_TYPE:
      if (type == char_type_node && ! TREE_UNSIGNED (type))
	{
	  /* Output the type `char' as a subrange of itself!
	     I don't understand this definition, just copied it
	     from the output of pcc.
	     This used to use `r2' explicitly and we used to
	     take care to make sure that `char' was type number 2.  */
	  fprintf (asmfile, "r");
	  dbxout_type_index (type);
	  fprintf (asmfile, ";0;127;");
	}

      /* If this is a subtype of another integer type, always prefer to
	 write it as a subtype.  */
      else if (TREE_TYPE (type) != 0
	       && TREE_CODE (TREE_TYPE (type)) == INTEGER_CST)
	dbxout_range_type (type);

      else
  	{
	  /* If the size is non-standard, say what it is if we can use
	     GDB extensions.  */

	  if (use_gnu_debug_info_extensions
	      && TYPE_PRECISION (type) != TYPE_PRECISION (integer_type_node))
	    fprintf (asmfile, "@s%d;", TYPE_PRECISION (type));

	  /* If we can use GDB extensions and the size is wider than a
	     long (the size used by GDB to read them) or we may have
	     trouble writing the bounds the usual way, write them in
	     octal.  Note the test is for the *target's* size of "long",
	     not that of the host.  The host test is just to make sure we
	     can write it out in case the host wide int is narrower than the
	     target "long".  */

	  /* For unsigned types, we use octal if they are the same size or
	     larger.  This is because we print the bounds as signed decimal,
	     and hence they can't span same size unsigned types.  */

 	  if (use_gnu_debug_info_extensions
	      && (TYPE_PRECISION (type) > TYPE_PRECISION (integer_type_node)
		  || (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)
		      && TREE_UNSIGNED (type))
		  || TYPE_PRECISION (type) > HOST_BITS_PER_WIDE_INT
		  || (TYPE_PRECISION (type) == HOST_BITS_PER_WIDE_INT
		      && TREE_UNSIGNED (type))))
	    {
	      fprintf (asmfile, "r");
	      dbxout_type_index (type);
	      fprintf (asmfile, ";");
	      print_int_cst_octal (TYPE_MIN_VALUE (type));
	      fprintf (asmfile, ";");
	      print_int_cst_octal (TYPE_MAX_VALUE (type));
	      fprintf (asmfile, ";");
	    }

	  else
	    /* Output other integer types as subranges of `int'.  */
	    dbxout_range_type (type);
  	}

      CHARS (22);
      break;

    case REAL_TYPE:
      /* This used to say `r1' and we used to take care
	 to make sure that `int' was type number 1.  */
      fprintf (asmfile, "r");
      dbxout_type_index (integer_type_node);
      fputc (';', asmfile);
      fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC, int_size_in_bytes (type));
      fputs (";0;", asmfile);
      CHARS (13);
      break;

    case CHAR_TYPE:
      if (use_gnu_debug_info_extensions)
	{
	  fputs ("@s", asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		   BITS_PER_UNIT * int_size_in_bytes (type));
	  fputs (";-20;", asmfile);
	}
      else
	{
	  /* Output the type `char' as a subrange of itself.
	     That is what pcc seems to do.  */
	  fprintf (asmfile, "r");
	  dbxout_type_index (char_type_node);
	  fprintf (asmfile, ";0;%d;", TREE_UNSIGNED (type) ? 255 : 127);
	}
      CHARS (9);
      break;

    case BOOLEAN_TYPE:
      if (use_gnu_debug_info_extensions)
	{
	  fputs ("@s", asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		   BITS_PER_UNIT * int_size_in_bytes (type));
	  fputs (";-16;", asmfile);
	}
      else /* Define as enumeral type (False, True) */
	fprintf (asmfile, "eFalse:0,True:1,;");
      CHARS (17);
      break;

    case FILE_TYPE:
      putc ('d', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0, 0);
      break;

    case COMPLEX_TYPE:
      /* Differs from the REAL_TYPE by its new data type number */

      if (TREE_CODE (TREE_TYPE (type)) == REAL_TYPE)
	{
	  fprintf (asmfile, "r");
	  dbxout_type_index (type);
	  fputc (';', asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		   int_size_in_bytes (TREE_TYPE (type)));
	  fputs (";0;", asmfile);
	  CHARS (12);		/* The number is probably incorrect here.  */
	}
      else
	{
	  /* Output a complex integer type as a structure,
	     pending some other way to do it.  */
	  fputc ('s', asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC, int_size_in_bytes (type));

	  fprintf (asmfile, "real:");
	  CHARS (10);
	  dbxout_type (TREE_TYPE (type), 0, 0);
	  fprintf (asmfile, ",%d,%d;",
		   0, TYPE_PRECISION (TREE_TYPE (type)));
	  CHARS (8);
	  fprintf (asmfile, "imag:");
	  CHARS (5);
	  dbxout_type (TREE_TYPE (type), 0, 0);
	  fprintf (asmfile, ",%d,%d;;",
		   TYPE_PRECISION (TREE_TYPE (type)),
		   TYPE_PRECISION (TREE_TYPE (type)));
	  CHARS (9);
	}
      break;

    case SET_TYPE:
      if (use_gnu_debug_info_extensions)
	{
	  have_used_extensions = 1;
	  fputs ("@s", asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		   BITS_PER_UNIT * int_size_in_bytes (type));
	  fputc (';', asmfile);
	  /* Check if a bitstring type, which in Chill is
	     different from a [power]set.  */
	  if (TYPE_STRING_FLAG (type))
	    fprintf (asmfile, "@S;");
	}
      putc ('S', asmfile);
      CHARS (1);
      dbxout_type (TYPE_DOMAIN (type), 0, 0);
      break;

    case ARRAY_TYPE:
      /* Make arrays of packed bits look like bitstrings for chill.  */
      if (TYPE_PACKED (type) && use_gnu_debug_info_extensions)
	{
	  have_used_extensions = 1;
	  fputs ("@s", asmfile);
	  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		   BITS_PER_UNIT * int_size_in_bytes (type));
	  fputc (';', asmfile);
	  fprintf (asmfile, "@S;");
	  putc ('S', asmfile);
	  CHARS (1);
	  dbxout_type (TYPE_DOMAIN (type), 0, 0);
	  break;
	}
      /* Output "a" followed by a range type definition
	 for the index type of the array
	 followed by a reference to the target-type.
	 ar1;0;N;M for a C array of type M and size N+1.  */
      /* Check if a character string type, which in Chill is
	 different from an array of characters.  */
      if (TYPE_STRING_FLAG (type) && use_gnu_debug_info_extensions)
	{
	  have_used_extensions = 1;
	  fprintf (asmfile, "@S;");
	}
      tem = TYPE_DOMAIN (type);
      if (tem == NULL)
	{
	  fprintf (asmfile, "ar");
	  dbxout_type_index (integer_type_node);
	  fprintf (asmfile, ";0;-1;");
	}
      else
	{
	  fprintf (asmfile, "a");
	  dbxout_range_type (tem);
	}
      CHARS (14);
      dbxout_type (TREE_TYPE (type), 0, 0);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	int i, n_baseclasses = 0;

	if (TYPE_BINFO (type) != 0
	    && TREE_CODE (TYPE_BINFO (type)) == TREE_VEC
	    && TYPE_BINFO_BASETYPES (type) != 0)
	  n_baseclasses = TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (type));

	/* Output a structure type.  We must use the same test here as we
	   use in the DBX_NO_XREFS case above.  */
	if ((TYPE_NAME (type) != 0
	     && ! (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		   && DECL_IGNORED_P (TYPE_NAME (type)))
	     && !full)
	    || TYPE_SIZE (type) == 0
	    /* No way in DBX fmt to describe a variable size.  */
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  {
	    /* If the type is just a cross reference, output one
	       and mark the type as partially described.
	       If it later becomes defined, we will output
	       its real definition.
	       If the type has a name, don't nest its definition within
	       another type's definition; instead, output an xref
	       and let the definition come when the name is defined.  */
	    fputs ((TREE_CODE (type) == RECORD_TYPE) ? "xs" : "xu", asmfile);
	    CHARS (3);
#if 0 /* This assertion is legitimately false in C++.  */
	    /* We shouldn't be outputting a reference to a type before its
	       definition unless the type has a tag name.
	       A typedef name without a tag name should be impossible.  */
	    if (TREE_CODE (TYPE_NAME (type)) != IDENTIFIER_NODE)
	      abort ();
#endif
	    if (TYPE_NAME (type) != 0)
	      dbxout_type_name (type);
	    else
	      fprintf (asmfile, "$$%d", anonymous_type_number++);
	    fprintf (asmfile, ":");
	    typevec[TYPE_SYMTAB_ADDRESS (type)].status = TYPE_XREF;
	    break;
	  }

	/* Identify record or union, and print its size.  */
	fputc (((TREE_CODE (type) == RECORD_TYPE) ? 's' : 'u'), asmfile);
	fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		 int_size_in_bytes (type));

	if (use_gnu_debug_info_extensions)
	  {
	    if (n_baseclasses)
	      {
		have_used_extensions = 1;
		fprintf (asmfile, "!%d,", n_baseclasses);
		CHARS (8);
	      }
	  }
	for (i = 0; i < n_baseclasses; i++)
	  {
	    tree child = TREE_VEC_ELT (BINFO_BASETYPES (TYPE_BINFO (type)), i);
	    if (use_gnu_debug_info_extensions)
	      {
		have_used_extensions = 1;
		putc (TREE_VIA_VIRTUAL (child) ? '1'
		      : '0',
		      asmfile);
		putc (TREE_VIA_PUBLIC (child) ? '2'
		      : '0',
		      asmfile);
		fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
			 TREE_INT_CST_LOW (BINFO_OFFSET (child)) * BITS_PER_UNIT);
		fputc (',', asmfile);
		CHARS (15);
		dbxout_type (BINFO_TYPE (child), 0, 0);
		putc (';', asmfile);
	      }
	    else
	      {
		/* Print out the base class information with fields
		   which have the same names at the types they hold.  */
		dbxout_type_name (BINFO_TYPE (child));
		putc (':', asmfile);
		dbxout_type (BINFO_TYPE (child), full, 0);
		fputc (',', asmfile);
		fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
			 TREE_INT_CST_LOW (BINFO_OFFSET (child)) * BITS_PER_UNIT);
		fputc (',', asmfile);
		fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
			 TREE_INT_CST_LOW (DECL_SIZE (TYPE_NAME (BINFO_TYPE (child)))) * BITS_PER_UNIT);
		fputc (';', asmfile);
		CHARS (20);
	      }
	  }
      }

      CHARS (11);

      /* Write out the field declarations.  */
      dbxout_type_fields (type);
      if (use_gnu_debug_info_extensions && TYPE_METHODS (type) != NULL_TREE)
	{
	  have_used_extensions = 1;
	  dbxout_type_methods (type);
	}
      putc (';', asmfile);

      if (use_gnu_debug_info_extensions && TREE_CODE (type) == RECORD_TYPE
	  /* Avoid the ~ if we don't really need it--it confuses dbx.  */
	  && TYPE_VFIELD (type))
	{
	  have_used_extensions = 1;

	  /* Tell GDB+ that it may keep reading.  */
	  putc ('~', asmfile);

	  /* We need to write out info about what field this class
	     uses as its "main" vtable pointer field, because if this
	     field is inherited from a base class, GDB cannot necessarily
	     figure out which field it's using in time.  */
	  if (TYPE_VFIELD (type))
	    {
	      putc ('%', asmfile);
	      dbxout_type (DECL_FCONTEXT (TYPE_VFIELD (type)), 0, 0);
	    }
	  putc (';', asmfile);
	  CHARS (3);
	}
      break;

    case ENUMERAL_TYPE:
      /* We must use the same test here as we use in the DBX_NO_XREFS case
	 above.  We simplify it a bit since an enum will never have a variable
	 size.  */
      if ((TYPE_NAME (type) != 0
	   && ! (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		 && DECL_IGNORED_P (TYPE_NAME (type)))
	   && !full)
	  || TYPE_SIZE (type) == 0)
	{
	  fprintf (asmfile, "xe");
	  CHARS (3);
	  dbxout_type_name (type);
	  typevec[TYPE_SYMTAB_ADDRESS (type)].status = TYPE_XREF;
	  fprintf (asmfile, ":");
	  return;
	}
#ifdef DBX_OUTPUT_ENUM
      DBX_OUTPUT_ENUM (asmfile, type);
#else
      if (use_gnu_debug_info_extensions
	  && TYPE_PRECISION (type) != TYPE_PRECISION (integer_type_node))
	fprintf (asmfile, "@s%d;", TYPE_PRECISION (type));
      putc ('e', asmfile);
      CHARS (1);
      for (tem = TYPE_VALUES (type); tem; tem = TREE_CHAIN (tem))
	{
	  fprintf (asmfile, "%s:", IDENTIFIER_POINTER (TREE_PURPOSE (tem)));
	  if (TREE_INT_CST_HIGH (TREE_VALUE (tem)) == 0)
	    fprintf (asmfile, HOST_WIDE_INT_PRINT_UNSIGNED,
		     TREE_INT_CST_LOW (TREE_VALUE (tem)));
	  else if (TREE_INT_CST_HIGH (TREE_VALUE (tem)) == -1
		   && (HOST_WIDE_INT) TREE_INT_CST_LOW (TREE_VALUE (tem)) < 0)
	    fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC,
		     TREE_INT_CST_LOW (TREE_VALUE (tem)));
	  else
	    print_int_cst_octal (TREE_VALUE (tem));
	  fprintf (asmfile, ",");
	  CHARS (20 + IDENTIFIER_LENGTH (TREE_PURPOSE (tem)));
	  if (TREE_CHAIN (tem) != 0)
	    {
	      CONTIN;
	    }
	}
      putc (';', asmfile);
      CHARS (1);
#endif
      break;

    case POINTER_TYPE:
      putc ('*', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0, 0);
      break;

    case METHOD_TYPE:
      if (use_gnu_debug_info_extensions)
	{
	  have_used_extensions = 1;
	  putc ('#', asmfile);
	  CHARS (1);
	  if (flag_minimal_debug && !show_arg_types)
	    {
	      /* Normally, just output the return type.
		 The argument types are encoded in the method name.  */
	      putc ('#', asmfile);
	      CHARS (1);
	      dbxout_type (TREE_TYPE (type), 0, 0);
	      putc (';', asmfile);
	      CHARS (1);
	    }
	  else
	    {
	      /* When outputting destructors, we need to write
		 the argument types out longhand.  */
	      dbxout_type (TYPE_METHOD_BASETYPE (type), 0, 0);
	      putc (',', asmfile);
	      CHARS (1);
	      dbxout_type (TREE_TYPE (type), 0, 0);
	      dbxout_args (TYPE_ARG_TYPES (type));
	      putc (';', asmfile);
	      CHARS (1);
	    }
	}
      else
	{
	  /* Treat it as a function type.  */
	  dbxout_type (TREE_TYPE (type), 0, 0);
	}
      break;

    case OFFSET_TYPE:
      if (use_gnu_debug_info_extensions)
	{
	  have_used_extensions = 1;
	  putc ('@', asmfile);
	  CHARS (1);
	  dbxout_type (TYPE_OFFSET_BASETYPE (type), 0, 0);
	  putc (',', asmfile);
	  CHARS (1);
	  dbxout_type (TREE_TYPE (type), 0, 0);
	}
      else
	{
	  /* Should print as an int, because it is really
	     just an offset.  */
	  dbxout_type (integer_type_node, 0, 0);
	}
      break;

    case REFERENCE_TYPE:
      if (use_gnu_debug_info_extensions)
	have_used_extensions = 1;
      putc (use_gnu_debug_info_extensions ? '&' : '*', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0, 0);
      break;

    case FUNCTION_TYPE:
      putc ('f', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0, 0);
      break;

    default:
      abort ();
    }
}

/* Print the value of integer constant C, in octal,
   handling double precision.  */

static void
print_int_cst_octal (c)
     tree c;
{
  unsigned HOST_WIDE_INT high = TREE_INT_CST_HIGH (c);
  unsigned HOST_WIDE_INT low = TREE_INT_CST_LOW (c);
  int excess = (3 - (HOST_BITS_PER_WIDE_INT % 3));
  int width = TYPE_PRECISION (TREE_TYPE (c));

  /* GDB wants constants with no extra leading "1" bits, so
     we need to remove any sign-extension that might be
     present.  */
  if (width == HOST_BITS_PER_WIDE_INT * 2)
    ;
  else if (width > HOST_BITS_PER_WIDE_INT)
    high &= (((HOST_WIDE_INT) 1 << (width - HOST_BITS_PER_WIDE_INT)) - 1);
  else if (width == HOST_BITS_PER_WIDE_INT)
    high = 0;
  else
    high = 0, low &= (((HOST_WIDE_INT) 1 << width) - 1);

  fprintf (asmfile, "0");

  if (excess == 3)
    {
      print_octal (high, HOST_BITS_PER_WIDE_INT / 3);
      print_octal (low, HOST_BITS_PER_WIDE_INT / 3);
    }
  else
    {
      unsigned HOST_WIDE_INT beg = high >> excess;
      unsigned HOST_WIDE_INT middle
	= ((high & (((HOST_WIDE_INT) 1 << excess) - 1)) << (3 - excess)
	   | (low >> (HOST_BITS_PER_WIDE_INT / 3 * 3)));
      unsigned HOST_WIDE_INT end
	= low & (((unsigned HOST_WIDE_INT) 1
		  << (HOST_BITS_PER_WIDE_INT / 3 * 3))
		 - 1);

      fprintf (asmfile, "%o%01o", (int)beg, (int)middle);
      print_octal (end, HOST_BITS_PER_WIDE_INT / 3);
    }
}

static void
print_octal (value, digits)
     unsigned HOST_WIDE_INT value;
     int digits;
{
  int i;

  for (i = digits - 1; i >= 0; i--)
    fprintf (asmfile, "%01o", (int)((value >> (3 * i)) & 7));
}

/* Output the name of type TYPE, with no punctuation.
   Such names can be set up either by typedef declarations
   or by struct, enum and union tags.  */

static void
dbxout_type_name (type)
     register tree type;
{
  tree t;
  if (TYPE_NAME (type) == 0)
    abort ();
  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
    {
      t = TYPE_NAME (type);
    }
  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
    {
      t = DECL_NAME (TYPE_NAME (type));
    }
  else
    abort ();

  fprintf (asmfile, "%s", IDENTIFIER_POINTER (t));
  CHARS (IDENTIFIER_LENGTH (t));
}

/* Output a .stabs for the symbol defined by DECL,
   which must be a ..._DECL node in the normal namespace.
   It may be a CONST_DECL, a FUNCTION_DECL, a PARM_DECL or a VAR_DECL.
   LOCAL is nonzero if the scope is less than the entire file.
   Return 1 if a stabs might have been emitted.  */

int
dbxout_symbol (decl, local)
     tree decl;
     int local ATTRIBUTE_UNUSED;
{
  tree type = TREE_TYPE (decl);
  tree context = NULL_TREE;
  int result = 0;

  /* Cast avoids warning in old compilers.  */
  current_sym_code = (STAB_CODE_TYPE) 0;
  current_sym_value = 0;
  current_sym_addr = 0;

  /* Ignore nameless syms, but don't ignore type tags.  */

  if ((DECL_NAME (decl) == 0 && TREE_CODE (decl) != TYPE_DECL)
      || DECL_IGNORED_P (decl))
    return 0;

  dbxout_prepare_symbol (decl);

  /* The output will always start with the symbol name,
     so always count that in the length-output-so-far.  */

  if (DECL_NAME (decl) != 0)
    current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (decl));

  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* Enum values are defined by defining the enum type.  */
      break;

    case FUNCTION_DECL:
      if (DECL_RTL (decl) == 0)
	return 0;
      if (DECL_EXTERNAL (decl))
	break;
      /* Don't mention a nested function under its parent.  */
      context = decl_function_context (decl);
      if (context == current_function_decl)
	break;
      if (GET_CODE (DECL_RTL (decl)) != MEM
	  || GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
	break;
      FORCE_TEXT;

      fprintf (asmfile, "%s \"%s:%c", ASM_STABS_OP,
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
	       TREE_PUBLIC (decl) ? 'F' : 'f');
      result = 1;

      current_sym_code = N_FUN;
      current_sym_addr = XEXP (DECL_RTL (decl), 0);

      if (TREE_TYPE (type))
	dbxout_type (TREE_TYPE (type), 0, 0);
      else
	dbxout_type (void_type_node, 0, 0);

      /* For a nested function, when that function is compiled,
	 mention the containing function name
	 as well as (since dbx wants it) our own assembler-name.  */
      if (context != 0)
	fprintf (asmfile, ",%s,%s",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
		 IDENTIFIER_POINTER (DECL_NAME (context)));

      dbxout_finish_symbol (decl);
      break;

    case TYPE_DECL:
#if 0
      /* This seems all wrong.  Outputting most kinds of types gives no name
	 at all.  A true definition gives no name; a cross-ref for a
	 structure can give the tag name, but not a type name.
	 It seems that no typedef name is defined by outputting a type.  */

      /* If this typedef name was defined by outputting the type,
	 don't duplicate it.  */
      if (typevec[TYPE_SYMTAB_ADDRESS (type)].status == TYPE_DEFINED
	  && TYPE_NAME (TREE_TYPE (decl)) == decl)
	return 0;
#endif
      /* Don't output the same typedef twice.
         And don't output what language-specific stuff doesn't want output.  */
      if (TREE_ASM_WRITTEN (decl) || TYPE_DECL_SUPPRESS_DEBUG (decl))
	return 0;

      FORCE_TEXT;
      result = 1;
      {
	int tag_needed = 1;
	int did_output = 0;

	if (DECL_NAME (decl))
	  {
	    /* Nonzero means we must output a tag as well as a typedef.  */
	    tag_needed = 0;

	    /* Handle the case of a C++ structure or union
	       where the TYPE_NAME is a TYPE_DECL
	       which gives both a typedef name and a tag.  */
	    /* dbx requires the tag first and the typedef second.  */
	    if ((TREE_CODE (type) == RECORD_TYPE
		 || TREE_CODE (type) == UNION_TYPE
		 || TREE_CODE (type) == QUAL_UNION_TYPE)
		&& TYPE_NAME (type) == decl
		&& !(use_gnu_debug_info_extensions && have_used_extensions)
		&& !TREE_ASM_WRITTEN (TYPE_NAME (type))
		/* Distinguish the implicit typedefs of C++
		   from explicit ones that might be found in C.  */
                && DECL_ARTIFICIAL (decl))
	      {
		tree name = TYPE_NAME (type);
		if (TREE_CODE (name) == TYPE_DECL)
		  name = DECL_NAME (name);

		current_sym_code = DBX_TYPE_DECL_STABS_CODE;
		current_sym_value = 0;
		current_sym_addr = 0;
		current_sym_nchars = 2 + IDENTIFIER_LENGTH (name);

		fprintf (asmfile, "%s \"%s:T", ASM_STABS_OP,
			 IDENTIFIER_POINTER (name));
		dbxout_type (type, 1, 0);
		dbxout_finish_symbol (NULL_TREE);
	      }

	    /* Output typedef name.  */
	    fprintf (asmfile, "%s \"%s:", ASM_STABS_OP,
		     IDENTIFIER_POINTER (DECL_NAME (decl)));

	    /* Short cut way to output a tag also.  */
	    if ((TREE_CODE (type) == RECORD_TYPE
		 || TREE_CODE (type) == UNION_TYPE
		 || TREE_CODE (type) == QUAL_UNION_TYPE)
		&& TYPE_NAME (type) == decl
		/* Distinguish the implicit typedefs of C++
		   from explicit ones that might be found in C.  */
                && DECL_ARTIFICIAL (decl))
	      {
		if (use_gnu_debug_info_extensions && have_used_extensions)
		  {
		    putc ('T', asmfile);
		    TREE_ASM_WRITTEN (TYPE_NAME (type)) = 1;
		  }
#if 0 /* Now we generate the tag for this case up above.  */
		else
		  tag_needed = 1;
#endif
	      }

	    putc ('t', asmfile);
	    current_sym_code = DBX_TYPE_DECL_STABS_CODE;

	    dbxout_type (type, 1, 0);
	    dbxout_finish_symbol (decl);
	    did_output = 1;
	  }

	/* Don't output a tag if this is an incomplete type (TYPE_SIZE is
	   zero).  This prevents the sun4 Sun OS 4.x dbx from crashing.  */ 

	if (tag_needed && TYPE_NAME (type) != 0
	    && (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE
		|| (DECL_NAME (TYPE_NAME (type)) != 0))
	    && TYPE_SIZE (type) != 0
	    && !TREE_ASM_WRITTEN (TYPE_NAME (type)))
	  {
	    /* For a TYPE_DECL with no name, but the type has a name,
	       output a tag.
	       This is what represents `struct foo' with no typedef.  */
	    /* In C++, the name of a type is the corresponding typedef.
	       In C, it is an IDENTIFIER_NODE.  */
	    tree name = TYPE_NAME (type);
	    if (TREE_CODE (name) == TYPE_DECL)
	      name = DECL_NAME (name);

	    current_sym_code = DBX_TYPE_DECL_STABS_CODE;
	    current_sym_value = 0;
	    current_sym_addr = 0;
	    current_sym_nchars = 2 + IDENTIFIER_LENGTH (name);

	    fprintf (asmfile, "%s \"%s:T", ASM_STABS_OP,
		     IDENTIFIER_POINTER (name));
	    dbxout_type (type, 1, 0);
	    dbxout_finish_symbol (NULL_TREE);
	    did_output = 1;
	  }

	/* If an enum type has no name, it cannot be referred to,
	   but we must output it anyway, since the enumeration constants
	   can be referred to.  */
	if (!did_output && TREE_CODE (type) == ENUMERAL_TYPE)
	  {
	    current_sym_code = DBX_TYPE_DECL_STABS_CODE;
	    current_sym_value = 0;
	    current_sym_addr = 0;
	    current_sym_nchars = 2;

	    /* Some debuggers fail when given NULL names, so give this a
	       harmless name of ` '.  */
	    fprintf (asmfile, "%s \" :T", ASM_STABS_OP);
	    dbxout_type (type, 1, 0);
	    dbxout_finish_symbol (NULL_TREE);
	  }

	/* Prevent duplicate output of a typedef.  */
	TREE_ASM_WRITTEN (decl) = 1;
	break;
      }

    case PARM_DECL:
      /* Parm decls go in their own separate chains
	 and are output by dbxout_reg_parms and dbxout_parms.  */
      abort ();

    case RESULT_DECL:
      /* Named return value, treat like a VAR_DECL.  */
    case VAR_DECL:
      if (DECL_RTL (decl) == 0)
	return 0;
      /* Don't mention a variable that is external.
	 Let the file that defines it describe it.  */
      if (DECL_EXTERNAL (decl))
	break;

      /* If the variable is really a constant
	 and not written in memory, inform the debugger.  */
      if (TREE_STATIC (decl) && TREE_READONLY (decl)
	  && DECL_INITIAL (decl) != 0
	  && ! TREE_ASM_WRITTEN (decl)
	  && (DECL_FIELD_CONTEXT (decl) == NULL_TREE
	      || TREE_CODE (DECL_FIELD_CONTEXT (decl)) == BLOCK))
	{
	  if (TREE_PUBLIC (decl) == 0)
	    {
	      /* The sun4 assembler does not grok this.  */
	      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
	      if (TREE_CODE (TREE_TYPE (decl)) == INTEGER_TYPE
		  || TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE)
		{
		  HOST_WIDE_INT ival = TREE_INT_CST_LOW (DECL_INITIAL (decl));
#ifdef DBX_OUTPUT_CONSTANT_SYMBOL
		  DBX_OUTPUT_CONSTANT_SYMBOL (asmfile, name, ival);
#else
		  fprintf (asmfile, "%s \"%s:c=i", ASM_STABS_OP, name);

		  fprintf (asmfile, HOST_WIDE_INT_PRINT_DEC, ival);
		  fprintf (asmfile, "\",0x%x,0,0,0\n", N_LSYM);
#endif
		  return 1;
		}
	      else if (TREE_CODE (TREE_TYPE (decl)) == REAL_TYPE)
		{
		  /* don't know how to do this yet.  */
		}
	      break;
	    }
	  /* else it is something we handle like a normal variable.  */
	}

      DECL_RTL (decl) = eliminate_regs (DECL_RTL (decl), 0, NULL_RTX);
#ifdef LEAF_REG_REMAP
      if (current_function_uses_only_leaf_regs)
	leaf_renumber_regs_insn (DECL_RTL (decl));
#endif

      result = dbxout_symbol_location (decl, type, 0, DECL_RTL (decl));
      break;
      
    default:
      break;
    }
  return result;
}

/* Output the stab for DECL, a VAR_DECL, RESULT_DECL or PARM_DECL.
   Add SUFFIX to its name, if SUFFIX is not 0.
   Describe the variable as residing in HOME
   (usually HOME is DECL_RTL (DECL), but not always).
   Returns 1 if the stab was really emitted.  */

static int
dbxout_symbol_location (decl, type, suffix, home)
     tree decl, type;
     const char *suffix;
     rtx home;
{
  int letter = 0;
  int regno = -1;

  /* Don't mention a variable at all
     if it was completely optimized into nothingness.
     
     If the decl was from an inline function, then its rtl
     is not identically the rtl that was used in this
     particular compilation.  */
  if (GET_CODE (home) == REG)
    {
      regno = REGNO (home);
      if (regno >= FIRST_PSEUDO_REGISTER)
	return 0;
    }
  else if (GET_CODE (home) == SUBREG)
    {
      rtx value = home;
      int offset = 0;
      while (GET_CODE (value) == SUBREG)
	{
	  offset += SUBREG_WORD (value);
	  value = SUBREG_REG (value);
	}
      if (GET_CODE (value) == REG)
	{
	  regno = REGNO (value);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    return 0;
	  regno += offset;
	}
      alter_subreg (home);
    }

  /* The kind-of-variable letter depends on where
     the variable is and on the scope of its name:
     G and N_GSYM for static storage and global scope,
     S for static storage and file scope,
     V for static storage and local scope,
     for those two, use N_LCSYM if data is in bss segment,
     N_STSYM if in data segment, N_FUN otherwise.
     (We used N_FUN originally, then changed to N_STSYM
     to please GDB.  However, it seems that confused ld.
     Now GDB has been fixed to like N_FUN, says Kingdon.)
     no letter at all, and N_LSYM, for auto variable,
     r and N_RSYM for register variable.  */

  if (GET_CODE (home) == MEM
      && GET_CODE (XEXP (home, 0)) == SYMBOL_REF)
    {
      if (TREE_PUBLIC (decl))
	{
	  letter = 'G';
	  current_sym_code = N_GSYM;
	}
      else
	{
	  current_sym_addr = XEXP (home, 0);

	  letter = decl_function_context (decl) ? 'V' : 'S';

	  /* This should be the same condition as in assemble_variable, but
	     we don't have access to dont_output_data here.  So, instead,
	     we rely on the fact that error_mark_node initializers always
	     end up in bss for C++ and never end up in bss for C.  */
	  if (DECL_INITIAL (decl) == 0
	      || (!strcmp (lang_identify (), "cplusplus")
		  && DECL_INITIAL (decl) == error_mark_node))
	    current_sym_code = N_LCSYM;
	  else if (DECL_IN_TEXT_SECTION (decl))
	    /* This is not quite right, but it's the closest
	       of all the codes that Unix defines.  */
	    current_sym_code = DBX_STATIC_CONST_VAR_CODE;
	  else
	    {
	      /* Ultrix `as' seems to need this.  */
#ifdef DBX_STATIC_STAB_DATA_SECTION
	      data_section ();
#endif
	      current_sym_code = N_STSYM;
	    }
	}
    }
  else if (regno >= 0)
    {
      letter = 'r';
      current_sym_code = N_RSYM;
      current_sym_value = DBX_REGISTER_NUMBER (regno);
    }
  else if (GET_CODE (home) == MEM
	   && (GET_CODE (XEXP (home, 0)) == MEM
	       || (GET_CODE (XEXP (home, 0)) == REG
		   && REGNO (XEXP (home, 0)) != HARD_FRAME_POINTER_REGNUM
		   && REGNO (XEXP (home, 0)) != STACK_POINTER_REGNUM
#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		   && REGNO (XEXP (home, 0)) != ARG_POINTER_REGNUM
#endif
		   )))
    /* If the value is indirect by memory or by a register
       that isn't the frame pointer
       then it means the object is variable-sized and address through
       that register or stack slot.  DBX has no way to represent this
       so all we can do is output the variable as a pointer.
       If it's not a parameter, ignore it.
       (VAR_DECLs like this can be made by integrate.c.)  */
    {
      if (GET_CODE (XEXP (home, 0)) == REG)
	{
	  letter = 'r';
	  current_sym_code = N_RSYM;
	  current_sym_value = DBX_REGISTER_NUMBER (REGNO (XEXP (home, 0)));
	}
      else
	{
	  current_sym_code = N_LSYM;
	  /* RTL looks like (MEM (MEM (PLUS (REG...) (CONST_INT...)))).
	     We want the value of that CONST_INT.  */
	  current_sym_value
	    = DEBUGGER_AUTO_OFFSET (XEXP (XEXP (home, 0), 0));
	}

      /* Effectively do build_pointer_type, but don't cache this type,
	 since it might be temporary whereas the type it points to
	 might have been saved for inlining.  */
      /* Don't use REFERENCE_TYPE because dbx can't handle that.  */
      type = make_node (POINTER_TYPE);
      TREE_TYPE (type) = TREE_TYPE (decl);
    }
  else if (GET_CODE (home) == MEM
	   && GET_CODE (XEXP (home, 0)) == REG)
    {
      current_sym_code = N_LSYM;
      current_sym_value = DEBUGGER_AUTO_OFFSET (XEXP (home, 0));
    }
  else if (GET_CODE (home) == MEM
	   && GET_CODE (XEXP (home, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (home, 0), 1)) == CONST_INT)
    {
      current_sym_code = N_LSYM;
      /* RTL looks like (MEM (PLUS (REG...) (CONST_INT...)))
	 We want the value of that CONST_INT.  */
      current_sym_value = DEBUGGER_AUTO_OFFSET (XEXP (home, 0));
    }
  else if (GET_CODE (home) == MEM
	   && GET_CODE (XEXP (home, 0)) == CONST)
    {
      /* Handle an obscure case which can arise when optimizing and
	 when there are few available registers.  (This is *always*
	 the case for i386/i486 targets).  The RTL looks like
	 (MEM (CONST ...)) even though this variable is a local `auto'
	 or a local `register' variable.  In effect, what has happened
	 is that the reload pass has seen that all assignments and
	 references for one such a local variable can be replaced by
	 equivalent assignments and references to some static storage
	 variable, thereby avoiding the need for a register.  In such
	 cases we're forced to lie to debuggers and tell them that
	 this variable was itself `static'.  */
      current_sym_code = N_LCSYM;
      letter = 'V';
      current_sym_addr = XEXP (XEXP (home, 0), 0);
    }
  else if (GET_CODE (home) == CONCAT)
    {
      tree subtype = TREE_TYPE (type);

      /* If the variable's storage is in two parts,
	 output each as a separate stab with a modified name.  */
      if (WORDS_BIG_ENDIAN)
	dbxout_symbol_location (decl, subtype, "$imag", XEXP (home, 0));
      else
	dbxout_symbol_location (decl, subtype, "$real", XEXP (home, 0));

      /* Cast avoids warning in old compilers.  */
      current_sym_code = (STAB_CODE_TYPE) 0;
      current_sym_value = 0;
      current_sym_addr = 0;
      dbxout_prepare_symbol (decl);

      if (WORDS_BIG_ENDIAN)
	dbxout_symbol_location (decl, subtype, "$real", XEXP (home, 1));
      else
	dbxout_symbol_location (decl, subtype, "$imag", XEXP (home, 1));
      return 1;
    }
  else
    /* Address might be a MEM, when DECL is a variable-sized object.
       Or it might be const0_rtx, meaning previous passes
       want us to ignore this variable.  */
    return 0;

  /* Ok, start a symtab entry and output the variable name.  */
  FORCE_TEXT;

#ifdef DBX_STATIC_BLOCK_START
  DBX_STATIC_BLOCK_START (asmfile, current_sym_code);
#endif

  dbxout_symbol_name (decl, suffix, letter);
  dbxout_type (type, 0, 0);
  dbxout_finish_symbol (decl);

#ifdef DBX_STATIC_BLOCK_END
  DBX_STATIC_BLOCK_END (asmfile, current_sym_code);
#endif
  return 1;
}

/* Output the symbol name of DECL for a stabs, with suffix SUFFIX.
   Then output LETTER to indicate the kind of location the symbol has.  */

static void
dbxout_symbol_name (decl, suffix, letter)
     tree decl;
     const char *suffix;
     int letter;
{
  /* One slight hitch: if this is a VAR_DECL which is a static
     class member, we must put out the mangled name instead of the
     DECL_NAME.  Note also that static member (variable) names DO NOT begin
     with underscores in .stabs directives.  */
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  if (name == 0)
    name = "(anon)";
  fprintf (asmfile, "%s \"%s%s:", ASM_STABS_OP, name,
	   (suffix ? suffix : ""));

  if (letter) putc (letter, asmfile);
}

static void
dbxout_prepare_symbol (decl)
     tree decl ATTRIBUTE_UNUSED;
{
#ifdef WINNING_GDB
  const char *filename = DECL_SOURCE_FILE (decl);

  dbxout_source_file (asmfile, filename);
#endif
}

static void
dbxout_finish_symbol (sym)
     tree sym;
{
#ifdef DBX_FINISH_SYMBOL
  DBX_FINISH_SYMBOL (sym);
#else
  int line = 0;
  if (use_gnu_debug_info_extensions && sym != 0)
    line = DECL_SOURCE_LINE (sym);

  fprintf (asmfile, "\",%d,0,%d,", current_sym_code, line);
  if (current_sym_addr)
    output_addr_const (asmfile, current_sym_addr);
  else
    fprintf (asmfile, "%d", current_sym_value);
  putc ('\n', asmfile);
#endif
}

/* Output definitions of all the decls in a chain. Return non-zero if
   anything was output */

int
dbxout_syms (syms)
     tree syms;
{
  int result = 0;
  while (syms)
    {
      result += dbxout_symbol (syms, 1);
      syms = TREE_CHAIN (syms);
    }
  return result;
}

/* The following two functions output definitions of function parameters.
   Each parameter gets a definition locating it in the parameter list.
   Each parameter that is a register variable gets a second definition
   locating it in the register.

   Printing or argument lists in gdb uses the definitions that
   locate in the parameter list.  But reference to the variable in
   expressions uses preferentially the definition as a register.  */

/* Output definitions, referring to storage in the parmlist,
   of all the parms in PARMS, which is a chain of PARM_DECL nodes.  */

void
dbxout_parms (parms)
     tree parms;
{
  for (; parms; parms = TREE_CHAIN (parms))
    if (DECL_NAME (parms) && TREE_TYPE (parms) != error_mark_node)
      {
	dbxout_prepare_symbol (parms);

	/* Perform any necessary register eliminations on the parameter's rtl,
	   so that the debugging output will be accurate.  */
	DECL_INCOMING_RTL (parms)
	  = eliminate_regs (DECL_INCOMING_RTL (parms), 0, NULL_RTX);
	DECL_RTL (parms) = eliminate_regs (DECL_RTL (parms), 0, NULL_RTX);
#ifdef LEAF_REG_REMAP
	if (current_function_uses_only_leaf_regs)
	  {
	    leaf_renumber_regs_insn (DECL_INCOMING_RTL (parms));
	    leaf_renumber_regs_insn (DECL_RTL (parms));
	  }
#endif

	if (PARM_PASSED_IN_MEMORY (parms))
	  {
	    rtx addr = XEXP (DECL_INCOMING_RTL (parms), 0);

	    /* ??? Here we assume that the parm address is indexed
	       off the frame pointer or arg pointer.
	       If that is not true, we produce meaningless results,
	       but do not crash.  */
	    if (GET_CODE (addr) == PLUS
		&& GET_CODE (XEXP (addr, 1)) == CONST_INT)
	      current_sym_value = INTVAL (XEXP (addr, 1));
	    else
	      current_sym_value = 0;

	    current_sym_code = N_PSYM;
	    current_sym_addr = 0;

	    FORCE_TEXT;
	    if (DECL_NAME (parms))
	      {
		current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));

		fprintf (asmfile, "%s \"%s:%c", ASM_STABS_OP,
			 IDENTIFIER_POINTER (DECL_NAME (parms)),
			 DBX_MEMPARM_STABS_LETTER);
	      }
	    else
	      {
		current_sym_nchars = 8;
		fprintf (asmfile, "%s \"(anon):%c", ASM_STABS_OP,
			 DBX_MEMPARM_STABS_LETTER);
	      }

	    /* It is quite tempting to use:
	       
	           dbxout_type (TREE_TYPE (parms), 0, 0);

	       as the next statement, rather than using DECL_ARG_TYPE(), so
	       that gcc reports the actual type of the parameter, rather
	       than the promoted type.  This certainly makes GDB's life
	       easier, at least for some ports.  The change is a bad idea
	       however, since GDB expects to be able access the type without
	       performing any conversions.  So for example, if we were
	       passing a float to an unprototyped function, gcc will store a
	       double on the stack, but if we emit a stab saying the type is a
	       float, then gdb will only read in a single value, and this will
	       produce an erropneous value.  */
 	    dbxout_type (DECL_ARG_TYPE (parms), 0, 0);
	    current_sym_value = DEBUGGER_ARG_OFFSET (current_sym_value, addr);
	    dbxout_finish_symbol (parms);
	  }
	else if (GET_CODE (DECL_RTL (parms)) == REG)
	  {
	    rtx best_rtl;
	    char regparm_letter;
	    tree parm_type;
	    /* Parm passed in registers and lives in registers or nowhere.  */

	    current_sym_code = DBX_REGPARM_STABS_CODE;
	    regparm_letter = DBX_REGPARM_STABS_LETTER;
	    current_sym_addr = 0;

	    /* If parm lives in a register, use that register;
	       pretend the parm was passed there.  It would be more consistent
	       to describe the register where the parm was passed,
	       but in practice that register usually holds something else.

	       If we use DECL_RTL, then we must use the declared type of
	       the variable, not the type that it arrived in.  */
	    if (REGNO (DECL_RTL (parms)) >= 0
		&& REGNO (DECL_RTL (parms)) < FIRST_PSEUDO_REGISTER)
	      {
		best_rtl = DECL_RTL (parms);
		parm_type = TREE_TYPE (parms);
	      }
	    /* If the parm lives nowhere, use the register where it was
	       passed.  It is also better to use the declared type here.  */
	    else
	      {
		best_rtl = DECL_INCOMING_RTL (parms);
		parm_type = TREE_TYPE (parms);
	      }
	    current_sym_value = DBX_REGISTER_NUMBER (REGNO (best_rtl));

	    FORCE_TEXT;
	    if (DECL_NAME (parms))
	      {
		current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));
		fprintf (asmfile, "%s \"%s:%c", ASM_STABS_OP,
			 IDENTIFIER_POINTER (DECL_NAME (parms)),
			 regparm_letter);
	      }
	    else
	      {
		current_sym_nchars = 8;
		fprintf (asmfile, "%s \"(anon):%c", ASM_STABS_OP,
			 regparm_letter);
	      }

	    dbxout_type (parm_type, 0, 0);
	    dbxout_finish_symbol (parms);
	  }
	else if (GET_CODE (DECL_RTL (parms)) == MEM
		 && GET_CODE (XEXP (DECL_RTL (parms), 0)) == REG
		 && REGNO (XEXP (DECL_RTL (parms), 0)) != HARD_FRAME_POINTER_REGNUM
		 && REGNO (XEXP (DECL_RTL (parms), 0)) != STACK_POINTER_REGNUM
#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		 && REGNO (XEXP (DECL_RTL (parms), 0)) != ARG_POINTER_REGNUM
#endif
		 )
	  {
	    /* Parm was passed via invisible reference.
	       That is, its address was passed in a register.
	       Output it as if it lived in that register.
	       The debugger will know from the type
	       that it was actually passed by invisible reference.  */

	    char regparm_letter;
	    /* Parm passed in registers and lives in registers or nowhere.  */

	    current_sym_code = DBX_REGPARM_STABS_CODE;
	    if (use_gnu_debug_info_extensions)
	      regparm_letter = GDB_INV_REF_REGPARM_STABS_LETTER;
	    else
	      regparm_letter = DBX_REGPARM_STABS_LETTER;

	    /* DECL_RTL looks like (MEM (REG...).  Get the register number.
	       If it is an unallocated pseudo-reg, then use the register where
	       it was passed instead.  */
	    if (REGNO (XEXP (DECL_RTL (parms), 0)) >= 0
		&& REGNO (XEXP (DECL_RTL (parms), 0)) < FIRST_PSEUDO_REGISTER)
	      current_sym_value = REGNO (XEXP (DECL_RTL (parms), 0));
	    else
	      current_sym_value = REGNO (DECL_INCOMING_RTL (parms));

	    current_sym_addr = 0;

	    FORCE_TEXT;
	    if (DECL_NAME (parms))
	      {
		current_sym_nchars = 2 + strlen (IDENTIFIER_POINTER (DECL_NAME (parms)));

		fprintf (asmfile, "%s \"%s:%c", ASM_STABS_OP,
			 IDENTIFIER_POINTER (DECL_NAME (parms)),
			 regparm_letter);
	      }
	    else
	      {
		current_sym_nchars = 8;
		fprintf (asmfile, "%s \"(anon):%c", ASM_STABS_OP,
			 regparm_letter);
	      }

	    dbxout_type (TREE_TYPE (parms), 0, 0);
	    dbxout_finish_symbol (parms);
	  }
	else if (GET_CODE (DECL_RTL (parms)) == MEM
		 && XEXP (DECL_RTL (parms), 0) != const0_rtx
		 /* ??? A constant address for a parm can happen
		    when the reg it lives in is equiv to a constant in memory.
		    Should make this not happen, after 2.4.  */
		 && ! CONSTANT_P (XEXP (DECL_RTL (parms), 0)))
	  {
	    /* Parm was passed in registers but lives on the stack.  */
	    int aux_sym_value = 0;

	    current_sym_code = N_PSYM;
	    /* DECL_RTL looks like (MEM (PLUS (REG...) (CONST_INT...))),
	       in which case we want the value of that CONST_INT,
	       or (MEM (REG ...)) or (MEM (MEM ...)),
	       in which case we use a value of zero.  */
	    if (GET_CODE (XEXP (DECL_RTL (parms), 0)) == REG)
	      current_sym_value = 0;
	    else if (GET_CODE (XEXP (DECL_RTL (parms), 0)) == MEM)
	      {
		/* Remember the location on the stack the parm is moved to */
	        aux_sym_value
		  = INTVAL (XEXP (XEXP (XEXP (DECL_RTL (parms), 0), 0), 1));
	        current_sym_value = 0;
	      }
	    else
		current_sym_value
		  = INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1));

	    current_sym_addr = 0;

	    /* Make a big endian correction if the mode of the type of the
	       parameter is not the same as the mode of the rtl.  */
	    if (BYTES_BIG_ENDIAN
		&& TYPE_MODE (TREE_TYPE (parms)) != GET_MODE (DECL_RTL (parms))
		&& GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (parms))) < UNITS_PER_WORD)
	      {
		current_sym_value += 
		    GET_MODE_SIZE (GET_MODE (DECL_RTL (parms)))
		    - GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (parms)));
	      }

	    FORCE_TEXT;
	    if (DECL_NAME (parms))
	      {
		current_sym_nchars
		  = 2 + strlen (IDENTIFIER_POINTER (DECL_NAME (parms)));

		fprintf (asmfile, "%s \"%s:%c", ASM_STABS_OP,
			 IDENTIFIER_POINTER (DECL_NAME (parms)),
			 DBX_MEMPARM_STABS_LETTER);
	      }
	    else
	      {
		current_sym_nchars = 8;
		fprintf (asmfile, "%s \"(anon):%c", ASM_STABS_OP,
		DBX_MEMPARM_STABS_LETTER);
	      }

	    current_sym_value
	      = DEBUGGER_ARG_OFFSET (current_sym_value,
				     XEXP (DECL_RTL (parms), 0));
	    dbxout_type (TREE_TYPE (parms), 0, 0);
	    dbxout_finish_symbol (parms);
	    if (aux_sym_value != 0)
	      {
		/* Generate an entry for the stack location */

		fprintf (asmfile, "%s \"%s:", ASM_STABS_OP,
			 IDENTIFIER_POINTER (DECL_NAME (parms)));
		current_sym_value = aux_sym_value;
	        current_sym_code = N_LSYM;
	        dbxout_type (build_reference_type (TREE_TYPE (parms)), 0, 0);
	        dbxout_finish_symbol (parms);
	      }
	  }
      }
}

/* Output definitions for the places where parms live during the function,
   when different from where they were passed, when the parms were passed
   in memory.

   It is not useful to do this for parms passed in registers
   that live during the function in different registers, because it is
   impossible to look in the passed register for the passed value,
   so we use the within-the-function register to begin with.

   PARMS is a chain of PARM_DECL nodes.  */

void
dbxout_reg_parms (parms)
     tree parms;
{
  for (; parms; parms = TREE_CHAIN (parms))
    if (DECL_NAME (parms) && PARM_PASSED_IN_MEMORY (parms))
      {
	dbxout_prepare_symbol (parms);

	/* Report parms that live in registers during the function
	   but were passed in memory.  */
	if (GET_CODE (DECL_RTL (parms)) == REG
	    && REGNO (DECL_RTL (parms)) >= 0
	    && REGNO (DECL_RTL (parms)) < FIRST_PSEUDO_REGISTER)
	  dbxout_symbol_location (parms, TREE_TYPE (parms),
				  0, DECL_RTL (parms));
	else if (GET_CODE (DECL_RTL (parms)) == CONCAT)
	  dbxout_symbol_location (parms, TREE_TYPE (parms),
				  0, DECL_RTL (parms));
	/* Report parms that live in memory but not where they were passed.  */
	else if (GET_CODE (DECL_RTL (parms)) == MEM
		 && ! rtx_equal_p (DECL_RTL (parms), DECL_INCOMING_RTL (parms)))
	  dbxout_symbol_location (parms, TREE_TYPE (parms),
				  0, DECL_RTL (parms));
      }
}

/* Given a chain of ..._TYPE nodes (as come in a parameter list),
   output definitions of those names, in raw form */

void
dbxout_args (args)
     tree args;
{
  while (args)
    {
      putc (',', asmfile);
      dbxout_type (TREE_VALUE (args), 0, 0);
      CHARS (1);
      args = TREE_CHAIN (args);
    }
}

/* Given a chain of ..._TYPE nodes,
   find those which have typedef names and output those names.
   This is to ensure those types get output.  */

void
dbxout_types (types)
     register tree types;
{
  while (types)
    {
      if (TYPE_NAME (types)
	  && TREE_CODE (TYPE_NAME (types)) == TYPE_DECL
	  && ! TREE_ASM_WRITTEN (TYPE_NAME (types)))
	dbxout_symbol (TYPE_NAME (types), 1);
      types = TREE_CHAIN (types);
    }
}

/* Output everything about a symbol block (a BLOCK node
   that represents a scope level),
   including recursive output of contained blocks.

   BLOCK is the BLOCK node.
   DEPTH is its depth within containing symbol blocks.
   ARGS is usually zero; but for the outermost block of the
   body of a function, it is a chain of PARM_DECLs for the function parameters.
   We output definitions of all the register parms
   as if they were local variables of that block.

   If -g1 was used, we count blocks just the same, but output nothing
   except for the outermost block.

   Actually, BLOCK may be several blocks chained together.
   We handle them all in sequence.  */

static void
dbxout_block (block, depth, args)
     register tree block;
     int depth;
     tree args;
{
  int blocknum = -1;
  int ignored;

#if DBX_BLOCKS_FUNCTION_RELATIVE
  const char *begin_label; 
  if (current_function_func_begin_label != NULL_TREE)
    begin_label = IDENTIFIER_POINTER (current_function_func_begin_label);
  else
    begin_label = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
#endif

  while (block)
    {
      /* Ignore blocks never expanded or otherwise marked as real.  */
      if (TREE_USED (block) && TREE_ASM_WRITTEN (block))
	{
#ifndef DBX_LBRAC_FIRST
	  /* In dbx format, the syms of a block come before the N_LBRAC.
	     If nothing is output, we don't need the N_LBRAC, either. */
	  ignored = 1;
	  if (debug_info_level != DINFO_LEVEL_TERSE || depth == 0)
	    ignored = dbxout_syms (BLOCK_VARS (block));
	  if (args)
	    dbxout_reg_parms (args);
#endif

	  /* Now output an N_LBRAC symbol to represent the beginning of
	     the block.  Use the block's tree-walk order to generate
	     the assembler symbols LBBn and LBEn
	     that final will define around the code in this block.  */
	  if (depth > 0 && !ignored)
	    {
	      char buf[20];
	      blocknum = BLOCK_NUMBER (block);
	      ASM_GENERATE_INTERNAL_LABEL (buf, "LBB", blocknum);

	      if (BLOCK_HANDLER_BLOCK (block))
		{
		  /* A catch block.  Must precede N_LBRAC.  */
		  tree decl = BLOCK_VARS (block);
		  while (decl)
		    {
#ifdef DBX_OUTPUT_CATCH
		      DBX_OUTPUT_CATCH (asmfile, decl, buf);
#else
		      fprintf (asmfile, "%s \"%s:C1\",%d,0,0,", ASM_STABS_OP,
			       IDENTIFIER_POINTER (DECL_NAME (decl)), N_CATCH);
		      assemble_name (asmfile, buf);
		      fprintf (asmfile, "\n");
#endif
		      decl = TREE_CHAIN (decl);
		    }
		}

#ifdef DBX_OUTPUT_LBRAC
	      DBX_OUTPUT_LBRAC (asmfile, buf);
#else
	      fprintf (asmfile, "%s %d,0,0,", ASM_STABN_OP, N_LBRAC);
	      assemble_name (asmfile, buf);
#if DBX_BLOCKS_FUNCTION_RELATIVE
	      fputc ('-', asmfile);
	      assemble_name (asmfile, begin_label);
#endif
	      fprintf (asmfile, "\n");
#endif
	    }

#ifdef DBX_LBRAC_FIRST
	  /* On some weird machines, the syms of a block
	     come after the N_LBRAC.  */
	  if (debug_info_level != DINFO_LEVEL_TERSE || depth == 0)
	    dbxout_syms (BLOCK_VARS (block));
	  if (args)
	    dbxout_reg_parms (args);
#endif

	  /* Output the subblocks.  */
	  dbxout_block (BLOCK_SUBBLOCKS (block), depth + 1, NULL_TREE);

	  /* Refer to the marker for the end of the block.  */
	  if (depth > 0 && !ignored)
	    {
	      char buf[20];
	      ASM_GENERATE_INTERNAL_LABEL (buf, "LBE", blocknum);
#ifdef DBX_OUTPUT_RBRAC
	      DBX_OUTPUT_RBRAC (asmfile, buf);
#else
	      fprintf (asmfile, "%s %d,0,0,", ASM_STABN_OP, N_RBRAC);
	      assemble_name (asmfile, buf);
#if DBX_BLOCKS_FUNCTION_RELATIVE
	      fputc ('-', asmfile);
	      assemble_name (asmfile, begin_label);
#endif
	      fprintf (asmfile, "\n");
#endif
	    }
	}
      block = BLOCK_CHAIN (block);
    }
}

/* Output the information about a function and its arguments and result.
   Usually this follows the function's code,
   but on some systems, it comes before.  */

static void
dbxout_really_begin_function (decl)
     tree decl;
{
  dbxout_symbol (decl, 0);
  dbxout_parms (DECL_ARGUMENTS (decl));
  if (DECL_NAME (DECL_RESULT (decl)) != 0)
    dbxout_symbol (DECL_RESULT (decl), 1);
}

/* Called at beginning of output of function definition.  */

void
dbxout_begin_function (decl)
     tree decl ATTRIBUTE_UNUSED;
{
#ifdef DBX_FUNCTION_FIRST
  dbxout_really_begin_function (decl);
#endif
}

/* Output dbx data for a function definition.
   This includes a definition of the function name itself (a symbol),
   definitions of the parameters (locating them in the parameter list)
   and then output the block that makes up the function's body
   (including all the auto variables of the function).  */

void
dbxout_function (decl)
     tree decl;
{
#ifndef DBX_FUNCTION_FIRST
  dbxout_really_begin_function (decl);
#endif
  dbxout_block (DECL_INITIAL (decl), 0, DECL_ARGUMENTS (decl));
#ifdef DBX_OUTPUT_FUNCTION_END
  DBX_OUTPUT_FUNCTION_END (asmfile, decl);
#endif
#if defined(ASM_OUTPUT_SECTION_NAME)
  if (use_gnu_debug_info_extensions
#if defined(NO_DBX_FUNCTION_END)
      && ! NO_DBX_FUNCTION_END
#endif
      )
    dbxout_function_end ();
#endif
}
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */
