/* Definitions for Intel 386 running SCO Unix System V 3.2 Version 5.
   Copyright (C) 1992, 95-98, 1999 Free Software Foundation, Inc.
   Contributed by Kean Johnston (hug@netcom.com)

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

#include "i386/i386.h"	/* Base i386 target definitions */
#include "i386/att.h"	/* Use AT&T i386 assembler syntax */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386, SCO OpenServer 5 Syntax)");

#undef LPREFIX
#define LPREFIX				".L"

#undef ALIGN_ASM_OP
#define ALIGN_ASM_OP			"\t.align"

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP		"\t.ascii"

#undef ASM_BYTE_OP
#define ASM_BYTE_OP			"\t.byte"

#undef IDENT_ASM_OP
#define IDENT_ASM_OP			"\t.ident"

#undef COMMON_ASM_OP
#define COMMON_ASM_OP			"\t.comm"

#undef SET_ASM_OP
#define SET_ASM_OP			"\t.set"

#undef LOCAL_ASM_OP
#define LOCAL_ASM_OP			"\t.local"

#undef INT_ASM_OP
#define INT_ASM_OP			"\t.long"

#undef ASM_SHORT
#define ASM_SHORT			"\t.value"

#undef ASM_LONG
#define ASM_LONG			"\t.long"

#undef ASM_DOUBLE
#define ASM_DOUBLE			"\t.double"

#undef TYPE_ASM_OP
#define TYPE_ASM_OP			"\t.type"

#undef SIZE_ASM_OP
#define SIZE_ASM_OP			"\t.size"

#undef STRING_ASM_OP
#define STRING_ASM_OP			"\t.string"

#undef SKIP_ASM_OP
#define SKIP_ASM_OP			"\t.zero"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP			"\t.globl"

#undef EH_FRAME_SECTION_ASM_OP
#define EH_FRAME_SECTION_ASM_OP_COFF	"\t.section\t.ehfram, \"x\""
#define EH_FRAME_SECTION_ASM_OP_ELF	"\t.section\t.eh_frame, \"aw\""
#define EH_FRAME_SECTION_ASM_OP	\
  ((TARGET_ELF) ? EH_FRAME_SECTION_ASM_OP_ELF : EH_FRAME_SECTION_ASM_OP_COFF)

/* Avoid problems (long sectino names, forward assembler refs) with DWARF
   exception unwinding when we're generating COFF */
#define DWARF2_UNWIND_INFO	\
  ((TARGET_ELF) ? 1 : 0 )  

#undef CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP_COFF	"\t.section\t.rodata, \"x\""
#define CONST_SECTION_ASM_OP_ELF	"\t.section\t.rodata"
#define CONST_SECTION_ASM_OP	\
  ((TARGET_ELF) ? CONST_SECTION_ASM_OP_ELF : CONST_SECTION_ASM_OP_COFF)

#undef USE_CONST_SECTION
#define USE_CONST_SECTION_ELF		1
#define USE_CONST_SECTION_COFF		0
#define USE_CONST_SECTION	\
 ((TARGET_ELF) ? USE_CONST_SECTION_ELF : USE_CONST_SECTION_COFF)

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP_ELF		"\t.section\t.init"
/* Rename these for COFF becuase crt1.o will try to run them. */
#define INIT_SECTION_ASM_OP_COFF	"\t.section\t.ctor ,\"x\""
#define INIT_SECTION_ASM_OP	\
  ((TARGET_ELF) ? INIT_SECTION_ASM_OP_ELF : INIT_SECTION_ASM_OP_COFF)

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP_ELF	"\t.section\t.ctors,\"aw\""
#define CTORS_SECTION_ASM_OP_COFF	INIT_SECTION_ASM_OP_COFF
#define CTORS_SECTION_ASM_OP	\
 ((TARGET_ELF) ? CTORS_SECTION_ASM_OP_ELF : CTORS_SECTION_ASM_OP_COFF)

#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP_ELF	"\t.section\t.dtors, \"aw\""
#define DTORS_SECTION_ASM_OP_COFF	FINI_SECTION_ASM_OP_COFF
#define DTORS_SECTION_ASM_OP	\
 ((TARGET_ELF) ? DTORS_SECTION_ASM_OP_ELF : DTORS_SECTION_ASM_OP_COFF)

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP_ELF		"\t.section\t.fini"
#define FINI_SECTION_ASM_OP_COFF	"\t.section\t.dtor, \"x\""
#define FINI_SECTION_ASM_OP	\
 ((TARGET_ELF) ? FINI_SECTION_ASM_OP_ELF : FINI_SECTION_ASM_OP_COFF)

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP		"\t.data"

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP		"\t.text"

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP		"\t.data"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT		"@%s"

#undef APPLY_RESULT_SIZE
#define APPLY_RESULT_SIZE						\
(TARGET_ELF) ? size : 116

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

#define SCO_DEFAULT_ASM_COFF(FILE,NAME)					\
do {									\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
  } while (0)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    if (TARGET_ELF) {							\
      fprintf (FILE, "%s\t ", TYPE_ASM_OP);				\
      assemble_name (FILE, NAME);					\
      putc (',', FILE);							\
      fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
      putc ('\n', FILE);						\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
      ASM_OUTPUT_LABEL(FILE, NAME);					\
    } else								\
      SCO_DEFAULT_ASM_COFF(FILE, NAME);					\
} while (0)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (TARGET_ELF) { if (!flag_inhibit_size_directive)			\
      {									\
	fprintf (FILE, "%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",.-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }	}								\
  } while (0)

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    if (TARGET_ELF) {							\
      fprintf (FILE, "%s\t ", TYPE_ASM_OP);				\
      assemble_name (FILE, NAME);					\
      putc (',', FILE);							\
      fprintf (FILE, TYPE_OPERAND_FMT, "object");			\
      putc ('\n', FILE);						\
      size_directive_output = 0;					\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
        {								\
  	size_directive_output = 1;					\
	fprintf (FILE, "%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL)));	\
        }								\
      ASM_OUTPUT_LABEL(FILE, NAME);					\
    } else								\
      SCO_DEFAULT_ASM_COFF(FILE, NAME);					\
  } while (0)

#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE)

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
do {									\
  output_file_directive((FILE),main_input_filename);			\
  fprintf ((FILE), "\t.version\t\"01.01\"\n");				\
} while (0)

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)						\
do {									\
     if (!flag_no_ident)						\
	fprintf ((FILE), "%s\t\"GCC: (GNU) %s\"\n",			\
		 IDENT_ASM_OP, version_string);				\
} while (0)

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
  if (TARGET_ELF) {							\
     char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);			 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "%s\t ", SIZE_ASM_OP);			 	 \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
       }								 \
    }									 \
} while (0)

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  if (TARGET_ELF)							\
    sprintf (LABEL, "*.%s%d", (PREFIX), (NUM));				\
  else									\
    sprintf (LABEL, ".%s%d", (PREFIX), (NUM));				\
} while (0)

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
do {									\
  if (TARGET_ELF)							\
    fprintf (FILE, "%s _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", ASM_LONG, LPREFIX, VALUE); \
  else									\
    fprintf (FILE, "\t.word %s%d-%s%d\n", LPREFIX,VALUE,LPREFIX,REL);	\
} while (0)

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "%s\t", COMMON_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  if (TARGET_ELF)							\
    fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
  else									\
    fprintf ((FILE), ",%u\n", (SIZE));					\
} while (0)

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if (TARGET_ELF) {							\
    fprintf ((FILE), "%s\t", LOCAL_ASM_OP);				\
    assemble_name ((FILE), (NAME));					\
    fprintf ((FILE), "\n");						\
    ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);		\
  } else {								\
    int align = exact_log2 (ALIGN);					\
    if (align > 2) align = 2;						\
    if (TARGET_SVR3_SHLIB)						\
      data_section ();							\
    else								\
      bss_section ();							\
    ASM_OUTPUT_ALIGN ((FILE), align == -1 ? 2 : align);			\
    fprintf ((FILE), "%s\t", "\t.lcomm");				\
    assemble_name ((FILE), (NAME));					\
    fprintf ((FILE), ",%u\n", (SIZE));					\
   }									\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

#undef ESCAPES
#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"

#undef STRING_LIMIT
#define STRING_LIMIT	((unsigned) 256)

#undef ASM_OUTPUT_LIMITED_STRING
#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)				\
  do									\
    {									\
      register unsigned char *_limited_str = (unsigned char *) (STR);	\
      register unsigned ch;						\
      fprintf ((FILE), "%s\t\"", STRING_ASM_OP);			\
      for (; (ch = *_limited_str); _limited_str++)			\
        {								\
	  register int escape;						\
	  switch (escape = ESCAPES[ch])					\
	    {								\
	    case 0:							\
	      putc (ch, (FILE));					\
	      break;							\
	    case 1:							\
	      fprintf ((FILE), "\\%03o", ch);				\
	      break;							\
	    default:							\
	      putc ('\\', (FILE));					\
	      putc (escape, (FILE));					\
	      break;							\
	    }								\
        }								\
      fprintf ((FILE), "\"\n");						\
    }									\
  while (0)


#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
do {									\
      register unsigned char *_ascii_bytes = (unsigned char *) (STR);	\
      register unsigned char *limit = _ascii_bytes + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;				\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register unsigned char *p;					\
	  if (bytes_in_chunk >= 64)					\
	    {								\
	      fputc ('\n', (FILE));					\
	      bytes_in_chunk = 0;					\
	    }								\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
	  if (p < limit && (p - _ascii_bytes) <= STRING_LIMIT)		\
	    {								\
	      if (bytes_in_chunk > 0)					\
		{							\
		  fputc ('\n', (FILE));					\
		  bytes_in_chunk = 0;					\
		}							\
	      ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);		\
	      _ascii_bytes = p;						\
	    }								\
	  else								\
	    {								\
	      if (bytes_in_chunk == 0)					\
		fprintf ((FILE), "%s\t", ASM_BYTE_OP);			\
	      else							\
		fputc (',', (FILE));					\
	      fprintf ((FILE), "0x%02x", *_ascii_bytes);		\
	      bytes_in_chunk += 5;					\
	    }								\
	}								\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\n");						\
} while (0) 

/* Must use data section for relocatable constants when pic.  */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX)					\
{									\
  if (TARGET_ELF) {							\
    if (flag_pic && symbolic_operand (RTX, VOIDmode))			\
      data_section ();							\
    else								\
      const_section ();							\
  } else								\
    readonly_data_section();						\
}

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,JUMPTABLE)		\
do {									\
  if (TARGET_ELF)							\
    ASM_OUTPUT_ALIGN ((FILE), 2);					\
  ASM_OUTPUT_INTERNAL_LABEL((FILE),(PREFIX),(NUM));			\
} while (0)


#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
do {									\
  if (TARGET_ELF) {							\
     ctors_section ();							\
     fprintf (FILE, "%s\t ", INT_ASM_OP);				\
     assemble_name (FILE, NAME);					\
     fprintf (FILE, "\n");						\
  } else {								\
    init_section ();							\
    fprintf (FILE, "\tpushl $");					\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n"); }						\
  } while (0)

#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)				\
do {									\
  if (TARGET_ELF) {							\
    dtors_section ();                   				\
    fprintf (FILE, "%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } else {								\
    fini_section ();                   					\
    fprintf (FILE, "%s\t ", ASM_LONG);					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n"); }						\
  } while (0)


#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\t\"%s\"\n", IDENT_ASM_OP, NAME);

#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  (fprintf ((FILE), "%s ", GLOBAL_ASM_OP), assemble_name (FILE, NAME), fputs ("\n", FILE))

#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)				\
  if (TARGET_ELF) ASM_GLOBALIZE_LABEL (FILE, XSTR (FUN, 0))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* 
 * Compensate for the difference between ELF and COFF assembler syntax.
 * Otherwise, this is cribbed from ../svr4.h.
 * We rename 'gcc_except_table' to the shorter name in preparation
 * for the day when we're ready to do DWARF2 eh unwinding under COFF 
 */
#undef ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
do {									\
  static struct section_info                                            \
    {                                                                   \
      struct section_info *next;                                        \
      char *name;                                                       \
      enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;                \
    } *sections;                                                        \
  struct section_info *s;                                               \
  char *mode;                                                           \
  enum sect_enum type;                                                  \
  char *sname = NAME ;							\
  if (strcmp(NAME, ".gcc_except_table") == 0) sname = ".gccexc" ;	\
                                                                        \
  for (s = sections; s; s = s->next)                                    \
    if (!strcmp (NAME, s->name))                                        \
      break;                                                            \
                                                                        \
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)                        \
    type = SECT_EXEC, mode = (TARGET_ELF) ? "ax" : "x" ;                \
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))                 \
    type = SECT_RO, mode = "a";                                         \
  else                                                                  \
    type = SECT_RW, mode = (TARGET_ELF) ? "aw" : "w" ;                  \
                                                                        \
  if (s == 0)                                                           \
    {                                                                   \
      s = (struct section_info *) xmalloc (sizeof (struct section_info));  \
      s->name = xmalloc ((strlen (NAME) + 1) * sizeof (*NAME));         \
      strcpy (s->name, NAME);                                           \
      s->type = type;                                                   \
      s->next = sections;                                               \
      sections = s;                                                     \
      fprintf (FILE, ".section\t%s,\"%s\"%s\n", sname, mode,		\
		(TARGET_ELF) ? ",@progbits" : "" );    			\
    }                                                                   \
  else                                                                  \
    {                                                                   \
      if (DECL && s->type != type)                                      \
        error_with_decl (DECL, "%s causes a section type conflict");    \
                                                                        \
      fprintf (FILE, ".section\t%s\n", sname);                          \
    }                                                                   \
} while (0)

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE) \
do {									\
  if (TARGET_ELF)							\
    fprintf (FILE, "%s\t%u\n", SKIP_ASM_OP, (SIZE));			\
  else									\
    fprintf ((FILE), "%s\t.,.+%u\n", SET_ASM_OP, (SIZE));		\
} while (0)


#undef CTOR_LIST_BEGIN
#define CTOR_LIST_BEGIN							\
do {									\
  asm (CTORS_SECTION_ASM_OP);						\
  if (TARGET_ELF)							\
    STATIC func_ptr __CTOR_LIST__[1] = { (func_ptr) (-1) };		\
  else									\
    asm ("pushl $0");							\
} while (0)

#undef CTOR_LIST_END
#define CTOR_LIST_END							\
do {									\
  if (TARGET_ELF) {							\
    asm (CTORS_SECTION_ASM_OP);						\
    STATIC func_ptr __CTOR_LIST__[1] = { (func_ptr) (0) };		\
  } else {								\
    CTOR_LIST_BEGIN;							\
  }									\
} while (0)

#undef DBX_BLOCKS_FUNCTION_RELATIVE
#define DBX_BLOCKS_FUNCTION_RELATIVE 1

#undef DBX_FUNCTION_FIRST
#define DBX_FUNCTION_FIRST 1

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  ((TARGET_ELF) ? svr4_dbx_register_map[n] : dbx_register_map[n])

#undef DWARF_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

#define DWARF_DEBUGGING_INFO 1
#define SDB_DEBUGGING_INFO   1
#define DBX_DEBUGGING_INFO   1
#define PREFERRED_DEBUGGING_TYPE					\
  ((TARGET_ELF) ? DWARF_DEBUG: SDB_DEBUG)

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_init, in_fini, in_ctors, in_dtors

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  INIT_SECTION_FUNCTION							\
  FINI_SECTION_FUNCTION							\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#undef CONST_SECTION_FUNCTION
#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  if (!USE_CONST_SECTION)						\
    text_section();							\
  else if (in_section != in_const)					\
    {									\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);		\
      in_section = in_const;						\
    }									\
}

#undef FINI_SECTION_FUNCTION
#define FINI_SECTION_FUNCTION						\
void									\
fini_section ()								\
{									\
  if ((!TARGET_ELF) && in_section != in_fini)				\
    {									\
      fprintf (asm_out_file, "%s\n", FINI_SECTION_ASM_OP);		\
      in_section = in_fini;						\
    }									\
}

#undef INIT_SECTION_FUNCTION
#define INIT_SECTION_FUNCTION						\
void									\
init_section ()								\
{									\
  if ((!TARGET_ELF) && in_section != in_init)				\
    {									\
      fprintf (asm_out_file, "%s\n", INIT_SECTION_ASM_OP);		\
      in_section = in_init;						\
    }									\
}

#undef CTORS_SECTION_FUNCTION
#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#undef DTORS_SECTION_FUNCTION
#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#undef FRAME_POINTER_REQUIRED
#define FRAME_POINTER_REQUIRED						\
  ((TARGET_ELF) ? 0 : 							\
   (current_function_calls_setjmp || current_function_calls_longjmp))

#undef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION (TARGET_ELF && flag_pic)

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX						\
 ((TARGET_ELF) ? "" : ".")

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin/"
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/"

#undef NON_SAVING_SETJMP
#define NON_SAVING_SETJMP						\
  ((TARGET_ELF) ? 0 : 							\
   (current_function_calls_setjmp && current_function_calls_longjmp))

#undef NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C 1

/* JKJ FIXME - examine the ramifications of RETURN_IN_MEMORY and
   RETURN_POPS_ARGS */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 				\
 ((TARGET_ELF) ?							\
  (ix86_return_pops_args (FUNDECL, FUNTYPE, SIZE)) : 			\
  (((FUNDECL) && (TREE_CODE (FUNDECL) == IDENTIFIER_NODE)) ? 0		\
   : (TARGET_RTD							\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0					\
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))		\
	      == void_type_node))) ? (SIZE)				\
   : 0))

#undef SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (TARGET_ELF && flag_pic && RELOC)					\
     data_section ();							\
  else if (TREE_CODE (DECL) == STRING_CST)				\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if (! DECL_READONLY_SECTION (DECL, RELOC)) 			\
	data_section ();						\
      else								\
	const_section ();						\
    }									\
  else									\
    const_section ();							\
}

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) 						\
  (DEFAULT_SWITCH_TAKES_ARG(CHAR)					\
   || (CHAR) == 'h' 							\
   || (CHAR) == 'R' 							\
   || (CHAR) == 'Y' 							\
   || (CHAR) == 'z')

#undef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)					\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)					\
  && strcmp (STR, "Tdata") && strcmp (STR, "Ttext")			\
  && strcmp (STR, "Tbss"))

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS)

#undef HANDLE_SYSV_PRAGMA
#define HANDLE_SYSV_PRAGMA 1

/* Though OpenServer supports .weak in COFF, we don't use it.
 * G++ will frequently emit a symol as .weak and then (in the same .s 
 * file) declare it global.   The COFF assembler finds this unamusing.
 */
#define SUPPORTS_WEAK (TARGET_ELF)
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME);		\
	fputc ('\n', FILE); } while (0)

#undef SCCS_DIRECTIVE
#define SCCS_DIRECTIVE 1

/*
 * Define sizes and types
 */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 	96
#define SIZE_TYPE		"unsigned int"
#define PTRDIFF_TYPE		"int"
#define WCHAR_TYPE		"long int"
#define WCHAR_TYPE_SIZE		BITS_PER_WORD

/*
 * New for multilib support. Set the default switches for multilib,
 * which is -melf.
 */
#define MULTILIB_DEFAULTS { "melf" }


/* Please note that these specs may look messy but they are required in
   order to emulate the SCO Development system as closely as possible.
   With SCO Open Server 5.0, you now get the linker and assembler free,
   so that is what these specs are targeted for. These utilities are
   very argument sensitive: a space in the wrong place breaks everything.
   So RMS, please forgive this mess. It works.

   Parameters which can be passed to gcc, and their SCO equivalents:
   GCC Parameter                SCO Equivalent
   -ansi                        -a ansi
   -posix                       -a posix
   -Xpg4                        -a xpg4
   -Xpg4plus                    -a xpg4plus
   -Xods30                      -a ods30

   As with SCO, the default is XPG4 plus mode. SCO also allows you to
   specify a C dialect with -Xt, -Xa, -Xc, -Xk and -Xm. These are passed
   on to the assembler and linker in the same way that the SCO compiler
   does.

   SCO also allows you to compile, link and generate either ELF or COFF
   binaries. With gcc, unlike the SCO compiler, the default is ELF.
   Specify -mcoff to gcc to produce COFF binaries. -fpic will get the
   assembler and linker to produce PIC code.
*/

/* Set up assembler flags for PIC and ELF compilations */
#undef ASM_SPEC

#if USE_GAS
  /* Leave ASM_SPEC undefined so we pick up the master copy from gcc.c 
   * Undef MD_EXEC_PREFIX becuase we don't know where GAS is, but it's not
   * likely in /usr/ccs/bin/ 
   */
#undef MD_EXEC_PREFIX 
#else

#define ASM_SPEC \
   "-b %{!mcoff:elf}%{mcoff:coff \
     %{static:%e-static not valid with -mcoff} \
     %{shared:%e-shared not valid with -mcoff} \
     %{symbolic:%e-symbolic not valid with -mcoff}} \
    %{Ym,*} %{Yd,*} %{Wa,*:%*} \
    %{!mcoff:-E%{Xa:a}%{!Xa:%{Xc:c}%{!Xc:%{Xk:k}%{!Xk:%{Xt:t}%{!Xt:a}}}},%{ansi:ansi}%{!ansi:%{posix:posix}%{!posix:%{Xpg4:xpg4}%{!Xpg4:%{Xpg4plus:XPG4PLUS}%{!Xpg4plus:%{Xods30:ods30}%{!Xods30:XPG4PLUS}}}}},ELF %{Qn:} %{!Qy:-Qn}}"
#endif

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{shared: %{!mcoff: crti.o%s}} \
  %{!shared:\
   %{!symbolic: \
    %{pg:gcrt.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}}} \
  %{ansi:values-Xc.o%s} \
  %{!ansi: \
   %{traditional:values-Xt.o%s} \
    %{!traditional: \
     %{Xa:values-Xa.o%s} \
      %{!Xa:%{Xc:values-Xc.o%s} \
       %{!Xc:%{Xk:values-Xk.o%s} \
        %{!Xk:%{Xt:values-Xt.o%s} \
         %{!Xt:values-Xa.o%s}}}}}} \
  %{mcoff:crtbeginS.o%s} %{!mcoff:crtbegin.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
 "%{!mcoff:crtend.o%s} \
  %{mcoff:crtendS.o%s} \
  %{pg:gcrtn.o%s}%{!pg:crtn.o%s}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Asystem(svr3)"

/* You are in a maze of GCC specs ... all alike */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) \
  %{fpic:%{mcoff:%e-fpic is not valid with -mcoff}} \
  %{fPIC:%{mcoff:%e-fPIC is not valid with -mcoff}} \
  -D__i386 -D__unix -D_SCO_DS=1 -D_M_I386 -D_M_XENIX -D_M_UNIX \
  %{!Xods30:-D_STRICT_NAMES} \
  %{!ansi:%{!posix:%{!Xods30:-D_SCO_XPG_VERS=4}}} \
  %{ansi:-isystem include/ansi%s -isystem /usr/include/ansi -D_STRICT_ANSI} \
  %{!ansi: \
   %{posix:-isystem include/posix%s -isystem /usr/include/posix \
           -D_POSIX_C_SOURCE=2 -D_POSIX_SOURCE=1} \
    %{!posix:%{Xpg4:-isystem include/xpg4%s -isystem /usr/include/xpg4 \
                    -D_XOPEN_SOURCE=1} \
     %{!Xpg4:-D_M_I86 -D_M_I86SM -D_M_INTERNAT -D_M_SDATA -D_M_STEXT \
             -D_M_BITFIELDS -D_M_SYS5 -D_M_SYSV -D_M_SYSIII \
             -D_M_WORDSWAP -Dunix -DM_I386 -DM_UNIX -DM_XENIX \
             %{Xods30:-isystem include/ods_30_compat%s \
                      -isystem /usr/include/ods_30_compat \
                      -D_SCO_ODS_30 -DM_I86 -DM_I86SM -DM_SDATA -DM_STEXT \
                      -DM_BITFIELDS -DM_SYS5 -DM_SYSV -DM_INTERNAT -DM_SYSIII \
                      -DM_WORDSWAP}}}} \
  %{scointl:-DM_INTERNAT -D_M_INTERNAT} \
  %{traditional:-D_KR -D_SVID -D_NO_PROTOTYPE} \
  %{!mcoff:-D_SCO_ELF} \
  %{mcoff:-D_M_COFF -D_SCO_COFF} \
  %{!mcoff:%{fpic:-D__PIC__ -D__pic__} \
         %{fPIC:%{!fpic:-D__PIC__ -D__pic__}}} \
  %{Xa:-D_SCO_C_DIALECT=1} \
  %{!Xa:%{Xc:-D_SCO_C_DIALECT=3} \
   %{!Xc:%{Xk:-D_SCO_C_DIALECT=4} \
    %{!Xk:%{Xt:-D_SCO_C_DIALECT=2} \
     %{!Xt:-D_SCO_C_DIALECT=1}}}} \
  %{traditional:-traditional -D_KR -D_NO_PROTOTYPE}"

#undef LINK_SPEC
#define LINK_SPEC \
 "-b %{!mcoff:elf}%{mcoff:coff \
   %{static:%e-static not valid with -mcoff} \
   %{shared:%e-shared not valid with -mcoff} \
   %{symbolic:%e-symbolic not valid with -mcoff} \
   %{fpic:%e-fpic not valid with -mcoff} \
   %{fPIC:%e-fPIC not valid with -mcoff}} \
  -R%{Xa:a}%{!Xa:%{Xc:c}%{!Xc:%{Xk:k}%{!Xk:%{Xt:t}%{!Xt:a}}}},%{ansi:ansi}%{!ansi:%{posix:posix}%{!posix:%{Xpg4:xpg4}%{!Xpg4:%{Xpg4plus:XPG4PLUS}%{!Xpg4plus:%{Xods30:ods30}%{!Xods30:XPG4PLUS}}}}},%{mcoff:COFF}%{!mcoff:ELF} \
  %{Wl,*%*} %{YP,*} %{YL,*} %{YU,*} \
  %{!YP,*:%{p:-YP,/usr/ccs/libp:/lib/libp:/usr/lib/libp:/usr/ccs/lib:/lib:/usr/lib} \
   %{!p:-YP,/usr/ccs/lib:/lib:/usr/lib}} \
  %{h*} %{static:-dn -Bstatic} %{shared:-G -dy %{!z*:-z text}} \
  %{symbolic:-Bsymbolic -G -dy %{!z*:-z text}} %{z*} %{R*} %{Y*} \
  %{G:-G} %{!mcoff:%{Qn:} %{!Qy:-Qn}}"

/* The SCO COFF linker gets confused on the difference between "-ofoo"
   and "-o foo".   So we just always force a single space. */

#define SWITCHES_NEED_SPACES "o"

/* Library spec. If we are not building a shared library, provide the
   standard libraries, as per the SCO compiler.  */

#undef LIB_SPEC
#define LIB_SPEC \
 "%{shared:pic/libgcc.a%s}%{!shared:%{!symbolic:-lcrypt -lgen -lc}}"

#undef LIBGCC_SPEC
#define LIBGCC_SPEC \
 "%{!shared:-lgcc}"

#define MASK_COFF     		010000000000	/* Mask for elf generation */
#define TARGET_COFF             (target_flags & MASK_COFF)
#define TARGET_ELF              (!(target_flags & MASK_COFF))

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES 		\
	{ "coff", MASK_COFF, "Generate COFF output" }, 		\
	{ "elf", -MASK_COFF, "Generate ELF output"  },

#define NO_DOLLAR_IN_LABEL

/* Implicit library calls should use memcpy, not bcopy, etc.  They are 
   faster on OpenServer libraries. */

#define TARGET_MEM_FUNCTIONS

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */

#define MAX_OFILE_ALIGNMENT (32768*8)

/* Define the `__builtin_va_list' type for the ABI.  On OpenServer, this
   type is `char *'.  */
#undef BUILD_VA_LIST_TYPE
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = build_pointer_type (char_type_node)


/*
Here comes some major hackery to get the crt stuff to compile properly.
Since we can (and do) compile for both COFF and ELF environments, we
set things up accordingly, based on the pre-processor defines for ELF
and COFF. This is insane, but then I guess having one compiler with a
single back-end supporting two vastly different file format types is
a little insane too. But it is not impossible and we get a useful
compiler at the end of the day. Onward we go ...
*/

#if defined(CRT_BEGIN) || defined(CRT_END) || defined(IN_LIBGCC2)
# undef OBJECT_FORMAT_ELF
# undef INIT_SECTION_ASM_OP
# undef FINI_SECTION_ASM_OP
# undef CTORS_SECTION_ASM_OP
# undef DTORS_SECTION_ASM_OP
# undef EH_FRAME_SECTION_ASM_OP
# undef CTOR_LIST_BEGIN
# undef CTOR_LIST_END
# undef DO_GLOBAL_CTORS_BODY

# if defined (_SCO_ELF)
#  define OBJECT_FORMAT_ELF
#  define INIT_SECTION_ASM_OP INIT_SECTION_ASM_OP_ELF
#  define FINI_SECTION_ASM_OP FINI_SECTION_ASM_OP_ELF
#  define DTORS_SECTION_ASM_OP DTORS_SECTION_ASM_OP_ELF
#  define CTORS_SECTION_ASM_OP CTORS_SECTION_ASM_OP_ELF
#  define EH_FRAME_SECTION_ASM_OP EH_FRAME_SECTION_ASM_OP_ELF
# else /* ! _SCO_ELF */
#  define INIT_SECTION_ASM_OP INIT_SECTION_ASM_OP_COFF
#  define FINI_SECTION_ASM_OP FINI_SECTION_ASM_OP_COFF
#  define DTORS_SECTION_ASM_OP DTORS_SECTION_ASM_OP_COFF
#  define CTORS_SECTION_ASM_OP CTORS_SECTION_ASM_OP_COFF
#  define EH_FRAME_SECTION_ASM_OP EH_FRAME_SECTION_ASM_OP_COFF
#  define CTOR_LIST_BEGIN asm (INIT_SECTION_ASM_OP); asm ("pushl $0")
#  define CTOR_LIST_END CTOR_LIST_BEGIN
#  define DO_GLOBAL_CTORS_BODY						\
do {									\
     func_ptr *p, *beg = alloca(0);					\
     for (p = beg; *p;)							\
      (*p++) ();							\
} while (0)
# endif /* ! _SCO_ELF */
#endif /* CRT_BEGIN !! CRT_END */
