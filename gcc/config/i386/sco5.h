/* Definitions for Intel 386 running SCO Unix System V 3.2 Version 5.
   Copyright (C) 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2002
   Free Software Foundation, Inc.
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

#define TARGET_VERSION fprintf (stderr, " (i386, SCO OpenServer 5 Syntax)");

#undef LPREFIX
#define LPREFIX				".L"

#undef ALIGN_ASM_OP
#define ALIGN_ASM_OP			"\t.align\t"

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP		"\t.ascii\t"

#undef IDENT_ASM_OP
#define IDENT_ASM_OP			"\t.ident\t"

#undef COMMON_ASM_OP
#define COMMON_ASM_OP			"\t.comm\t"

#undef SET_ASM_OP
#define SET_ASM_OP			"\t.set\t"

#undef LOCAL_ASM_OP
#define LOCAL_ASM_OP			"\t.local\t"

#undef ASM_SHORT
#define ASM_SHORT			"\t.value\t"

#undef ASM_LONG
#define ASM_LONG			"\t.long\t"

#undef ASM_QUAD

#undef TYPE_ASM_OP
#define TYPE_ASM_OP			"\t.type\t"

#undef SIZE_ASM_OP
#define SIZE_ASM_OP			"\t.size\t"

#undef STRING_ASM_OP
#define STRING_ASM_OP			"\t.string\t"

#undef SKIP_ASM_OP
#define SKIP_ASM_OP			"\t.zero\t"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP			"\t.globl\t"

#undef EH_FRAME_SECTION_ASM_OP
#define EH_FRAME_SECTION_NAME_COFF	".ehfram"
#define EH_FRAME_SECTION_NAME_ELF	".eh_frame"
#define EH_FRAME_SECTION_NAME	\
  ((TARGET_ELF) ? EH_FRAME_SECTION_NAME_ELF : EH_FRAME_SECTION_NAME_COFF)

/* Avoid problems (long sectino names, forward assembler refs) with DWARF
   exception unwinding when we're generating COFF */
#define DWARF2_UNWIND_INFO	\
  ((TARGET_ELF) ? 1 : 0 )  

#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP_COFF	"\t.section\t.rodata, \"x\""
#define READONLY_DATA_SECTION_ASM_OP_ELF	"\t.section\t.rodata"
#define READONLY_DATA_SECTION_ASM_OP		\
  ((TARGET_ELF)					\
   ? READONLY_DATA_SECTION_ASM_OP_ELF		\
   : READONLY_DATA_SECTION_ASM_OP_COFF)

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP_ELF		"\t.section\t.init"
/* Rename these for COFF because crt1.o will try to run them.  */
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
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");		\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
    } else								\
      SCO_DEFAULT_ASM_COFF(FILE, NAME);					\
} while (0)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (TARGET_ELF && !flag_inhibit_size_directive)			\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);				\
  } while (0)

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    if (TARGET_ELF) {							\
      HOST_WIDE_INT size;						\
									\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
									\
      size_directive_output = 0;					\
      if (!flag_inhibit_size_directive					\
	  && (DECL) && DECL_SIZE (DECL))				\
	{								\
	  size_directive_output = 1;					\
	  size = int_size_in_bytes (TREE_TYPE (DECL));			\
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);			\
	}								\
									\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
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

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
  if (TARGET_ELF) {							 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     HOST_WIDE_INT size;						 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 size = int_size_in_bytes (TREE_TYPE (DECL));			 \
	 ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			 \
       }								 \
    }									 \
} while (0)

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  if (TARGET_ELF)							\
    sprintf (LABEL, "*.%s%ld", (PREFIX), (long)(NUM));			\
  else									\
    sprintf (LABEL, ".%s%ld", (PREFIX), (long)(NUM));			\
} while (0)

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "%s", COMMON_ASM_OP);				\
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
    fprintf ((FILE), "%s", LOCAL_ASM_OP);				\
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
      register const unsigned char *_limited_str =			\
        (const unsigned char *) (STR);					\
      register unsigned ch;						\
      fprintf ((FILE), "%s\"", STRING_ASM_OP);				\
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
      register const unsigned char *_ascii_bytes =			\
        (const unsigned char *) (STR);					\
      register const unsigned char *limit = _ascii_bytes + (LENGTH);	\
      register unsigned bytes_in_chunk = 0;				\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register unsigned const char *p;				\
	  if (bytes_in_chunk >= 64)					\
	    {								\
	      fputc ('\n', (FILE));					\
	      bytes_in_chunk = 0;					\
	    }								\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
	  if (p < limit && (p - _ascii_bytes) <= (long) STRING_LIMIT)	\
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
		fputs ("\t.byte\t", (FILE));				\
	      else							\
		fputc (',', (FILE));					\
	      fprintf ((FILE), "0x%02x", *_ascii_bytes);		\
	      bytes_in_chunk += 5;					\
	    }								\
	}								\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\n");						\
} while (0) 

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,JUMPTABLE)		\
do {									\
  if (TARGET_ELF)							\
    ASM_OUTPUT_ALIGN ((FILE), 2);					\
  ASM_OUTPUT_INTERNAL_LABEL((FILE),(PREFIX),(NUM));			\
} while (0)

#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\"%s\"\n", IDENT_ASM_OP, NAME);

#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)				\
  if (TARGET_ELF) (*targetm.asm_out.globalize_label) (FILE, XSTR (FUN, 0))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* The prefix to add to user-visible assembler symbols.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* 
 * We rename 'gcc_except_table' to the shorter name in preparation
 * for the day when we're ready to do DWARF2 eh unwinding under COFF.
 */
/* #define EXCEPTION_SECTION()		named_section (NULL, ".gccexc", 1) */

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section 

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE) \
do {									\
  if (TARGET_ELF)							\
    fprintf (FILE, "%s%u\n", SKIP_ASM_OP, (SIZE));			\
  else									\
    fprintf ((FILE), "%s.,.+%u\n", SET_ASM_OP, (SIZE));		\
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

#define DWARF2_DEBUGGING_INFO 1
#define DWARF_DEBUGGING_INFO 1
#define SDB_DEBUGGING_INFO 1
#define DBX_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE					\
  ((TARGET_ELF) ? DWARF2_DEBUG: SDB_DEBUG)

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_init, in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  INIT_SECTION_FUNCTION							\
  FINI_SECTION_FUNCTION

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

#undef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED				\
  ((TARGET_ELF) ? 0 : 							\
   (current_function_calls_setjmp || current_function_calls_longjmp))

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

/* ??? Ignore coff.  */
#undef	TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION  default_elf_select_section

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

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS)

#define HANDLE_SYSV_PRAGMA 1

/* Though OpenServer supports .weak in COFF, we don't use it.
 * G++ will frequently emit a symol as .weak and then (in the same .s 
 * file) declare it global.   The COFF assembler finds this unamusing.
 */
#define SUPPORTS_WEAK (TARGET_ELF)
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME);		\
	fputc ('\n', FILE); } while (0)

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
   * Undef MD_EXEC_PREFIX because we don't know where GAS is, but it's not
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
   %{Xa:values-Xa.o%s} \
    %{!Xa:%{Xc:values-Xc.o%s} \
     %{!Xc:%{Xk:values-Xk.o%s} \
      %{!Xk:%{Xt:values-Xt.o%s} \
       %{!Xt:values-Xa.o%s}}}}} \
  %{mcoff:crtbeginS.o%s} %{!mcoff:crtbegin.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
 "%{!mcoff:crtend.o%s} \
  %{mcoff:crtendS.o%s} \
  %{pg:gcrtn.o%s}%{!pg:crtn.o%s}"

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__unix");		\
	builtin_define ("_SCO_DS");		\
	builtin_define ("_M_I386");		\
	builtin_define ("_M_XENIX");		\
	builtin_define ("_M_UNIX");		\
	builtin_assert ("system=svr3");		\
	if (flag_iso)				\
	  cpp_define (pfile, "_STRICT_ANSI");	\
	if (flag_pic)							\
	  {								\
	    builtin_define ("__PIC__");					\
	    builtin_define ("__pic__");					\
	  }								\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "\
  %{fpic:%{mcoff:%e-fpic is not valid with -mcoff}} \
  %{fPIC:%{mcoff:%e-fPIC is not valid with -mcoff}} \
  %{!Xods30:-D_STRICT_NAMES} \
  %{!ansi:%{!posix:%{!Xods30:-D_SCO_XPG_VERS=4}}} \
  %{ansi:-isystem include/ansi%s -isystem /usr/include/ansi} \
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
  %{!mcoff:-D_SCO_ELF} \
  %{mcoff:-D_M_COFF -D_SCO_COFF} \
  %{Xa:-D_SCO_C_DIALECT=1} \
  %{!Xa:%{Xc:-D_SCO_C_DIALECT=3} \
   %{!Xc:%{Xk:-D_SCO_C_DIALECT=4} \
    %{!Xk:%{Xt:-D_SCO_C_DIALECT=2} \
     %{!Xt:-D_SCO_C_DIALECT=1}}}}"

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
   and "-o foo".   So we just always force a single space.  */

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
#define TARGET_ELF              (1) /* (!(target_flags & MASK_COFF)) */

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES 					\
	{ "elf", -MASK_COFF, N_("Generate ELF output")  },

#define NO_DOLLAR_IN_LABEL

/* Implicit library calls should use memcpy, not bcopy, etc.  They are 
   faster on OpenServer libraries.  */

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
# undef EH_FRAME_SECTION_NAME
# undef CTOR_LIST_BEGIN
# undef CTOR_LIST_END
# undef DO_GLOBAL_CTORS_BODY

# if defined (_SCO_ELF)
#  define OBJECT_FORMAT_ELF
#  define INIT_SECTION_ASM_OP INIT_SECTION_ASM_OP_ELF
#  define FINI_SECTION_ASM_OP FINI_SECTION_ASM_OP_ELF
#  define DTORS_SECTION_ASM_OP DTORS_SECTION_ASM_OP_ELF
#  define CTORS_SECTION_ASM_OP CTORS_SECTION_ASM_OP_ELF
#  define EH_FRAME_SECTION_NAME EH_FRAME_SECTION_NAME_ELF
# else /* ! _SCO_ELF */
#  define INIT_SECTION_ASM_OP INIT_SECTION_ASM_OP_COFF
#  define FINI_SECTION_ASM_OP FINI_SECTION_ASM_OP_COFF
#  define DTORS_SECTION_ASM_OP DTORS_SECTION_ASM_OP_COFF
#  define CTORS_SECTION_ASM_OP CTORS_SECTION_ASM_OP_COFF
#  define EH_FRAME_SECTION_NAME EH_FRAME_SECTION_NAME_COFF
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

/* Handle special EH pointer encodings.  Absolute, pc-relative, and
   indirect are handled automatically.  */
#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE) \
  do {									\
    if ((SIZE) == 4 && ((ENCODING) & 0x70) == DW_EH_PE_datarel)		\
      {									\
        fputs (ASM_LONG, FILE);						\
        assemble_name (FILE, XSTR (ADDR, 0));				\
	fputs (((ENCODING) & DW_EH_PE_indirect ? "@GOT" : "@GOTOFF"), FILE); \
        goto DONE;							\
      }									\
  } while (0)

/* Used by crtstuff.c to initialize the base of data-relative relocations.
   These are GOT relative on x86, so return the pic register.  */
#ifdef __PIC__
#define CRT_GET_RFIB_DATA(BASE)			\
  {						\
    register void *ebx_ __asm__("ebx");		\
    BASE = ebx_;				\
  }
#else
#define CRT_GET_RFIB_DATA(BASE)						\
  __asm__ ("call\t.LPR%=\n"						\
	   ".LPR%=:\n\t"						\
	   "popl\t%0\n\t"						\
	   /* Due to a GAS bug, this cannot use EAX.  That encodes	\
	      smaller than the traditional EBX, which results in the	\
	      offset being off by one.  */				\
	   "addl\t$_GLOBAL_OFFSET_TABLE_+[.-.LPR%=],%0"			\
	   : "=d"(BASE))
#endif

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic ? (GLOBAL ? DW_EH_PE_indirect : 0) | DW_EH_PE_datarel	\
   : DW_EH_PE_absptr)
